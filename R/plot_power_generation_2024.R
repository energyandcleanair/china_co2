power_generation_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(days_in_month(.)),
                                   lang=parent.frame()$lang,
                                   output_dir=get('output_dir', envir=.GlobalEnv),
                                   make_additional_plots=F) { #make a number of plots in addition to the main ones
  pwr_data <- read_power_generation()

  try(ember <- read_csv("https://ember-climate.org/app/uploads/2022/07/monthly_full_release_long_format-4.csv"))
  if(!exists("ember")) ember <- read_csv(get_data_file("monthly_full_release_long_format-4.csv"))

  ember %>% filter(Area=='China', Unit=='TWh',
                   Variable %in% c('Wind', 'Solar', 'Coal', 'Gas', 'Hydro', 'Nuclear',
                                   'Biomass')) %>%
    mutate(Value=Value*10,
           Variable=ifelse(Variable=='Bioenergy', 'Biomass', Variable),
           source=case_when(Variable %in% c('Coal', 'Gas', 'Biomass')~'Thermal',
                            Variable=='Total Generation'~'Total',
                            T~Variable),
           subtype=ifelse(Variable %in% c('Coal', 'Gas', 'Biomass'), Variable, NA),
           Date=Date %>% 'day<-'(days_in_month(Date))) %>%
    select(date=Date, Value1m=Value, source, subtype) %>%
    mutate(var='Generation, hybrid', Unit="100 million kwh", data_source='Ember') ->
    ember_chn

  pwr_data$monthly %>%
    filter(date<=focus_month, var=='Generation, hybrid') %>%
    mutate(data_source='CREA') %>%
    bind_rows(ember_chn) %>% filter(!is.na(Value1m)) %>%
    group_by(date, source, subtype, var) %>%
    filter(data_source=='CREA' | 'CREA' %notin% data_source) ->
    pwr_growth_plot

  pwr_growth_plot %<>%
    (function(df) {
      df %>% filter(source!='Total',
                    source!='Thermal' | !is.na(subtype)) %>%
        group_by(var, Unit, date) %>%
        summarise(across(Value1m, sum)) %>%
        mutate(source='Total') %>%
        bind_rows(df %>% filter(source!='Total'))
    }) %>%
    group_by(source, subtype) %>%
    mutate(YoY_change_absolute_1m=get.yoy(Value1m, date, 'absolute'),
           label=na.cover(subtype, source)) %>%
    filter(year(date)>2015, !is.na(YoY_change_absolute_1m))


  pwr_growth_plot %<>%
    mutate(label=factor(label, levels=c('Coal', 'Gas', 'Thermal',
                                        'Biomass','Hydro', 'Nuclear',
                                        'Wind', 'Solar', 'Total')),
           broad_label=case_when(label %in% c('Solar', 'Wind')~'Solar & wind',
                                 label %in% c('Nuclear', 'Hydro', 'Biomass')~'Hydro, nuclear & biomass',
                                 label %in% c('Coal', 'Gas')~'Coal & gas',
                                 label=='Total'~label))

  pwr_cols = c(fuel_cols[-5],
               Nuclear=unname(crea_palettes$CREA['Orange']),
               Biomass=unname(crea_palettes$CREA['Green']),
               Coal=unname(crea_palettes$CREA['Black']),
               Gas=unname(crea_palettes$CREA['Light.gray']))

  if(make_additional_plots) {
    pwr_growth_plot %>% filter(label %notin% c('Total', 'Thermal')) %>%
      ggplot(aes(date, YoY_change_absolute_1m/10, fill=label)) + geom_col() +
      scale_fill_manual(values=pwr_cols) +
      theme_crea() +
      labs(title='Growth in monthly power generation by source', y='TWh', fill='') -> p
    quicksave(file.path(output_dir, 'Growth in monthly power generation by source.png'),
              plot=p, logo=F, scale=1)


    pwr_growth_plot %>% filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass')) %>%
      ggplot(aes(date, YoY_change_absolute_1m/10)) + geom_col(aes(fill=label)) +
      geom_point(data=pwr_growth_plot %>% filter(label=='Total'), aes(col=label)) +
      scale_fill_manual(values=pwr_cols) +
      theme_crea() +
      labs(title='Growth in monthly clean and total power generation by source', y='TWh', fill='', col='') -> p
    quicksave(file.path(output_dir, 'Growth in monthly clean and total power generation by source.png'),
              plot=p, logo=F, scale=1)


    pwr_growth_plot %>%
      filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass', 'Coal', 'Gas', 'Hydro', 'Total')) %>%
      ggplot(aes(date, YoY_change_absolute_1m/10, fill=label)) +
      facet_wrap(~label) +
      geom_col() +
      scale_fill_manual(values=pwr_cols) +
      theme_crea() +
      labs(title='Growth in monthly power generation by source', y='TWh', fill='') -> p
    quicksave(file.path(output_dir, 'Growth in monthly clean and total power generation by source, pivoted.png'),
              plot=p, logo=F, scale=1)

    pwr_growth_plot %>%
      filter(!is.na(broad_label)) %>%
      group_by(date, broad_label) %>%
      summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
      ggplot(aes(date, YoY_change_absolute_1m/10, fill=broad_label)) +
      facet_wrap(~broad_label) +
      geom_col() +
      theme_crea() +
      labs(title='Growth in monthly power generation by source', y='TWh', fill='', x='') +
      scale_fill_crea_d('change', col.index = c(7,5,2,1), guide='none') -> p
    quicksave(file.path(output_dir, 'Growth in monthly clean and total power generation by source category, pivoted.png'),
              plot=p, logo=F, scale=1)

    pwr_data$monthly %>% filter(var=='Utilization', !(subtype %eqna% 'Biomass'), year(date)>=2018,
                                year(date)>=2020 | is.na(subtype) | subtype %notin% c('Coal', 'Gas')) %>%
      mutate(plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1), year=as.factor(year(date)),
             label=na.cover(subtype, source)) %>%
      ggplot(aes(plotdate, Value1m, col=year)) + facet_wrap(~label) + geom_line() +
      labs(title='Monthly running hours') +
      scale_x_date(date_labels = '%b') +
      theme_crea() +
      scale_color_crea_d('change') -> p
    quicksave(file.path(output_dir, 'Monthly running hours.png'),
              plot=p, logo=F, scale=1)

    pwr_growth_plot %>% group_by(date) %>%
      mutate(plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1), year=as.factor(year(date)),
             generation_share = Value1m/Value1m[label=='Total']) %>%
      filter(label %notin% c('Thermal', 'Total')) %>%
      ggplot(aes(plotdate, generation_share, col=year)) + facet_wrap(~label, scales='free_y') + geom_line() +
      labs(title='Monthly shares of generation by technology') +
      scale_x_date(date_labels = '%b') -> p
    quicksave(file.path(output_dir, 'Monthly shares of generation by technology.png'),
              plot=p, logo=F, scale=1)
  }

  # power generation data with subtype
  pwr_growth_plot %>%
    filter(!is.na(broad_label)) %>%
    group_by(date, source, subtype, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    write_csv(file.path(output_dir, 'Growth in monthly power generation by source.csv'))

  pwr_growth_plot %>%
    filter(!is.na(broad_label)) %>%
    group_by(date, broad_label, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    write_csv(file.path(output_dir, 'Growth in monthly power generation by source category.csv')) %>%
    filter(broad_label!='Total') %>%
    ggplot(aes(date, YoY_change_absolute_1m/10)) +
    geom_col(aes(fill=broad_label)) +
    geom_point(data=pwr_growth_plot %>% filter(label=='Total'),
               mapping=aes(shape=label)) +
    theme_crea(legend.position='top') +
    labs(title='Growth in monthly power generation by source', y='TWh', fill='', x='') +
    scale_fill_crea_d('change', col.index = c(7,5,2,1),
                      guide=guide_legend(nrow=1)) +
    scale_x_date(expand=expansion(mult=c(.01, .01))) +
    scale_shape(name='') -> p
  quicksave(file.path(output_dir, 'Growth in monthly power generation by source category.png'),
            plot=p, logo=F, scale=1)


  pwr_growth_plot %>%
    mutate(broad_label=case_when(label %in% c('Coal', 'Gas')~label, T~broad_label)) %>%
    filter(!is.na(broad_label), broad_label!='Total') %>%
    group_by(date, broad_label, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    ggplot(aes(date, Value1m/10)) +
    geom_col(aes(fill=broad_label), position='fill') +
    theme_crea(legend.position='top') +
    labs(title='Monthly power generation mix', y='TWh', fill='', x='') +
    scale_fill_crea_d('change', col.index = c(7,5,2,1),
                      guide=guide_legend(nrow=1)) +
    scale_x_date(expand=expansion(mult=c(.01, .01))) +
    x_at_zero(labels=scales::percent) -> p
  quicksave(file.path(output_dir, 'Monthly power generation mix.png'),
            plot=p, logo=F, scale=1)

  pwr_growth_plot %>%
    select(date, label, broad_label, source, subtype, value=Value1m, contains('YoY'), Unit, data_source) %>%
    write_csv('outputs/power_data.csv')

  pwr_data$monthly %>% filter(var=='Capacity', source %in% c('Wind', 'Solar')) %>%
    group_by(source, subtype) %>%
    mutate(change=Value1m-lag(Value1m),
           plotdate=date %>% 'year<-'(2022), year=as.factor(year(date))) %>%
    group_by(source, subtype, year) %>%
    mutate(change_cumulative=cumsum(change)) %>%
    filter(year(date)>=2020) %>%
    write_csv(file.path(output_dir, 'Newly added wind and solar.csv')) %>%
    ggplot(aes(plotdate, change_cumulative/100, col=year)) + geom_line(linewidth=1) +
    facet_wrap(~source, ncol=1, scales='free_y') +
    theme_crea() + scale_color_crea_d('change', col.index = c(1:3,5:7)) +
    x_at_zero() +
    scale_x_date(date_labels = '%b') +
    labs(title='Newly added power capacity, year-to-date', x='', y='GW') -> p
  quicksave(file.path(output_dir, 'Newly added wind and solar.png'),
            plot=p, logo=F, scale=.8)

  #I couldn't get this to run in the function; it does run in the terminal -Lauri
  if(F) {
    system('git add outputs/power_data.csv')
    system(paste0('git commit -m "power data until ',focus_month,'"'))
    system('git push')
  }
}
