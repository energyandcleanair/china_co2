power_generation_plots <- function(focus_month = today() %>% subtract(30) %>% 'day<-'(days_in_month(.)),
                                   lang = parent.frame()$lang,
                                   output_dir = get('output_dir', envir = .GlobalEnv),
                                   make_additional_plots=F) { #make a number of plots in addition to the main ones
  pwr_data <- read_power_generation()

  try(ember <- read_csv("https://ember-climate.org/app/uploads/2022/07/monthly_full_release_long_format-4.csv"))
  if(!exists("ember")) ember <- read_csv(get_data_file("monthly_full_release_long_format-4.csv"))

  ember_chn <- ember %>% filter(Area == 'China', Unit == 'TWh',
                                Variable %in% c('Wind', 'Solar', 'Coal', 'Gas', 'Hydro',
                                                'Nuclear', 'Biomass')) %>%
    mutate(Value = Value * 10,
           Variable = ifelse(Variable == 'Bioenergy', 'Biomass', Variable),
           source = case_when(Variable %in% c('Coal', 'Gas', 'Biomass') ~ 'Thermal',
                              Variable == 'Total Generation' ~ 'Total',
                              T ~ Variable),
           subtype = ifelse(Variable %in% c('Coal', 'Gas', 'Biomass'), Variable, NA),
           Date = Date %>% 'day<-'(days_in_month(Date))) %>%
    select(date=Date, Value1m = Value, source, subtype) %>%
    mutate(var = 'Generation, hybrid', Unit = "100 million kwh", data_source = 'Ember')


  pwr_growth_plot <- pwr_data$monthly %>%
    filter(date <= focus_month, var == 'Generation, hybrid') %>%
    mutate(data_source = 'CREA') %>%
    bind_rows(ember_chn) %>%
    filter(!is.na(Value1m)) %>%
    group_by(date, source, subtype, var) %>%
    filter(data_source == 'CREA' | 'CREA' %notin% data_source)


  pwr_growth_plot %<>%
    (function(df) {
      df %>% filter(source != 'Total',
                    source != 'Thermal' | !is.na(subtype)) %>%
        group_by(var, Unit, date) %>%
        summarise(across(Value1m, sum)) %>%
        mutate(source = 'Total') %>%
        bind_rows(df %>% filter(source != 'Total'))
    }) %>%
    group_by(source, subtype) %>%
    mutate(YoY_change_absolute_1m = get.yoy(Value1m, date, 'absolute'),
           label = na.cover(subtype, source)) %>%
    filter(year(date) > 2015, !is.na(YoY_change_absolute_1m))


  pwr_growth_plot %<>%
    mutate(label = factor(label, levels=c('Coal', 'Gas', 'Thermal',
                                          'Biomass','Hydro', 'Nuclear',
                                          'Wind', 'Solar', 'Total')),
           broad_label = case_when(label %in% c('Solar', 'Wind') ~ 'Solar & wind',
                                   label %in% c('Nuclear', 'Hydro', 'Biomass') ~
                                     'Hydro, nuclear & biomass',
                                   label %in% c('Coal', 'Gas') ~ 'Coal & gas',
                                   label == 'Total' ~ label))

  pwr_cols = c(fuel_cols[-5],
               Nuclear = unname(crea_palettes$CREA['Orange']),
               Biomass = unname(crea_palettes$CREA['Green']),
               Coal = unname(crea_palettes$CREA['Black']),
               Gas = unname(crea_palettes$CREA['Light.gray']))

  if(lang == 'ZH'){
    names(pwr_cols) <- pwr_cols %>% names() %>% trans
  }

  if(make_additional_plots) {
    p <- pwr_growth_plot %>% filter(label %notin% c('Total', 'Thermal')) %>%
      ggplot(aes(date, YoY_change_absolute_1m / 10, fill = trans(label))) +
      geom_col() +
      scale_fill_manual(values = pwr_cols) +
      theme_crea() +
      labs(title = trans('Growth in monthly power generation by source'),
           x = '', y = trans('TWh'), fill = '')
    quicksave(file.path(output_dir,
                        paste0('Growth in monthly power generation by source ',
                               lang, '.png')),
              plot = p, logo = T, scale = 1) +
              lang_theme()


    p <- pwr_growth_plot %>%
      filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass')) %>%
      ggplot(aes(date, YoY_change_absolute_1m / 10)) +
      geom_col(aes(fill = trans(label))) +
      geom_point(data = pwr_growth_plot %>% filter(label == 'Total'),
                 aes(col = trans(label))) +
      scale_fill_manual(values = pwr_cols) +
      theme_crea() +
      labs(title = trans('Growth in monthly clean and total power generation by source'),
           x = '', y = trans('TWh'), fill = '', col = '')
    quicksave(file.path(output_dir,
                        paste0('Growth in monthly clean and total power generation by source ',
                               lang, '.png')),
              plot = p, logo = T, scale = 1)


    p <- pwr_growth_plot %>%
      filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass', 'Coal',
                          'Gas', 'Hydro', 'Total')) %>%
      ggplot(aes(date, YoY_change_absolute_1m / 10, fill = trans(label))) +
      facet_wrap(~ trans(label)) +
      geom_col() +
      scale_fill_manual(values = pwr_cols) +
      theme_crea() +
      labs(title = trans('Growth in monthly power generation by source'),
           x = '', y = trans('TWh'), fill = '')
    quicksave(file.path(output_dir,
                        paste0('Growth in monthly clean and total power generation by source, pivoted',
                               lang, '.png')),
              plot = p, logo = T, scale = 1)


    p <- pwr_growth_plot %>%
      filter(!is.na(broad_label)) %>%
      group_by(date, broad_label) %>%
      summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
      ggplot(aes(date, YoY_change_absolute_1m / 10, fill = trans(broad_label))) +
      facet_wrap(~ trans(broad_label)) +
      geom_col() +
      theme_crea() +
      labs(title = trans('Growth in monthly power generation by source'),
           x = '', y = trans('TWh'), fill = '') +
      scale_fill_crea_d('change', col.index = c(7, 5, 2, 1), guide = 'none') +
              lang_theme()
    quicksave(file.path(output_dir,
                        paste0('Growth in monthly clean and total power generation by source category, pivoted',
                               lang, '.png')),
              plot = p, logo = T, scale = 1)

    p <- pwr_data$monthly %>%
      filter(var=='Utilization', !(subtype %eqna% 'Biomass'), year(date)>=2018,
             year(date)>=2020 | is.na(subtype) | subtype %notin% c('Coal', 'Gas')) %>%
      mutate(plotdate = date %>% 'year<-'(2022) %>% 'day<-'(1),
             year = as.factor(year(date)),
             label = na.cover(subtype, source)) %>%
      ggplot(aes(plotdate, Value1m, col = year)) +
      facet_wrap(~ trans(label)) +
      geom_line() +
      labs(title = 'Monthly running hours', x = '', y = trans('hours'), col = '') +
      scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
      theme_crea() +
      scale_color_crea_d('change') +
      lang_theme()

    quicksave(file.path(output_dir, paste0('Monthly running hours ', lang, '.png')),
              plot = p, logo = T, scale = 1)

    p <- pwr_growth_plot %>% group_by(date) %>%
      mutate(plotdate = date %>% 'year<-'(2022) %>% 'day<-'(1),
             year = as.factor(year(date)),
             generation_share = Value1m / Value1m[label == 'Total']) %>%
      filter(label %notin% c('Thermal', 'Total')) %>%
      ggplot(aes(plotdate, generation_share, col = year)) +
      facet_wrap(~ trans(label), scales = 'free_y') +
      geom_line(labels = scales::percent) +
      labs(title = trans('Monthly shares of generation by technology'),
           x = '', y = '', col = '') +
      scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
      scale_y_continuous(labels = scales::percent) +
      lang_theme()
    quicksave(file.path(output_dir,
                        paste0('Monthly shares of generation by technology ',
                               lang, '.png')),
              plot = p, logo = T, scale = 1)
  }

  # power generation data with subtype
  pwr_growth_plot %>%
    filter(!is.na(broad_label)) %>%
    group_by(date, source, subtype, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    write_csv(file.path(output_dir, 'Growth in monthly power generation by source.csv'))

  p <- pwr_growth_plot %>%
    filter(!is.na(broad_label)) %>%
    group_by(date, broad_label, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    write_csv(file.path(output_dir,
                        'Growth in monthly power generation by source category.csv')) %>%
    filter(broad_label != 'Total') %>%
    ggplot(aes(date, YoY_change_absolute_1m / 10)) +
    geom_col(aes(fill = trans(broad_label))) +
    geom_point(data = pwr_growth_plot %>% filter(label == 'Total') %>%
                 mutate(label = 'Total growth'),
               mapping = aes(shape = trans(label))) +
    theme_crea(legend.position = 'top') +
    labs(title = trans('Growth in monthly power generation by source'),
         y = trans('TWh'), fill = '', x = '', shape = 'test') +
    scale_fill_crea_d('change', col.index = c(7, 5, 2, 1),
                      guide = guide_legend(nrow = 1)) +
    scale_x_date(expand = expansion(mult = c(.01, .01))) +
    scale_shape(name = '') +
    lang_theme()

  quicksave(file.path(output_dir,
                      paste0('Growth in monthly power generation by source category ',
                             lang, '.png')),
            plot = p, logo = T, scale = 1)


  p <- pwr_growth_plot %>%
    mutate(broad_label = case_when(label %in% c('Coal', 'Gas') ~ label,
                                   T ~ broad_label)) %>%
    filter(!is.na(broad_label), broad_label != 'Total') %>%
    group_by(date, broad_label, Unit) %>%
    summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
    ggplot(aes(date, Value1m / 10)) +
    geom_col(aes(fill = trans(broad_label)), position = 'fill') +
    theme_crea(legend.position = 'top') +
    labs(title = trans('Monthly power generation mix'), y = trans('TWh'),
         fill = '', x = '') +
    scale_fill_crea_d('change', col.index = c(7, 5, 2, 1),
                      guide = guide_legend(nrow = 1)) +
    scale_x_date(expand = expansion(mult=c(.01, .01))) +
    x_at_zero(labels = scales::percent) +
    lang_theme()

  quicksave(file.path(output_dir, paste0('Monthly power generation mix ', lang, '.png')),
            plot = p, logo = T, scale = 1)

  pwr_growth_plot %>%
    select(date, label, broad_label, source, subtype, value = Value1m,
           contains('YoY'), Unit, data_source) %>%
    write_csv(file.path(output_dir, 'power_data.csv'))

  p <- pwr_data$monthly %>% filter(var == 'Capacity',
                                   source %in% c('Wind', 'Solar')) %>%
    group_by(source, subtype) %>%
    mutate(change = Value1m - lag(Value1m),
           plotdate = date %>% 'year<-'(2022),
           year = as.factor(year(date))) %>%
    group_by(source, subtype, year) %>%
    mutate(change_cumulative = cumsum(change)) %>%
    filter(year(date) >= 2020) %>%
    write_csv(file.path(output_dir, 'Newly added wind and solar.csv')) %>%
    ggplot(aes(plotdate, change_cumulative / 100, col = year)) +
    geom_line(linewidth = 1) +
    facet_wrap(~ trans(source), ncol = 1, scales = 'free_y') +
    theme_crea() +
    scale_color_crea_d('change', col.index = c(1:3, 5:7)) +
    x_at_zero() +
    scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
    labs(title = trans('Newly added power capacity, year-to-date'),
         x = '', y = trans('GW'), col = '') +
    lang_theme()
  quicksave(file.path(output_dir, paste0('Newly added wind and solar ', lang, '.png')),
            plot = p, logo = T, scale = .8)

  # I couldn't get this to run in the function; it does run in the terminal -Lauri
  if(F) {
    system('git add outputs/power_data.csv')
    system(paste0('git commit -m "power data until ', focus_month, '"'))
    system('git push')
  }
}
