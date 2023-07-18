industry_output_plots  <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1),
                                   lang=parent.frame()$lang,
                                   output_dir=get('output_dir', envir=.GlobalEnv)) {

  in_file = get_data_file("monthly industry stats.xlsx")
  readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption', read_vardata = T) -> prod

  yoy_2020M7 <- prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2020-07-31'] /
    prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2019-07-31']

  prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2020-05-31'] <-
    prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2019-05-31'] * yoy_2020M7
  prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2020-06-30'] <-
    prod$Value[grepl('Solar Cells', prod$prod) & prod$date=='2019-06-30'] * yoy_2020M7

  prod %<>%
    group_by(var, prod, type) %>%
    group_modify(function(df, k) {
      message(k)
      df %>% unYTD() %>%
        filter(Value1m>0) %>% roll12m %>% seasonal(year_range = 2012:2019) })

  prod$date %>% max -> latest_date
  prod %>% group_by(prod) %>%
    filter(latest_date %in% date) ->
    prod_withlatest



  prod_withlatest %<>%
    group_by(prod) %>%
    roll12m(months=3, incol='Value.seasonadj', outcol='Value3m.seasonadj') %>%
    mutate(YoY=get.yoy(Value3m.seasonadj, date))


  plots = list(#'Industrial output'='',
    #'Metals&cement output'='Steel Material|Crude Steel|Cement$|Coke|Pig Iron|Non-ferrous|Copper',
    #'Heavy industry output'='Steel Material|Crude Steel|Cement$|Chemical|Plastic|Coke|Copper|Metals|Aluminous|Glass|Pig Iron',
    'Heavy industry output'='Steel Material|Crude Steel|Cement$|Chemical|Plastics|Coke|Copper|Metals|Glass|Pig Iron',
    #'Housing indicators'='Household|TV|Escalator',
    #'Transport fuel production'='Diese|Gasoline|Kerosene',
    #'Coal mine output'='Raw Coal',
    #'Solar cell output'='Solar Cells',
    'Power generation'='Solar$|Power$|Generating|Nuclear|Hydro')

  for(i in 1:length(plots)) {
    prod_withlatest %>% filter(year(date)>=2017, grepl(plots[[i]], prod),
                               !is.na(Value1m)) %>%
      mutate(Unit_multiplier=case_when(Unit=='10000 weight-box'~1/20, T~1),
             Unit=case_when(Unit == '10000 weight-box'~'10000 tons', T~Unit),
             prod = case_when(prod=='Generating Capacity'~'Electricity',
                              prod=='Solar'~'Solar power',
                              T~gsub(' Generating Capacity', '', prod))) -> plotdata

    labscale=1.15
    if(length(unique(plotdata$prod))>3) labscale=.9

    plotdata %>% write_csv(file.path(output_dir, paste0(names(plots)[i], '.csv')))

    plotdata %<>% mutate(across(c(Value.seasonadj, Value1m), ~convert_value(.x, Unit)*Unit_multiplier),
                         YoY_3m = (Value3m.seasonadj/lag(Value3m.seasonadj, 12)-1)  %>% pmax(-.2) %>% pmin(.2),
                         plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1),
                         year=as.factor(year(date)))
    plotdata %>% mutate(YoY=get.yoy(Value1m, date) %>% scales::percent(accuracy = 1, style_positive='plus')) %>%
      filter(date %>% 'day<-'(1) %>% equals(focus_month)) -> yoy_labels

    plotdata %>%
      ggplot(aes(date, Value.seasonadj, col=YoY_3m))+
      geom_line(size=.8)+geom_point(size=.8)+
      geom_label(data=yoy_labels, aes(label=YoY), vjust=4, hjust=1) +
      facet_wrap(~trans(prod), scales='free_y') +
      scale_color_gradientn(colors=colorspace::darken(crea_palettes$change), labels=scales::percent,
                            name=trans('year-on-year change')) +
      labs(title=trans(names(plots)[i]),
           subtitle=trans('seasonally adjusted monthly data'), #'12-month moving sum',
           x='', y=unit_label(unique(plotdata$Unit), lang=lang)) +
      theme_crea() +
      theme(strip.text = element_text(size=rel(labscale*.8)),
            axis.text.y = element_text(size=rel(labscale))) +
      geom_vline(aes(linetype=trans('COVID-19 lockdown'), xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
      scale_linetype_manual(values='dashed', name='') +
      expand_limits(y=0) +
      x_at_zero() +
      scale_x_date(labels = yearlab) -> p
    quicksave(file.path(output_dir, paste0(names(plots)[i], '_seasonal, ',lang,'.png')), plot=p, scale=1.2)

    plotdata %>%
      ggplot(aes(plotdate, Value1m, col=year))+
      geom_line(size=.8)+
      geom_label(data=yoy_labels, aes(label=YoY), vjust=ifelse(i==2, -.5, 5), hjust=1, key_glyph="path") +
      facet_wrap(~trans(prod), scales='free_y') +
      scale_color_manual(values=colorspace::darken(crea_palettes$change), name=trans('year')) +
      labs(title=trans(names(plots)[i]),
           x='', y=unit_label(unique(plotdata$Unit, lang=lang)),
           caption=trans('Labels show year-on-year changes in the latest month of data')) +
      theme_crea() +
      #geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
      scale_linetype_manual(values='dashed', name='') +
      expand_limits(y=0) +
      x_at_zero() +
      scale_x_date(labels = monthlab) -> p
    quicksave(file.path(output_dir, paste0(names(plots)[i], '_monthly_by_year, ',lang,'.png')), plot=p, scale=ifelse(lang=='ZH', 1.2, 1.33))
  }

  #solar cell output
  prod_withlatest %>% filter(year(date)>=2015, grepl('Solar Cells', prod)) %>%
    mutate(YoY = (Value1m/lag(Value1m, 12)-1)  %>% pmax(-.5) %>% pmin(.5)) %>%
    mutate(Value12m=convert_value(Value12m, Unit)*12) -> solar_plotdata

  solar_labels <- solar_plotdata %>% filter(date==max(date) | month(date)==12)

  solar_plotdata %>%
    ggplot(aes(date, Value12m, col=YoY, label=round(Value12m,0)))+
    geom_line(size=1.2)+geom_point(size=.8)+
    geom_text(data=solar_labels, vjust=-.4, hjust=1.2, fontface='bold') +
    labs(title=trans('Solar cell output'),
         subtitle=trans('12-month moving sum'),
         x='', y=unit_label('10000 kw', lang=lang),
         col=trans('year-on-year change')) +
    theme_crea() +
    scale_linetype_manual(values='dashed', name='') +
    expand_limits(y=0) +
    scale_color_crea_c('change', labels=scales::percent, guide='none') +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) +
    scale_x_date(labels = yearlab) -> p
  quicksave(file.path(output_dir, paste0('solar cell output, ',lang,'.png')), plot=p)

  #EV and car output
  prod_withlatest %>% filter(year(date)>=2017, grepl('Automob|Vehicle', prod)) -> plotdata1

  plotdata1 %>%
    ggplot(aes(date, convert_value(Value12m, Unit)*12, col=prod))+
    geom_line(size=1.2)+geom_point(size=.8)+
    facet_wrap(~trans(prod), scales='free_y',ncol=1) +
    scale_color_crea_d('dramatic', guide=F) +
    labs(title=trans('Vehicle production'), subtitle='', x='', y=trans('million units, 12-month moving sum')) +
    theme_crea() + theme(strip.text = element_text(size=rel(1)), legend.position = 'top',
                         plot.title=element_text(size=rel(3))) +
    geom_vline(aes(linetype=trans('COVID-19 lockdown'), xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
    scale_linetype_manual(values='dashed', name='') +
    scale_x_date(labels=yearlab) +
    expand_limits(y=0) + x_at_zero() -> p1

  plotdata1 %>%
    group_by(prod) %>%
    roll12m(months=3, outcol='Value3m') %>%
    group_by(date) %>% summarise(Value3m = Value3m[grepl('New Energy', prod)]/Value3m[grepl('Auto', prod)]) %>%
    mutate(prod='Share of New Energy Vehicles', Unit='percent') -> plotdata2

  plotdata2 %>%
    ggplot(aes(date, Value3m, col=trans(prod)))+
    geom_line(size=1.2)+geom_point(size=.8)+
    scale_color_crea_d('dramatic', guide='none', col.index = 3) +
    labs(y=trans('new energy vehicle share, 3-month moving average'), title=' ', subtitle=' ', x='') +
    theme_crea() +
    geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
    scale_linetype_manual(values='dashed', name='', guide=F) +
    scale_y_continuous(labels = scales::percent, breaks=function(x) seq(0,x[2],.05),
                       expand=expansion(mult=c(0,.05))) +
    scale_x_date(labels=yearlab) +
    expand_limits(y=0) -> p2

  plot_grid(p1,p2, nrow=1) -> g
  quicksave(file.path(output_dir, paste0('Vehicle production, ',lang,'.png')), plot=g, footer_height=.01)

  bind_rows(plotdata1, plotdata2) %>%
    select(date, prod, Unit, Value12m) %>%
    write_csv(file.path(output_dir, 'vehicle production.csv'))

}