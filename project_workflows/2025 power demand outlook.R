readwindEN(get_data_file('Electricity consumption by sector YTD.xlsx'), c('var', 'sector'), skip=2, read_vardata = T) ->
  elec_cons_broad

elec_cons_broad %<>% mutate(sector=case_when(grepl('Residen', Name)~'Residential',
                                             grepl('Primary', sector)~'Agriculture',
                                             grepl('Seconda', sector)~'Industry',
                                             grepl('Tertia', sector)~'Services',
                                             grepl('Whole', sector)~'Total')) %>%
  group_by(var, sector) %>% unYTD() %>% roll12m()

elec_cons_broad %<>% group_by(var, sector, month(date)) %>%
  mutate(across(starts_with('Value'), list(yoy_change=~.x-lag(.x))))

start_year=2012

target_dates <- seq.Date(min(elec_cons_broad$date) %>% 'day<-'(1), ymd('2026-12-01'), by='month') %>% 'day<-'(days_in_month(.))
elec_cons_broad %<>% group_by(var, sector) %>%
  complete(date=target_dates) %>%
  group_modify(function(df, group) {
    message(group)

    form <- Value12m ~ a * exp(b * as.numeric(date)/365)
    #if(group$sector=='Agriculture') form <- Value12m ~ a+b * as.numeric(date)/365

    #df %>% filter(year(date) %in% 2012:2019) %>% lm(Value12m~exp(as.numeric(date)/365), data=.) -> m
    df %>% filter(year(date) %in% 2012:2019, !is.na(Value12m)) %>%
      nls(form, data = ., start = list(a = 1, b = 0.1)) -> m
    df$Value12m_pre_covid_trend <- predict(m, df)
    g_pre <- m %>% summary %>% '$'('coefficients') %>% '['('b', 'Estimate')
    df$pre_covid_growth_rate <- g_pre

    if(group$sector!='Agriculture')
      df %>% filter(year(date) %in% 2019:2024, !is.na(Value12m)) %>%
      nls(form, data = ., start = list(a = 1, b = 0.1)) -> m

    df$Value12m_covid_trend <- predict(m, df)
    g_post <- m %>% summary %>% '$'('coefficients') %>% '['('b', 'Estimate')
    df$covid_growth_rate <- g_post

    last_date <- df %>% filter(!is.na(Value12m)) %>% use_series(date) %>% max
    df$Value12m_return_to_pre_covid_growth_trend <- df$Value12m[df$date==last_date] * (1 + g_pre)^(as.numeric(df$date-last_date)/365)
    g_reversal <- g_pre - (g_post-g_pre)
    df$Value12m_reversal_to_pre_covid_trend <- df$Value12m[df$date==last_date] * (1 + g_reversal)^(as.numeric(df$date-last_date)/365)

    df$Value12m_covid_trend[year(df$date)<2020] <- NA
    df$Value12m_return_to_pre_covid_growth_trend[df$date<last_date] <- NA
    df$Value12m_reversal_to_pre_covid_trend[df$date<last_date] <- NA

    df
  })

elec_cons_broad %>% select(date, sector, starts_with('Value12m'), -contains('yoy')) %>%
  pivot_longer(starts_with('Value12m')) %>%
  mutate(name=disambiguate(name, c('return to pre-Covid growth rates'='return_to_pre_covid_growth',
                                   'reversal to pre-Covid trendline'='reversal_to_pre_covid',
                                   'pre-Covid trend'='pre_covid_trend',
                                   'Covid-period trend'='covid_trend',
                                   'actual data'='Value12m'))) ->
  elec_cons_trends

elec_cons_trends %>% filter(sector!='Agriculture', year(date)>=start_year) %>%
  filter(sector=='Total') %>%
  ggplot(aes(date, value*12/10, col=name, linetype=name, alpha=name)) + geom_line(linewidth=1) +
  #facet_wrap(~sector) +
  x_at_zero() + snug_x_date +
  theme_crea_new() + theme(legend.position = 'right', legend.title.position = 'top') +
  scale_color_crea_d('dramatic', name='scenario', guide=guide_legend(ncol=1)) +
  scale_linetype_manual(values=c('solid', 'dashed', 'dotted', 'dashed', 'dashed'), name='scenario', guide=guide_legend(ncol=1)) +
  scale_alpha_manual(values=c(1, rep(.6,4)), name='scenario', guide=guide_legend(ncol=1)) +
  labs(title="China's electricity consumption by sector: scenarios",
       subtitle='12-month rolling mean',
       y='TWh/year', x='')
quicksave('outputs/2024Q4/electricity consumption by sector, scenarios.png', scale = .8, increase_plot_margin_around_logo = 1)

elec_cons_broad %>% filter(year(date)>=start_year, Value12m>0) %>%
  mutate(across(starts_with('Value12m'), ~.x/10*12)) %>%
  ggplot(aes(date, Value12m, col=sector)) +
  geom_line(linewidth=1) +
  geom_line(aes(y=Value12m_pre_covid_trend, linetype='pre-Covid trend')) +
  #geom_smooth(data=elec_cons_broad %>% filter(year(date)>=start_year, year(date)<2020),
  #            aes(linetype='pre-Covid trend'),
  #            method='lm', formula=y~exp(as.numeric(x)/36500), fullrange=T, se=F) +
  scale_linetype_manual(values='dotted') +
  theme_crea_new() +
  theme(legend.text = element_text(size=rel(.6)),
        plot.margin = unit(rep(.5, 4), 'cm')) +
  scale_color_crea_d('dramatic') +
  x_at_zero() +
  labs(title="China's electricity consumption by sector",
       subtitle='12-month rolling mean',
       y='TWh/year',
       x='', color='', linetype='') +
  snug_x_date -> p
quicksave('outputs/2024Q4/electricity consumption by sector.png', scale = .6, increase_plot_margin_around_logo = 1, plot=p)

p + expand_limits(y=11e3, x=ymd('2026-12-31'))
quicksave('outputs/2024Q4/electricity consumption by sector, with space.png', scale = .6, increase_plot_margin_around_logo = 1)

elec_cons_broad %>%
  filter(year(date)>=2012, sector!='Agriculture') %>%
  ggplot(aes(date, Value12m_yoy_change, fill=sector)) + geom_area() + facet_wrap(~sector) +
  theme_crea_new() + scale_fill_crea_d('dramatic', guide='none') +
  labs(title='Electricity consumption growth by sector',
       subtitle='year-to-year change in 12-month rolling mean',
       y='TWh/year',
       x='') +
  snug_x_date

elec_cons_broad %>% ungroup %>%
  filter(year(date)>=2022, sector!='Agriculture') %>%
  mutate(across(sector, as.factor),
         Value1m_yoy_change=Value1m_yoy_change/10) -> plotdata

plotdata %>%
  ggplot(aes(date, Value1m_yoy_change, fill=sector)) +
  geom_col() +
  geom_smooth(aes(linetype='trend'), color=col.a('orange', .75), linewidth=1, se=F) +
  facet_wrap(~sector) +
  theme_crea_new() +
  scale_fill_crea_d('dramatic', guide='none') +
  scale_linetype_manual(values='solid', name='') +
  labs(title='Electricity consumption growth by sector',
       subtitle='monthly change year-to-year',
       y='TWh/month',
       x='') +
  snug_x_date -> p
quicksave('outputs/2024Q4/electricity consumption growth by sector.png', scale = .7, plot=p)

plotted_data <- ggplot_build(p)$data
plotted_data[[2]] %>%
  mutate(date=as_date(x)) %>%
  select(sector_code=PANEL, date, Value1m_yoy_change=y) ->
  fitted_data

plotdata %>% select(sector, date, Value1m_yoy_change) %>%
  group_by(sector, sector_code=as.numeric(sector)) %>%
  group_modify(function(df, group) {
    fitted_data %>% filter(sector_code==group$sector_code) -> fit_df
    df$trend <- approx(fit_df$date, fit_df$Value1m_yoy_change, df$date)$y
    df
  }) %>% na.omit %>% select(-sector_code) %>% mutate(unit='TWh/month') %>%
  write_csv(file.path('outputs/2024Q4/electricity consumption growth by sector.csv')) %>%
  ggplot(aes(date)) +
  geom_line(aes(y=Value1m_yoy_change)) +
  geom_line(aes(y=trend), col='red') +
  facet_wrap(~sector)
