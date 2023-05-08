source('R/wind mapping functions.R')

require(creahelpers)
require(rcrea)

output_dir='outputs/2022Q4'

plot_formats = 
  list(geom_vline(aes(xintercept=ymd('2020-02-10'), linetype='First COVID-19 lockdown')),
       x_at_zero(), theme_crea(legend.position='top'),
       scale_linetype_manual(values='dashed', name=''),
       scale_color_crea_d('dramatic', name=''),
       scale_y_continuous(labels=scales::comma))
  

in_file = "data/transport volumes.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, c('var', 'subsector'), columnExclude = 'Guangdong', read_vardata = T) ->
  traffic


traffic %>% group_by(date, var, Unit) %>% 
  summarise(across(Value, sum)) %>% 
  filter(date>='2015-01-01', Value>0,
         grepl('Civil Aviation.*Passenger|Highway Freight|Highway Passenger.*kilo',
               var)) %>% 
  group_by(var) %>% 
  mutate(Value = Value * ifelse(grepl('100 million', Unit), 1e8, 1e4),
         unit_multiplier = case_when(max(Value)>1e12~'trillion',
                                     max(Value)>1e9~'billion',
                                     max(Value)>1e6~'million'),
         Value = Value / case_when(unit_multiplier=='trillion'~1e12,
                                   unit_multiplier=='billion'~1e9,
                                   unit_multiplier=='million'~1e6),
         Unit = Unit %>% 
           gsub('10000 |100 million |10k ', '', .) %>% 
           gsub('persons', 'person', .) %>% 
           gsub('\\*', '-', .) %>% 
           paste(unit_multiplier, .)) ->
  traffic_plot

traffic_plot %>% 
  ggplot(aes(date, Value)) + geom_line(size=1) +
  facet_wrap(~var+Unit, scales='free_y') +
  geom_smooth(data=traffic_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='gam',
              aes(col='pre-COVID trendline')) +
  plot_formats +
  snug_x_date + x_at_zero() +
  labs(title='China transport trends', x='', y='') -> plt
quicksave(file.path(output_dir, 'China transport trends.png'), plot=plt)

traffic_plot %>% ungroup %>% distinct(var, Unit) %>% 
  mutate(PANEL=as.factor(seq_along(var))) -> traffic_panels

ggplot_build(plt)$data[[2]] %>% 
  mutate(date=as_datetime(x*24*3600, origin='1970-01-01')) %>% 
  group_by(PANEL) %>% 
  group_modify(function(df, ...) {
    tibble(date=unique(traffic_plot$date)) %>% 
      mutate(value=approx(df$date, df$y, as_datetime(date))$y,
             value_min=approx(df$date, df$y, as_datetime(date))$y,
             value_max=approx(df$date, df$y, as_datetime(date))$y)
  }) %>% left_join(traffic_panels) %>% ungroup %>% select(-PANEL) %>% 
  mutate(scenario='pre-COVID trendline') -> traffic_trend
  
traffic_trend %>% 
  bind_rows(traffic_plot %>% mutate(scenario='actual data') %>% 
              select(any_of(names(traffic_trend)))) %>% 
  write_csv(file.path(output_dir, 'China transport trends.csv'))



in_file = "data/real estate indicators.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, 'var', columnFilter = 'Floor Space', read_vardata = T) ->
  realestate

realestate %>% group_by(var, Unit) %>% unYTD %>% roll12m() %>% 
  filter(date>='2015-01-01', grepl('Sold|Newly', var)) -> realestate_plot


realestate_plot %>% 
  ggplot(aes(date, Value12m/100)) + geom_line(linewidth=1, color=crea_palettes$CREA[1]) +
  facet_wrap(~var, scales='free_y') +
  #geom_smooth(data=realestate_plot %>% filter(date<='2020-01-01'),
  #            fullrange=T, method='lm') +
  plot_formats + 
  labs(title='Real estate trends', x='', y='million m2') -> plt
quicksave(file.path(output_dir, 'Real estate trends.png'), plot=plt)


d.adj2 %>% replace_na(list(sector='All')) %>% 
  filter(date>='2016-01-01', grepl('consumption', var, ignore.case=T) | prod=='Cement', 
                  grepl('Cement|Oil Products|Coal|Coking|Natural', prod),
                  grepl('All|Power|Total', sector) | grepl('Coking', prod),
                  !grepl('WIND', var)) ->
  energy_plot


energy_plot %<>% filter(prod=='Steam Coal') %>% 
  group_by(prod, date, Unit, var) %>% 
  summarise(across(is.numeric, function(x) x[sector=='Total']-x[sector=='Power Industry'])) %>% 
  mutate(sector='Non-power use') %>% 
  bind_rows(energy_plot) %>% filter(sector != 'Total')

energy_plot %>% 
  ggplot(aes(date, CO2_12m*12)) + geom_line(size=1, col=crea_palettes$dramatic[2]) +
  facet_wrap(~prod+sector, scales='free_y') +
  geom_smooth(data=energy_plot %>% filter(date<='2020-01-01'),
              aes(col='pre-COVID trendline'),
              fullrange=T, method='lm') +
  plot_formats +
  labs(title='Trends in China CO2 by fuel and sector',
       x='', y='Mt/year, 12-month moving mean') -> plt
quicksave(file.path(output_dir, 'Trends in China CO2 by fuel and sector.png'), plot=plt)

energy_plot %>% 
  group_by(prod, sector) %>% 
  group_modify(function(df, ...) {
    lm(CO2_12m~date, data=df %>% filter(year(date) %in% 2016:2019)) -> m
    df %>% mutate(pre_covid_trend = predict(m, df))
  }) %>% filter(date == max(date)) %>% 
  mutate(anomaly_perc = CO2_12m/pre_covid_trend-1,
         anomaly_mt = (CO2_12m-pre_covid_trend)*12) %>% 
  select(date, starts_with('anomaly'))

energy_plot %>% group_by(date) %>% 
  summarise(across(where(is.numeric), sum)) ->
  co2_plot

co2_plot %>% 
  ggplot(aes(date, CO2_12m*12/1e3)) + geom_line(size=1, col=crea_palettes$dramatic[2]) +
  geom_smooth(data=co2_plot %>% filter(date<='2020-01-01'),
              aes(col='pre-COVID trendline'),
              fullrange=T, method='lm') +
  plot_formats +
  labs(title='Trends in China CO2 emissions', x='', y='Gt/year, 12-month moving mean') -> plt
quicksave(file.path(output_dir, 'Trends in China CO2.png'), plot=plt)




in_file = "data/monthly industry stats.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption', read_vardata = T) -> prod





prod %>% 
  filter(grepl('Steel Material|Crude Steel|Cement$|Chemical|Plastic|Coke|Copper|Metals|Aluminous|Glass|Pig Iron|Power Demand',
               prod)) %>% 
  group_by(var, prod, type) %>%
  unYTD() %>% filter(Value1m>0) %>% roll12m %>% 
  filter(date>='2016-01-01') ->
  prod_plot

prod_plot %>% 
  ggplot(aes(date, Value12m)) + geom_line() +
  facet_wrap(~prod, scales='free_y') +
  geom_smooth(data=prod_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='lm') +
  geom_vline(aes(xintercept=ymd('2020-01-29'), linetype='First COVID-19 lockdown'))





