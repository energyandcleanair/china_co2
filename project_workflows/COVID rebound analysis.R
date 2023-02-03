source('R/wind mapping functions.R')

require(creahelpers)
require(rcrea)

plot_formats = 
  list(geom_vline(aes(xintercept=ymd('2020-02-10'), linetype='First COVID-19 lockdown')),
         x_at_zero(), theme_crea(),
         scale_linetype_manual(values='dashed', name=''),
         scale_color_crea_d('dramatic', name=''))
  

in_file = "data/transport volumes.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, c('var', 'subsector'), columnExclude = 'Guangdong', read_vardata = T) ->
  traffic


traffic %>% group_by(date, var, Unit) %>% summarise(across(Value, sum)) %>% filter(date>='2015-01-01') ->
  traffic_plot

traffic_plot %>% 
  ggplot(aes(date, Value)) + geom_line(size=1) +
  facet_wrap(~var+Unit, scales='free_y') +
  geom_smooth(data=traffic_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='gam',
              aes(col='pre-COVID trendline')) +
  plot_formats +
  labs(title='China transport trends')



in_file = "data/real estate indicators.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, 'var', columnFilter = 'Floor Space', read_vardata = T) ->
  realestate

realestate %>% group_by(var, Unit) %>% unYTD %>% roll12m() %>% 
  filter(date>='2015-01-01', grepl('Sold|Newly', var)) -> realestate_plot


realestate_plot %>% 
  ggplot(aes(date, Value12m)) + geom_line() +
  facet_wrap(~var+Unit, scales='free_y') +
  geom_smooth(data=realestate_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='lm') +
  plot_formats +
  labs(title='Real estate trends')



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
  ggplot(aes(date, CO2_12m)) + geom_line(size=1, col=crea_palettes$dramatic[2]) +
  facet_wrap(~prod+sector, scales='free_y') +
  geom_smooth(data=energy_plot %>% filter(date<='2020-01-01'),
              aes(col='pre-COVID trendline'),
              fullrange=T, method='lm') +
  plot_formats +
  labs(title='Trends in China CO2')





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





in_file = 'data/power generation by type.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), 
           columnFilter = "Electricity Production|Thermal|Hydro|Nuclear|Solar|Wind", read_vardata = T) %>% 
  filter(!is.na(Value)) -> pwr

pwr %>% 
  group_by(var, prod, type) %>%
  unYTD() %>% filter(Value1m>0) %>% roll12m %>% 
  filter(date>='2017-01-01') ->
  pwr_plot


pwr_plot %>% 
  ggplot(aes(date, Value12m)) + geom_line() +
  facet_wrap(~prod, scales='free_y') +
  geom_smooth(data=pwr_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='lm') +
  geom_vline(aes(xintercept=ymd('2020-01-29'), linetype='First COVID-19 lockdown'))

#capacity was up 3.36%

pwr_plot %>% filter(grepl('Hydro', prod), year(date) %in% 2021:2022) %>% 
  mutate(Value1m = Value1m * ifelse(year(date)==2021, 1+3.36e-2, 1)) %>% 
  group_by(month(date)) %>% 
  summarise(output_max = max(Value1m),
            output_2022 = Value1m[year(date)==2022]) %>% 
  summarise(across(starts_with('output'), sum)) %>% 
  mutate(change=output_2022-output_max)


pwr_plot %>% filter(grepl('Electri|Hydro', prod), year(date) %in% 2019:2022) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  ggplot(aes(month, Value1m, col=as.factor(year))) + geom_line() + facet_wrap(~prod, scales='free_y')
  
pwr_plot %>% filter(grepl('Electri', prod), year(date) %in% 2021:2022) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, summer=month %in% 7:8) %>% 
  summarise(across(Value1m, mean)) %>% 
  ungroup %>% 
  summarise(summer_effect=Value1m[summer & year==2022] - 
              Value1m[summer & year==2021] * Value1m[!summer & year==2022]/Value1m[!summer & year==2021]) %>% 
  mutate(across(summer_effect, multiply_by, 2))
