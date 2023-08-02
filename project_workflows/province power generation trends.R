in_file="data/power generation by province and source.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T, columnExclude = 'Consumption', zero_as_NA = T) -> provgen
readwindEN(in_file, c('var', 'prov'), read_vardata = T, columnFilter = 'Consumption', zero_as_NA = T) %>% mutate(source=var) -> provcons


in_file="data/power capacity by province&type.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T, zero_as_NA = T) -> provcap

bind_rows(provgen, provcons) %>% 
  group_by(var, source, prov) %>% unYTD %>% roll12m() -> provpwr

provcap$source %<>% gsub(' Power.*', '', .) %>% gsub('power', '', .)
provpwr$source %<>% gsub(' Power.*', '', .) %>% gsub('power', '', .)

provcap %>% 
  select(prov, source, date, capacity=Value) %>% 
  left_join(provpwr, .) %>% 
  group_by(prov, source) %>% fill(capacity, .direction = 'down') %>% 
  group_by(prov, source, month=month(date)) %>% 
  mutate(utilization = Value1m/capacity,
         average_utilization = mean(utilization[year(date)>=2018], na.rm=T),
         Value1m_norm = Value1m * average_utilization / utilization) %>% 
  group_by(prov, source) %>% 
  roll12m(incol='Value1m_norm', outcol='Value12m_norm') ->
  provpwr_norm

provpwr_norm %>% filter(year(date)>=2012) %>% 
  ggplot(aes(date, Value12m_norm, col=source)) + geom_line() + facet_wrap(~prov, scales='free_y')

provpwr_norm %>% ungroup %>% 
  filter(month(date)==12, !grepl('Cons', source)) %>% 
  mutate(Value12m_norm = ifelse(source=='Thermal', Value12m, Value12m_norm),
         source=factor(source, levels=source %>% unique %>% (function(x) x[order(x=='Thermal', x)]))) %>% 
  group_by(prov, source) %>% 
  summarise(change = Value12m_norm[year(date)==2022]-Value12m_norm[year(date)==2020]) %>% 
  mutate(across(change, pmax, 0)) %>% 
  mutate(thermal_share=change[source=='Thermal']/sum(change, na.rm=T)) %>% 
  ungroup %>% arrange(-thermal_share) %>% mutate(prov=factor(prov, unique(prov))) %>% 
  ggplot(aes(prov, change, fill=source)) + geom_col(position='fill') + coord_flip() +
  scale_y_continuous(expand=expansion(), labels=scales::percent) +
  scale_fill_manual(values=fuel_cols, guide=guide_legend(nrow=1, reverse=T), name='') +
  theme_crea(legend.position='top', plot.margin=unit(c(1.5, 2, .2, .2), 'line')) + 
  labs(title='Shares of power generation sources in growth from 2020 to 2022', x='', y='') ->
  plt
quicksave(file.path(output_dir, 'Shares of power generation sources in growth from 2020 to 2022.png'), width=10, height=7.5, dpi=200, plot=plt)


provpwr_norm %>% ungroup %>% 
  filter(month(date)==12, grepl('Thermal', source)) %>% 
  group_by(prov, source) %>% 
  summarise(change = Value12m[year(date)==2022]-Value12m[year(date)==2020]) %>% 
  ungroup %>% arrange(-change) %>% mutate(prov=factor(prov, unique(prov))) %>% 
  ggplot(aes(prov, change*12/1e5, fill=change)) + geom_col() + coord_flip() +
  theme_crea() +
  labs(title='Changes in thermal power generation by province from 2020 to 2022', x='', y='TWh') +
  scale_fill_crea_c('change', guide='none', bias=.5) -> plt
quicksave(file.path(output_dir, 'Changes in thermal power generation by province from 2020 to 2022.png'), width=10, height=7.5, dpi=200, plot=plt)
