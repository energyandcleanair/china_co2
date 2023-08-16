output_dir="~/RPackages/gcpt-analysis/outputs/2023H1_analysis"

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

provpwr_norm %>% ungroup %>%
  filter(date %in% c(ymd('2020-12-31'), max(date)), !grepl('Cons', source)) %>%
  mutate(Value12m_norm = ifelse(source=='Thermal', Value12m, Value12m_norm),
         source=factor(source, levels=source %>% unique %>% (function(x) x[order(x=='Thermal', x)]))) %>%
  group_by(prov, source) %>%
  summarise(change = Value12m_norm[date==max(date)]-Value12m_norm[year(date)==2020]) %>%
  mutate(across(change, ~pmax(.x, 0))) %>%
  mutate(thermal_share=change[source=='Thermal']/sum(change, na.rm=T)) %>%
  ungroup %>% arrange(-thermal_share) %>% mutate(prov=factor(prov, unique(prov))) -> growth_mix_plot

growth_mix_plot %>% filter(source=='Thermal') %>% data.frame

growth_mix_plot %>%
  ggplot(aes(prov, change, fill=source)) + geom_col(position='fill') + coord_flip() +
  scale_y_continuous(expand=expansion(), labels=scales::percent) +
  scale_fill_manual(values=fuel_cols, guide=guide_legend(nrow=1, reverse=T), name='') +
  theme_crea(legend.position='top', plot.margin=unit(c(1.5, 2, .2, .2), 'line')) +
  labs(title='Shares of power generation sources in growth from 2020 to present', x='', y='',
       caption=paste('The present period refers to the most recent 12 months of data, up to', format(max(provpwr_norm$date), '%B %Y'))) ->
  plt
quicksave(file.path(output_dir,
                    paste0('Shares of power generation sources in growth from 2020 to ',
                           format(max(provpwr_norm$date), '%B %Y'),'.png')), plot=plt, scale=1.1)


provpwr_norm %>% ungroup %>%
  filter(date %in% c(ymd('2020-12-31'), max(date)), source=='Thermal') %>%
  group_by(prov, source) %>%
  summarise(change = Value12m[date==max(date)]-Value12m[year(date)==2020]) %>%
  ungroup %>% arrange(-change) %>% mutate(prov=factor(prov, unique(prov))) %>%
  ggplot(aes(prov, change*12/1e5, fill=change)) + geom_col() + coord_flip() +
  theme_crea() +
  labs(title='Changes in thermal power generation by province from 2020 to present', x='', y='TWh',
       caption=paste('The present period refers to the most recent 12 months of data, up to', format(max(provpwr_norm$date), '%B %Y'))) +
  scale_fill_crea_c('change', guide='none', bias=.5) -> plt
quicksave(file.path(output_dir,
                    paste0('Changes in thermal power generation by province from 2020 to ',
                           format(max(provpwr_norm$date), '%B %Y'),'.png')), scale=1.1, plot=plt)
