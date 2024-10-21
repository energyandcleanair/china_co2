require(tidyverse)
require(magrittr)
require(chinatracker)

#calculate national averages from province data
paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       '&region_type=gadm1',
       #'&region_id=CN',
       '&region_iso2=CN') %>% read_csv ->
  met_prov


pop <- read_csv(get_data_file('population_by_province_2022.csv'))

met_prov %>% mutate(date = date %>% 'day<-'(days_in_month(.)),
                    region_name=fix_province_names(region_name)) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  met_prov_monthly

met_prov_monthly %>% left_join(pop) %>%
  group_by(variable, date) %>%
  summarise(across(value, list(value=~weighted.mean(.x, population_total),
                               urban=~weighted.mean(.x, population_urban)))) %>%
  rename(value=value_value) %>%
  mutate(aggregation='Lauri') ->
  met_natl_monthly

#calculate national averages from national data
paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       #'&region_type=gadm1',
       '&region_id=CN') %>% read_csv ->
  met_natl

met_natl %>% mutate(date = date %>% 'day<-'(days_in_month(.))) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) %>%
  mutate(aggregation='API') ->
  met_natl_monthly2


#plot
bind_rows(met_natl_monthly2, met_natl_monthly) %>%
  filter(variable!='temperature') %>%
  ungroup %>% select(aggregation, value, variable, date) %>%
  spread(aggregation, value) %>%
  ggplot(aes(API, Lauri, col=date=='2024-09-30')) + geom_point() + geom_abline() + facet_wrap(~variable, scales='free_y')
