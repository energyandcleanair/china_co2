require(tidyverse); require(magrittr); require(creahelpers); require(readxl)


read_bp('G:/My Drive/CO2data/BPreviews/Statistical Review of World Energy Data.xlsx', 'coal consumption') -> coalcons

base_year=2017
coalcons %>% 
  group_by(country, is.country) %>% 
  summarise(change_abs=value[year==max(year)]-value[year==base_year],
            change_perc=(value[year==max(year)]/value[year==base_year])^(1/(max(year)-base_year))-1) %>% 
  arrange(desc(change_abs)) %>% filter(is.country) ->
  ranking

ranking %>%
  filter(change_abs>.1) %>% 
  #mutate(country=paste0(country, ' (', scales::percent(change_perc), ')')) %>% 
  mutate(country=paste0(country, ' (', round(change_abs/29.3*1e3,0), ' Mt)')) %>% 
  use_series(country) %>% paste(collapse=', ')

ranking %>% 
  group_by(is_China = country=='China',
           increased=change_abs>0) %>% 
  summarise(across(change_abs, sum))
