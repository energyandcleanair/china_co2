source('load_package.R')

require(tidyverse)
require(magrittr)
require(lubridate)
require(chinatracker)

focus_month <- ymd('2023-12-01')

aq_long <- get_aq(start_date=ymd('2017-01-01'),
                  update_data=T,
                  aq_file='cache/capitals_air_quality_data.RDS',
                  cities=china_admin_capitals)

aq_long %>% filter(pollutant=='pm25') %>%
  add_location_names(country='CN') %>%
  group_by(city_name, month=date %>% 'day<-'(1), pollutant) %>%
  summarise(across(value, mean)) %>%
  filter(month(month)==month(focus_month)) ->
  aq_focus_month

aq_focus_month %>% group_by(city_name) %>% filter(value[year(month)==2023]>max(value[year(month) %in% 2020:2022])) %>%
  use_series(city_name) %>% unique %>% paste(collapse=', ')
