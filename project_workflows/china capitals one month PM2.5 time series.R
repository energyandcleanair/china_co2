aq_long <- get_aq(start_date=ymd('2017-01-01'),
                  update_data=T,
                  aq_file='cache/capitals_air_quality_data.RDS',
                  cities=china_admin_capitals)


aq_long %>% filter(pollutant=='pm25') %>% add_location_names(country=country, lang=lang) -> aq_long_wnames

aq_long_wnames %>% #rename(city_name=city_id) %>%
  group_by(city_name, month=date %>% 'day<-'(1), pollutant) %>%
  summarise(across(value, mean)) %>%
  filter(pollutant=='pm25', month <= focus_month, month(month)==month(focus_month)) %>%
  group_by(city_name) %>% mutate(rank=rank(value)) %>%
  ggplot(aes(year(month), value, fill=rank)) + geom_col() + facet_wrap(~city_name) +
  coord_cartesian(ylim=c(0,100)) +
  labs(title='December average PM2.5 levels in province capitals', x='', y='ug/m3') +
  theme_crea() + scale_fill_crea_c('change', guide='none', col.index = 5:7) +
  x_at_zero()



aq_long_wnames %>% #rename(city_name=city_id) %>%
  group_by(city_name, month=date %>% 'day<-'(1), pollutant) %>%
  summarise(across(value, mean)) %>%
  filter(pollutant=='pm25', month <= focus_month, month(month)==month(focus_month)) %>%
  group_by(city_name) %>% filter(value[year(month)==2023]>max(value[year(month) %in% 2020:2022])) %>%
  use_series(city_name) %>% unique %>% paste(collapse=', ')
