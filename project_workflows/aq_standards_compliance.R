aq_capitals <- get_aq(start_date=ymd('2017-01-01'),
                      update_data=T,
                      aq_file='cache/province_capital_air_quality_data.RDS',
                      country='CN',
                      cities=china_admin_capitals,
                      source='mee')

aq_all %<>% group_by(across(c(where(is.character))), year=year(date))

quant_na_as_na <- function(x, ...) {
  if(any(is.na(x))) { return(NA)
  } else return(quantile(x, ...))
}

get_aq_mean <- function(df, na.rm=F) {
  df %>%
    group_modify(function(data, group) {
      if(group$pollutant != 'o3') fun = function(x) mean(x, na.rm=na.rm)
      if(group$pollutant == 'o3') fun = function(x) quant_na_as_na(x, probs=.9, na.rm=na.rm)
      data %>% summarise(across(value, fun))
    })
}

aq_all %>% filter(year<year(focus_month)) %>% get_aq_mean -> aq_by_year

aq_all %>%
  group_by(across(c(where(is.character))), year=year(date)) %>%
  filter(year>=year(focus_month)-1, month(date)<=month(focus_month)) %>%
  get_aq_mean %>% ungroup(year) %>%
  summarise(yoy_ytd = value[year==year(focus_month)]/value[year==year(focus_month)-1]) -> aq_ytd

aq_by_year %<>% filter(year==year(focus_month)-1) %>% full_join(aq_ytd) %>%
  mutate(value=value*yoy_ytd, year=year(focus_month)) %>%
  bind_rows(aq_by_year)

aqs = tibble(pollutant=c('pm25', 'no2', 'o3'), aqs=c(35,40,160))

aq_by_year %>% ungroup(year) %>% right_join(aqs) %>%
  filter(location_id %in% china_admin_capitals, !is.na(value)) %>%
  summarise(compliance_status = paste0(ifelse(value[year==2022]<aqs, '', 'not '),
                                       'compliant in 2022 and ',
                                       ifelse(value[year==2023]<aqs, '', 'not '),
                                       'on track to comply')) %>%
  distinct %>%
  group_by(compliance_status, pollutant) %>% tally
  #filter(compliance_status=='compliant in 2022 and not on track to comply', pollutant=='pm25')

rollmean_date <- function(x, dates, width=7) {
  x.out <- x
  x.out[] <- NA
  for(i in 1:length(x))
    x.out[i] <- sum(x[dates %in% (dates[i]-0:(width-1))], na.rm=T)/width
  return(x.out)
}

aq_capitals %<>% add_location_names(country = 'CN')

aq_capitals %>% filter(location_id %in% china_admin_capitals) %>%
  right_join(aqs) %>%
  group_by(across(where(is.character))) %>%
  mutate(value_12m = case_when(pollutant!='o3'~value %>% pmin(500) %>% rollapplyr(365, mean, fill=NA),
                               T~value %>% rollapplyr(365, quantile, probs=.9, fill=NA))) ->
  aq_capitals_12m

aq_capitals_12m %<>% mutate(city_label = case_when(city_name_EN==NAME_1_EN~city_name_EN, T~paste0(city_name_EN, ', ', NAME_1_EN)))

aq_capitals_12m %>%
  ungroup %>% filter(!is.na(value_12m), pollutant=='pm25', year(date)>=2019) %>%
  ggplot(aes(date, value_12m)) +
  geom_line(aes(col=value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
            linewidth=.75) +
  facet_wrap(~city_label) +
  geom_hline(aes(linetype='National air quality standard', yintercept = 35), alpha=.5) +
  theme_crea(legend.position='top') +
  scale_color_crea_c('change', guide='none') +
  labs(title='PM2.5 concentrations in provincial capitals',
       x='', y='µg/m3',
       subtitle='12-month moving average') +
  scale_linetype_manual(values='dotted', name='')


aq_capitals_12m %>%
  ungroup %>% filter(!is.na(value_12m), pollutant=='o3', year(date)>=2019) %>%
  ggplot(aes(date, value_12m)) +
  geom_line(aes(col=value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
            linewidth=.75) +
  facet_wrap(~city_label) +
  geom_hline(aes(linetype='National air quality standard', yintercept = 160), alpha=.5) +
  theme_crea(legend.position='top') +
  scale_color_crea_c('change', guide='none') +
  labs(title='Ozone concentrations in provincial capitals',
       x='', y='µg/m3, 90th percentile of daily 8-hour maximum',
       subtitle='12-month moving window') +
  scale_linetype_manual(values='dotted', name='')




aq_capitals_12m %>% ungroup %>% filter(!is.na(value_12m), pollutant=='o3') %>%
  ggplot(aes(date, value_12m)) + geom_line(aes(col=value_12m<aqs)) + facet_wrap(~location_id) +
  geom_hline(yintercept = 160, col='darkred', alpha=.5)

aq_capitals_12m %>% ungroup %>% filter(grepl('beijing', location_id), pollutant=='pm25') %>%
  group_by(location_id, pollutant, month=date %>% 'day<-'(1)) %>%
  summarise(across(value, mean)) %>% tail(24) %>% View()
