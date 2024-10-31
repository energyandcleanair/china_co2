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


library(ggrepel)
#plot
bind_rows(met_natl_monthly2, met_natl_monthly) %>%
  filter(variable!='temperature') %>%
  ungroup %>% select(aggregation, value, variable, date) %>%
  spread(aggregation, value) %>%
  filter(!is.na(API) & !is.na(Lauri)) %>%
  ggplot(aes(API, Lauri, col=date=='2024-09-30')) +
  geom_point() +
  # ggrepel::geom_text_repel(
  #   # only keep outliers
  #   data = function(x) x %>% filter(abs(API-Lauri) > quantile(abs(API-Lauri), 0.999)),
  #   aes(label=as.character(date))) +
  geom_abline() +
  facet_wrap(~variable, scales='free_y')



# Validation HDD with external source
# Only found EU data for now
library(eurostat)
# nrg_chddr2_a
hdd_eurostat <- get_eurostat("nrg_chddr2_m", time_format = "date")
hdd_api <- paste0('https://api.energyandcleanair.org/v1/',
                  'weather?',
                  'variable=HDD,CDD,CDD_21',
                  '&format=csv',
                  '&region_id=DE,CN,IN') %>% read_csv %>%
  mutate(source=glue("CREA {source}")) %>%
  mutate(variable=gsub("_CFSV2", "", variable))

library(chinatracker)
hdd_iea <- read_csv(get_data_file('iea_cdd21_monthly.csv')) %>%
  pivot_longer(-Country, names_to = "date") %>%
  mutate(date=as.Date(lubridate::parse_date_time(date, "b-y")),
         region_id=countrycode::countrycode(Country, "country.name", "iso2c"),
         variable="CDD_21")


bind_rows(
  hdd_eurostat %>% select(region_id=geo, date=TIME_PERIOD, variable=indic_nrg, value=values) %>% mutate(source="EUROSTAT"),
  hdd_api %>% group_by(region_id, date=lubridate::floor_date(date, "month"), source, variable) %>% summarise(value=sum(value)),
  hdd_iea %>% mutate(source="IEA")
) %>%
  filter(source!="CREA CFSV2") %>%
  filter(region_id %in% c("DE","CN", "IN")) %>%
  # filter(grepl("CDD", variable)) %>%
  filter(date >= "2015-01-01") %>%
  ggplot() +
  geom_line(aes(date, value, col=source)) +
  facet_wrap(region_id~variable, scales="free_y")

bind_rows(
  hdd_eurostat %>% select(region_id=geo, date=TIME_PERIOD, variable=indic_nrg, value=values) %>% mutate(source="EUROSTAT"),
  hdd_api %>% group_by(region_id, date=lubridate::floor_date(date, "month"), source="CREA", variable) %>% summarise(value=sum(value)),
  hdd_iea %>% mutate(source="IEA")
) %>%
  filter(region_id %in% c("CN")) %>%
  filter(grepl("CDD", variable)) %>%
  filter(date >= "2016-01-01") %>%
  filter(date < "2024-01-01") %>%
  group_by(region_id, variable, date=lubridate::floor_date(date, "year"), source) %>%
  summarise(value=sum(value)) %>%
  ggplot() +
  geom_line(aes(date, value, col=source)) +
  facet_grid(region_id~variable, scales="free_y") +
  rcrea::scale_y_crea_zero()

