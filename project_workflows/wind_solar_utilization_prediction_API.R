#wind and solar utilization prediction using data from CREA API

source('load_package.R')

pwr_data <- read_power_generation()


read_csv("https://api.energyandcleanair.org/v1/weather?region_type=gadm1&region_iso2=CN&variable=wind_speed,temperature,solar_radiation&format=csv") ->
  met_from_API

get_data_file("wind_solar_utilization_capacity_by_province.xlsx") %>%
  readwindEN(c('prov', 'var', 'source', 'source2'),
             read_vardata = T, skip=2, columnExclude = "National|China: Installed") ->
  provdata

provdata %<>% filter(!grepl('Above.*Solar', Name)) %>%
  mutate(source=disambiguate(paste(source, source2), c('Wind', 'Solar')),
                    var=disambiguate(var, c('Capacity', 'Utilization')),
         type=ifelse(var=='Capacity', 'cumulative', 'YTD')) %>%
  #calculate monthly utilization from YTD
  group_by(source, var, prov, type) %>% unYTD %>%
  mutate(Value1m=Value1m * ifelse(var=='Utilization'&month(date)==2, 2, 1)) %>%
  filter(month(date)!=1)

#add cubed wind speed
met_from_API %<>% filter(variable=='wind_speed') %>%
  mutate(value=value^3, variable='wind_speed_cubed') %>%
  bind_rows(met_from_API %>% filter(variable!='wind_speed_cubed'))


#aggregate to monthly averages and Jan-Feb to Feb
met_from_API %>%
  rename(data_source=source) %>%
  mutate(prov=region_name %>% fix_province_names(),
         date = case_when(month(date)==1~date %>% 'month<-'(2), T~date) %>%
           'day<-'(days_in_month(.))) %>%
  group_by(prov, variable, date) %>%
  summarise(across(value, mean)) %>% ungroup ->
  met_for_model

#merge datasets
left_join(met_for_model %>% spread(variable, value),
           provdata %>% ungroup %>%
             select(date, Value1m, var, source, prov) %>%
             spread(var, Value1m) %>%
             filter(!is.na(Capacity))) %>%
  mutate(is_sane=Utilization>30*24*0.03 & Utilization<30*24*0.35) -> alldata

alldata %>% filter(source=='Solar', is_sane) %>%
  lm(Utilization~(solar_radiation*temperature):prov, data=.) %>% summary

alldata %>% filter(source=='Solar', is_sane) %>%
  ggplot(aes(solar_radiation, Utilization)) + geom_point()

alldata %>% filter(source=='Wind', is_sane) %>%
  lm(Utilization~wind_speed:prov+temperature, data=.) %>% summary

alldata %>% filter(source=='Wind', is_sane) %>%
  ggplot(aes(wind_speed, Utilization)) + geom_point()

alldata %>% arrange(date) %>%
  group_by(source, prov) %>%
  fill(Capacity, .direction='down') %>%
  group_by(source, date) %>%
  summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
  select(-Capacity, -Utilization) ->
  national_data

pwr_data$monthly %>% filter(source %in% c('Wind','Solar'), var=='Utilization') %>%
  ungroup %>% select(date, source, Utilization=Value1m) %>%
  inner_join(national_data) ->
  national_data


national_data %>% filter(source=='Solar') %>%
  lm(Utilization~solar_radiation*temperature+date, data=.) ->
  m_solar

m_solar %>% summary

national_data %>% filter(source=='Wind') %>%
  lm(Utilization~wind_speed_cubed+temperature+date, data=.) -> m_wind

m_wind %>% summary

national_data %>% filter(source=='Wind') %>%
  ggplot(aes(wind_speed_cubed, Utilization)) + geom_point()


national_data %<>% ungroup %>%
  mutate(Utilization_predicted = case_when(source=='Wind'~predict(m_wind, national_data),
                                           source=='Solar'~predict(m_solar, national_data))) %>%
  group_by(month(date), source) %>%
  mutate(Utilization_YoY = Utilization/lag(Utilization)-1,
         Utilization_predicted_YoY = Utilization_predicted/lag(Utilization_predicted)-1)


national_data %>% filter(Utilization_YoY<.35) %>%
  ggplot(aes(Utilization_YoY, Utilization_predicted_YoY)) + geom_point() + facet_wrap(~source) +
  geom_smooth(method='lm') + geom_abline()
