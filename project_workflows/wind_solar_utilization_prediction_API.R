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
inner_join(met_for_model %>% spread(variable, value),
           provdata %>% ungroup %>%
             select(date, Value1m, var, source, prov) %>%
             spread(var, Value1m) %>%
             filter(!is.na(Capacity))) %>%
  mutate(is_sane=Utilization>30*24*0.03 & Utilization<30*24*0.35) -> alldata

alldata %>% filter(source=='Solar', is_sane) %>%
  lm(Utilization~(solar_radiation+temperature):prov, data=.) %>% summary

alldata %>% filter(source=='Solar', is_sane) %>%
  ggplot(aes(solar_radiation, Utilization)) + geom_point()

alldata %>% filter(source=='Wind', is_sane) %>%
  lm(Utilization~wind_speed:prov+temperature, data=.) %>% summary

alldata %>% filter(source=='Wind', is_sane) %>%
  ggplot(aes(wind_speed, Utilization)) + geom_point()

alldata %>%
  group_by(source, date) %>%
  summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
  select(-Capacity, -Utilization) ->
  national_data

pwr_data$monthly %>% filter(source %in% c('Wind','Solar'), var=='Utilization') %>%
  ungroup %>% select(date, source, Utilization=Value1m) %>%
  inner_join(national_data) ->
  national_data


national_data %>% filter(source=='Solar') %>%
  lm(Utilization~solar_radiation+temperature, data=.) %>% summary

national_data %>% filter(source=='Wind') %>%
  lm(Utilization~wind_speed_cubed+temperature, data=.) %>% summary

national_data %>% filter(source=='Wind', wind_speed<10) %>%
  ggplot(aes(wind_speed_cubed, Utilization)) + geom_point()


list.files('inst/extdata/wind_solar_utilization_prediction', pattern='\\.csv$', full.names = T) %>%
  lapply(read_csv, skip=7) %>% Reduce(full_join, .) %>%
  rename(wind_speed=contains('speed'),
         solar_radiation=contains('SWGNT'),
         precipitation=contains('PRECTOTLAND'),
         cloud_fraction=contains('CLDTOT'),
         AOD=contains('Aerosol_Optical_Depth'),
         temperature=contains('TLML'),
         date=time) %>%
  mutate(date=date %>% date) -> met

pwr_data$monthly %>% filter(source %in% c('Wind','Solar'), var=='Utilization') %>%
  mutate(date=date %>% 'day<-'(1), source=paste0(tolower(source), '_utilization')) %>%
  ungroup %>% select(date, source, Value1m) %>% spread(source, Value1m) %>%
  inner_join(met) ->
  alldata

alldata %>% ggplot(aes(wind_speed, wind_utilization)) + geom_point() + geom_smooth()
alldata %>% ggplot(aes(month(date), wind_utilization, col=year(date))) + geom_point() + geom_smooth()

alldata %>% lm(wind_utilization~wind_speed*temperature, data=.) -> m_ws

m_ws %>% summary

alldata %>% lm(wind_utilization~as.factor(month(date)), data=.) %>% summary

predict(m_ws, alldata) -> alldata$wind_utilization_predicted

alldata %>% filter(!is.na(wind_utilization)) %>%
  ggplot(aes(wind_utilization_predicted, wind_utilization)) +
  geom_point(aes(col=as.factor(year(date)))) + geom_smooth(method='lm')


alldata %>% filter(month(date)<=6) %>%
  group_by(year=year(date)) %>%
  select(contains('wind'), temperature) %>%
  summarise(across(where(is.numeric), sum))


met %>% pivot_longer(-date) %>%
  inner_join(alldata %>% select(date, solar_utilization)) %>% na.omit %>%
  ggplot(aes(value, solar_utilization)) + geom_point() + facet_wrap(~name, scales='free_x')

alldata %>% ggplot(aes(month(date), solar_utilization, col=year(date))) + geom_point() + geom_smooth()

alldata %>%
  lm(solar_utilization~
       solar_radiation+
       temperature+
       solar_radiation:temperature+
       cloud_fraction+
       precipitation+
       AOD, #AOD is unhelpful on the national aggregate level but should be tested on province level
     data=.) -> m_sr

m_sr %>% summary

alldata %>% lm(solar_utilization~as.factor(month(date)), data=.) %>% summary

predict(m_sr, alldata) -> alldata$solar_utilization_predicted

alldata %>% filter(!is.na(solar_utilization)) %>%
  ggplot(aes(solar_utilization, solar_utilization_predicted)) +
  geom_point(aes(col=as.factor(year(date)))) + geom_smooth()
