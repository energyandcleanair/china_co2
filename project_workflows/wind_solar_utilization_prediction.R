source('load_package.R')

pwr_data <- read_power_generation()

read_csv('outputs/power_data.csv') -> pwr_growth_plot


pwr_growth_plot %>%
  filter(!is.na(broad_label), year(date)==2024, month(date)>0) %>%
  group_by(broad_label) %>%
  summarise(across(c(value, YoY_change_absolute_1m), sum)) %>%
  mutate(share=YoY_change_absolute_1m/YoY_change_absolute_1m[broad_label=='Total'])

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

alldata %>% ggplot(aes(wind_utilization, wind_utilization_predicted)) + geom_point() + geom_smooth()


alldata %>% filter(month(date)<=6) %>%
  group_by(year=year(date)) %>%
  select(contains('wind'), temperature) %>%
  summarise(across(where(is.numeric), sum))


met %>% pivot_longer(-date) %>%
  inner_join(alldata %>% select(date, solar_utilization)) %>% na.omit %>%
  ggplot(aes(value, solar_utilization)) + geom_point() + facet_wrap(~name, scales='free_x')

sr %>% ggplot(aes(month(date), value, col=year(date))) + geom_point() + geom_smooth()

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

alldata %>% ggplot(aes(solar_utilization, solar_utilization_predicted)) + geom_point() + geom_smooth()
