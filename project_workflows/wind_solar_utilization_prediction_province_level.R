#the workflow here could use a bit of TLC -- when predict_solar_wind_utilization() works fully off of province data,
#the pwr_data_monthly argument won't be needed

pwr_data <- read_power_generation()

predict_solar_wind_utilization(pwr_data_monthly=pwr_data$monthly, output_full_data=T) -> pred
provdata_w_met <- readRDS(file.path('outputs', 'wind_solar_prediction_province_data.RDS')) %>%
  filter(!is.na(date))



provdata_w_met %>% filter(source=='Solar', Utilization>0) %>%
  lm(Utilization~(solar_radiation+temperature+date):prov, data=.) ->
  m_solar

m_solar %>% summary

provdata_w_met %>% filter(source=='Wind', Utilization>0) %>%
  lm(Utilization~(wind_speed+wind_speed_cubed+temperature+date):prov, data=.) -> m_wind

m_wind %>% summary

provdata_w_met %<>% mutate(Utilization_pred=case_when(source=='Wind'~predict(m_wind, .),
                                                      source=='Solar'~predict(m_solar, .)))

provdata_w_met %>% group_by(date, source) %>%
  summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
  group_by(source, month(date)) %>%
  mutate(across(is.numeric, list(yoy=~.x/lag(.x)-1)),
         error=Utilization-Utilization_pred,
         yoy_error=Utilization_yoy-Utilization_pred_yoy) %>% ungroup ->
  provdata_agg


