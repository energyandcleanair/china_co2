source('scripts/load_package.R')
require(units)
require(sf)
#Sys.setenv(GIS_DIR='~/GIS')

output_dir <- 'outputs/November2024_anomaly'
dir.create(output_dir)

read_power_generation(predict_solar_wind = F) -> pwr_data
read_power_generation(predict_solar_wind = T) -> pwr_data_w_pred

source('scripts/load_province_generation_data.R')
save.image(file.path(output_dir, 'alldata.RData'))
load(file.path(output_dir, 'alldata.RData'))



pwr_data$monthly %>% ungroup %>%
  filter(grepl('Generation$|hybrid|Consumption', var), grepl('Total|Whole', source)) %>%
  group_by(date) %>%
  mutate(Value1m=case_when(var=='Consumption'~Value1m - Value1m[var=='Generation']*0.99,
                           var=='Generation, hybrid'~Value1m - Value1m[var=='Generation']),
         var=case_when(var=='Consumption'~'implied "small scale" generation',
                       var=='Generation, hybrid'~'estimated "small scale" generation')) %>%
  mutate(Value_select_months=ifelse(month(date) %in% 3:11, Value1m, NA)) %>%
  filter(!is.na(var)) ->
  small_scale

small_scale %>% filter(month(date)==11) %>% group_by(var, month(date)) %>% mutate(change=Value1m-lag(Value1m))

plot_start_date = '2020-01-01'

ggplot(mapping=aes(date, Value_select_months/10, col=var, fill=var)) +
  geom_col(data=small_scale %>% filter(date>=plot_start_date, grepl('implied', var))) +
  geom_line(data=small_scale %>% filter(date>=plot_start_date, grepl('estimated', var)))

pwr_data_w_pred$monthly %>% ungroup %>%
  filter(grepl('Generation', var),
         date %in% c('2024-11-30', '2023-11-30'), source %in% c('Wind', 'Solar')) %>%
  group_by(var, date) %>%
  summarise(across(Value1m, sum)) %>%
  group_by(var, month(date)) %>%
  mutate(change=Value1m-lag(Value1m))

###FINAL GRAPH
pwr_data$monthly %>%
  filter(grepl('hybrid|Consumption', var), grepl('Total|Whole', source), year(date)>=2022) %>%
  mutate(date=case_when(month(date) %in% 1:2~date %>% 'month<-'(2) %>% 'day<-'(14), T~date),
         var=var %>% gsub(',.*', '', .)) %>%
  group_by(var, date) %>% summarise(across(Value1m, mean)) %>%
  ggplot(aes(date, Value1m/10, col=var)) + geom_line(linewidth=1) + x_at_zero() +
  theme_crea_new() + scale_color_crea_d('dramatic') +
  labs(y='TWh', title='Monthly power generation and consumption', x='')
quicksave(file.path(output_dir, 'Monthly power generation and consumption.png'))




pwr_data$monthly %>%
  filter(grepl('Generation$|Consumption', var), grepl('Total|Whole', source), year(date)>=2020) %>%
  mutate(date=case_when(month(date) %in% 1:2~date %>% 'month<-'(2) %>% 'day<-'(14), T~date)) %>%
  group_by(var, date) %>% summarise(across(Value1m, mean)) %>%
  group_by(var, month(date)) %>% mutate(yoy=Value1m/lag(Value1m)-1) %>%
  #group_by(date) %>% summarise(difference=yoy[var=='Consumption']-yoy[var=='Generation']) %>%
  ggplot(aes(date, yoy, col=var)) + geom_line() + x_at_zero()









###province data
predict_solar_wind_utilization(pwr_data_monthly=pwr_data$monthly, output_full_data=T) -> pred
provdata_w_met <- readRDS(file.path(output_dir, 'wind_solar_prediction_province_data.RDS'))



provdata_filled %<>%
  group_by(var, prov, source, variant, month(date)) %>%
  mutate(yoy=Value1m/lag(Value1m)-1,
         anomaly=Value1m/mean(Value1m[year(date) %in% 2019:2024])-1) %>% ungroup

provdata_filled %>%
  filter(var=='Utilization', variant=='Actual utilization', source %in% c('Solar', 'Wind', 'Hydro')) %>%
  select(prov, date, source, yoy, anomaly) %>%
  pivot_longer(c(yoy, anomaly)) %>%
  filter(date==max(date), abs(value)<1) %>%
  ggplot(aes(prov, value, col=name)) + geom_point() + coord_flip() + facet_wrap(~source)


provdata_filled %>%
  filter(var=='Utilization', variant=='Actual utilization', source %in% c('Solar', 'Wind'),
         month(date)==month(max(date))) ->
  utilization

provdata_w_met %>% select(source, prov, date, Value1m=Utilization_pred) %>%
  mutate(value_type='predicted') %>%
  bind_rows(utilization %>% mutate(value_type='reported')) -> utilization


###FINAL GRAPH
utilization %>% filter(date<max(date), value_type=='reported') %>%
  ggplot(aes(prov, Value1m)) +
  geom_violin(aes(col='Historical range')) +
  geom_point(data=utilization %>% filter(date==max(date)), aes(col=paste('2024', value_type)),
             size=2) +
  coord_flip() +
  facet_wrap(~source, scales='free_x') +
  scale_x_discrete(limits=rev) +
  scale_color_crea_d('dramatic') +
  labs(title='Solar and wind utilization in November',
       x='', y='hours', color='') +
  theme_crea_new()
quicksave(file.path(output_dir, 'Solar and wind utilization in November, spread.png'))



###FINAL GRAPH
utilization %>%
  filter(month(date)==month(max(date))) %>%
  group_by(prov, source, Value1m) %>% filter(n()==1) %>%
  ggplot(aes(year(date), Value1m, col=source, linetype=value_type)) +
  geom_line(linewidth=.4) + facet_wrap(~prov, scales='free_y') +
  x_at_zero() + scale_x_continuous(breaks=seq(2012, 2024, 4)) +
  scale_linetype_manual(values=c('dashed', 'solid')) +
  labs(title='November utilization hours by province and year',
       x='', y='hours', linetype='', col='') +
  scale_color_crea_d('dramatic') +
  theme_crea_new()
quicksave(file.path(output_dir, 'Solar and wind utilization in November, time series.png'))


###FINAL GRAPH
utilization %>% select(value_type, source, prov, date, Value1m) %>% spread(value_type, Value1m) %>%
  ggplot(aes(reported, predicted, col=source)) + geom_point() + geom_abline(linetype='dashed') +
  labs(title='Model performance: reported and predicted utilization',
       x='hours', y='hours', col='') +
  scale_color_crea_d('dramatic') +
  theme_crea_new()
quicksave(file.path(output_dir, 'reported and predicted utilization.png'))


provdata_w_met %>% filter(source=='Solar', Utilization>0) %>%
  lm(Utilization~(solar_radiation+temperature+date):prov, data=.) ->
  m_solar

m_solar %>% summary

provdata_w_met %>% filter(source=='Wind', Utilization>0) %>%
  lm(Utilization~(wind_speed+wind_speed_cubed+temperature+date):prov, data=.) -> m_wind

m_wind %>% summary

provdata_w_met %<>% filter(Utilization>0) %>%
  mutate(Utilization_pred=case_when(source=='Wind'~predict(m_wind, .),
                                    source=='Solar'~predict(m_solar, .)))

provdata_w_met %>% group_by(date, source) %>%
  summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
  group_by(source, month(date)) %>%
  mutate(across(is.numeric, list(yoy=~.x/lag(.x)-1)),
         error=Utilization-Utilization_pred,
         yoy_error=Utilization_yoy-Utilization_pred_yoy) %>% ungroup ->
  provdata_agg

provdata_agg %>% filter(date==max(date)) %>% data.frame
provdata_agg %>% summary

provdata_agg %>% select(date, source, starts_with('Utilization')) %>%
  pivot_longer(starts_with('Utilization')) %>%
  filter(!grepl('yoy', name)) %>%
  group_by(date, name) %>% summarise(across(value, mean)) %>%
  ggplot(aes(date, value, col=name)) + geom_line() #+ facet_wrap(~source)


provdata_agg %>%
  #group_by(date) %>% summarise(across(c(error, yoy_error), mean)) %>%
  ggplot(aes(date, yoy_error, col=source)) + geom_line() + facet_wrap(~source)


provdata_w_met %>%
  ggplot(aes(Utilization_pred, Utilization, col=source)) + geom_point() + facet_wrap(~prov) + geom_abline()

provdata_w_met %>% filter(Utilization>0) %>%
  mutate(Utilization_pred=case_when(source=='Wind'~predict(m_wind, .),
                                    source=='Solar'~predict(m_solar, .))) %>%
  ggplot(aes(prov, Utilization-Utilization_pred, col=source)) +
  geom_violin() +
  geom_point(data=provdata_w_met %>% filter(date==max(date))) +
  coord_flip() +
  facet_wrap(~source)


national_data <- readRDS(file.path(output_dir, 'wind_solar_prediction_national_data.RDS'))

national_data %>%
  group_by(source, month(date)) %>%
  mutate(yoy=Utilization/lag(Utilization)-1) %>% ungroup %>%
  filter(date==max(date)) %>% select(source, Utilization, yoy)

national_data %>% filter(!is.na(Utilization), year(date)>2014) %>%
  arrange(date) %>%
  mutate(plotdate=date %>% 'year<-'(2022),
         year=as.factor(year(date))) %>%
  ggplot(aes(plotdate, Utilization, col=year)) + geom_line() + facet_wrap(~source)

###FINAL GRAPH
national_data %>% filter(!is.na(Utilization)) %>%
  mutate(month=factor(month.abb[month(date)], levels=month.abb)) %>%
  ggplot(aes(year(date), Utilization, col=source)) + geom_line(linewidth=1) + facet_wrap(~month) +
  scale_x_continuous(breaks=seq(2014,2024,2)) +
  labs(title='Utilization hours by year and month',
       subtitle='National average',
       x='', y='hours', col='') +
  scale_color_crea_d('dramatic') +
  theme_crea_new()
quicksave(file.path(output_dir, 'Utilization hours by year and month - national.png'))




#do yoy in solar/wind and thermal correlate???
provdata_filled %>%
  filter(var=='Utilization', variant=='Actual utilization',
         year(date)>2012, Value1m>0, month(date) %in% c(3:12)) %>%
  group_by(prov, source, Value1m) %>% filter(n()==1) %>%
  group_by(prov, source, month(date)) %>%
  mutate(anomaly=Value1m-mean(Value1m)) %>%
  group_by(prov, date) %>%
  mutate(yoy_Thermal=if_null(yoy[source=='Thermal']),
         Thermal=if_null(Value1m[source=='Thermal']),
         anomaly_Thermal=if_null(anomaly[source=='Thermal'])) %>%
  filter(source %in% c('Solar', 'Wind')) %>%
  lm(yoy~yoy_Thermal:prov:source, data=.) %>%
  summary



pwr_data$monthly %>%
  filter(var=='Utilization', is.na(subtype)) %>%
  group_by(source, month(date)) %>%
  mutate(yoy=Value1m/lag(Value1m)-1,
         anomaly=Value1m-mean(Value1m)) %>%
  group_by(date) %>%
  mutate(yoy_Thermal=if_null(yoy[source=='Thermal']),
         Thermal=if_null(Value1m[source=='Thermal']),
         anomaly_Thermal=if_null(anomaly[source=='Thermal'])) %>%
  filter(source %in% c('Solar', 'Wind')) %>%
  lm(yoy~yoy_Thermal:source, data=.) %>%
  summary



pwr_data$monthly %>%
  filter(var=='Utilization', source %in% c('Solar', 'Wind')) %>%
  group_by(source, month(date)) %>%
  mutate(yoy=Value1m/lag(Value1m)-1,
         anomaly=Value1m-mean(Value1m)) %>%
  ggplot(aes(date, yoy)) + geom_col() + facet_wrap(~source, scales='free_x')
