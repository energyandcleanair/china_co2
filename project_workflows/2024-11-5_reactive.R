library(rcrea)
library(glue)
library(tidyverse)

key_cities <- c(
  "beijing_chn.2_1_cn",
  "tianjin_chn.27_1_cn",
  "shijiazhuang_chn.10_1_cn",
  "tangshan_chn.10_1_cn",
  "qinhuangdao_chn.10_1_cn",
  "handan_chn.10_1_cn",
  "xingtai_chn.10_1_cn",
  "baoding_chn.10_1_cn",
  "cangzhou_chn.10_1_cn",
  "langfang_chn.10_1_cn",
  "hengshui_chn.10_1_cn",
  "jinan_chn.23_1_cn",
  "zibo_chn.23_1_cn",
  "zaozhuang_chn.23_1_cn",
  "dongying_chn.23_1_cn",
  "weifang_chn.23_1_cn",
  "jining_chn.23_1_cn",
  "tai'an_chn.23_1_cn",
  "rizhao_chn.23_1_cn",
  "linyi_chn.23_1_cn",
  "dezhou_chn.23_1_cn",
  "liaocheng_chn.23_1_cn",
  "binzhou_chn.23_1_cn",
  "heze_chn.23_1_cn",
  "zhengzhou_chn.12_1_cn",
  "kaifeng_chn.12_1_cn",
  "luoyang_chn.12_1_cn",
  "pingdingshan_chn.12_1_cn",
  "anyang_chn.12_1_cn",
  "hebi_chn.12_1_cn",
  "xinxiang_chn.12_1_cn",
  "jiaozuo_chn.12_1_cn",
  "puyang_chn.12_1_cn",
  "xuchang_chn.12_1_cn",
  "luohe_chn.12_1_cn",
  "sanmenxia_chn.12_1_cn",
  "shangqiu_chn.12_1_cn",
  "zhoukou_chn.12_1_cn"
)

capital_cities <- c(
  "beijing_chn.2_1_cn",
  "tianjin_chn.27_1_cn",
  "shijiazhuang_chn.10_1_cn",
  "jinan_chn.23_1_cn",
  "zhengzhou_chn.12_1_cn"
)


labels_dict <- c(
  'change' = 'yoy change',
  'change_weather' = 'influence of weather',
  'change_emisson' = 'due to changes in emission'
)


labels_dict_ZH <- c(
  'change' = '同比变化',
  'change_weather' = '受天气影响',
  'change_emisson' = '受排放量变化影响'
)

prov_dict_ZH <- c(
  'Beijing' = '北京',
  'Tianjin' = '天津',
  'Hebei' = '河北',
  'Shandong' = '山东',
  'Henan' = '河南'
)

cities_match <- read_csv('data/city_list.csv')


get_aq <- function(cities){
  meas_raw <- rcrea::measurements(poll = 'pm25',
                                  date_from = '2024-10-23',
                                  date_to = '2024-11-03',
                                  source = 'mee',
                                  deweathered = F,
                                  process_id = 'city_day_mad',
                                  with_metadata = F,
                                  with_geometry = F) %>%
    collect() %>%
    filter(location_id %in% cities)

  return(meas_raw)
}


get_aq_deweather <- function(cities, date_from, date_to){
  base_url <- paste0('http://api.energyandcleanair.org/v1/measurements?',
                     'pollutant={pol}&location_id={cities}&source={source}',
                     '&date_from={date_from}&date_to={date_to}',
                     '&process=default_anomaly_2018_2099',
                     '&variable=anomaly,predicted,observed&format=csv')

  anomaly_raw <- lapply(seq(1, length(cities), 5), function(x){
    max <- min(x + 4, length(cities))

    cities_sub <- cities[x:max]

    print(glue('getting data for city {x} to {max}'))

    anomaly_data <- read_csv_and_retry(glue(base_url,
                                            pol = 'pm25', source = 'mee',
                                            date_from = date_from,
                                            date_to = date_to,
                                            cities = paste0(cities_sub, collapse = ','))) %>%
      filter(process_id == 'default_anomaly_2018_2099')
  }) %>% bind_rows()

  return(anomaly_raw)
}


aq_deweather <- get_aq_deweather(key_cities, date_from = '2024-10-23', date_to = '2024-10-30')
aq_deweather_prev <- get_aq_deweather(key_cities, date_from = '2023-10-23', date_to = '2023-10-30')


# sanity checks
aq_deweather %>% group_by(location_id, variable) %>%
  summarise(count = n()) %>%
  filter(count < 8)
aq_deweather_prev %>% group_by(location_id, variable) %>%
  summarise(count = n()) %>%
  filter(count < 8)


# all data - wide
aq_deweather_wide <- aq_deweather %>% mutate(month = month(date), day = mday(date)) %>%
  left_join(aq_deweather_prev %>% mutate(month = month(date), day = mday(date)) %>%
              select('location_id', 'pollutant', 'variable', 'month', 'day', 'value'),
            by = c('location_id', 'pollutant', 'variable', 'month', 'day'),
            suffix = c('', '_prev'))


# only Beijing, Tianjin, Hebei
aq_avg_3 <- aq_deweather_wide %>%
  filter(gadm1_name %in% c('Beijing', 'Tianjin', 'Hebei')) %>%
  group_by(date, pollutant, variable, prov = gadm1_name) %>% # average by date
  summarise(value = mean(value, na.rm = T),
            value_prev = mean(value_prev, na.rm = T)) %>%
  group_by(pollutant, variable, prov) %>% # average by province
  summarise(value = mean(value, na.rm = T),
            value_prev = mean(value_prev, na.rm = T)) %>%
  group_by(pollutant, variable) %>% # average the period
  summarise(value = mean(value, na.rm = T),
            value_prev = mean(value_prev, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = variable, values_from = c(value, value_prev),
              names_glue = '{variable}_{.value}') %>%
  mutate(change = (observed_value - observed_value_prev) / observed_value_prev,
         change_weather = (predicted_value - predicted_value_prev) / observed_value_prev,
         change_emisson = change - change_weather) %>%
  select(pollutant, change, change_weather, change_emisson) %>%
  pivot_longer(cols = -pollutant, names_to = 'variable', values_to = 'value') %>%
  mutate(variable_labels = factor(labels_dict[variable], levels = labels_dict),
         variable_labels_ZH = factor(labels_dict_ZH[variable], levels = labels_dict_ZH))
# mutate(labels = labels[variable]) %>%
# pivot_wider(names_from = variable, values_from = value)


p <- aq_avg_3 %>%
  ggplot(mapping = aes(pollutant, value)) +
  geom_col(data = aq_avg_3 %>% filter(variable != 'change'),
           aes(fill = variable_labels)) +
  geom_point(data = aq_avg_3 %>% filter(variable == 'change'),
             aes(shape = 'circle'), size = 3) +
  coord_flip() +
  theme_crea() +
  scale_fill_crea_d('change', col.index = c(5,2), name = '') +
  scale_shape_discrete(name='', labels = 'yoy change') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = 'Average PM2.5 concentration in Beijing, Tianjin and Hebei cities',
       subtitle = '23 - 30 October 2024. Unit: µg/m³',
       x = '',
       y = '')
quicksave('average PM2.5 yoy EN.png', plot = p)

p <- aq_avg_3 %>%
  ggplot(mapping = aes(pollutant, value)) +
  geom_col(data = aq_avg_3 %>% filter(variable != 'change'),
           aes(fill = variable_labels_ZH)) +
  geom_point(data = aq_avg_3 %>% filter(variable == 'change'),
             aes(shape = 'circle'), size = 3) +
  coord_flip() +
  theme_crea() +
  scale_fill_crea_d('change', col.index = c(5,2), name = '') +
  scale_shape_discrete(name='', labels = '同比变化') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = '京津冀PM2.5同比变化',
       subtitle = '10月23日至30日，微克/立方米',
       x = '',
       y = '')
quicksave('average PM2.5 yoy ZH.png', plot = p)

write_csv(aq_avg_3, 'average_pm25_yoy.csv')


# provincial average
prov_average <- aq_deweather_wide %>%
  group_by(date, pollutant, variable, prov = gadm1_name) %>% # average by date
  summarise(value = mean(value, na.rm = T),
            value_prev = mean(value_prev, na.rm = T)) %>%
  group_by(pollutant, variable, prov) %>% # average by period
  summarise(value = mean(value, na.rm = T),
            value_prev = mean(value_prev, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = variable, values_from = c(value, value_prev),
              names_glue = '{variable}_{.value}') %>%
  mutate(change = (observed_value - observed_value_prev) / observed_value_prev,
         change_weather = (predicted_value - predicted_value_prev) / observed_value_prev,
         change_emisson = change - change_weather) %>%
  select(pollutant, change, change_weather, change_emisson, prov) %>%
  pivot_longer(cols = -c(pollutant, prov), names_to = 'variable', values_to = 'value') %>%
  mutate(variable_labels = factor(labels_dict[variable], levels = labels_dict),
         variable_labels_ZH = factor(labels_dict_ZH[variable], levels = labels_dict_ZH))

prov_order <- prov_average %>% filter(variable == 'change') %>%
  arrange(value) %>%
  pull(prov)

p <- prov_average %>%
  ggplot(mapping = aes(factor(prov, levels = prov_order), value)) +
  geom_col(data = prov_average %>% filter(variable != 'change'),
           aes(fill = variable_labels)) +
  geom_point(data = prov_average %>% filter(variable == 'change'),
             aes(shape = 'circle'), size = 3) +
  coord_flip() +
  theme_crea() +
  scale_fill_crea_d('change', col.index = c(5,2), name='') +
  scale_shape_discrete(name = '', labels = 'yoy change') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.y = element_blank()) +
  labs(title = 'Average PM2.5 concentration by province',
       subtitle = '23 - 30 October 2024. Unit: µg/m³',
       x = '',
       y = '')
quicksave('provincial average PM2.5 yoy.png', plot = p)

p <- prov_average %>%
  ggplot(mapping = aes(factor(prov, levels = prov_order), value)) +
  geom_col(data = prov_average %>% filter(variable != 'change'),
           aes(fill = variable_labels_ZH)) +
  geom_point(data = prov_average %>% filter(variable == 'change'),
             aes(shape = 'circle'), size = 3) +
  coord_flip() +
  theme_crea() +
  scale_fill_crea_d('change', col.index = c(5,2), name='') +
  scale_shape_discrete(name = '', labels = '同比变化') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = prov_dict_ZH) +
  theme(axis.ticks.y = element_blank()) +
  labs(title = '京津冀及周边地区PM2.5同比变化',
       subtitle = '10月23日至30日，微克/立方米³',
       x = '',
       y = '')
quicksave('provincial average PM2.5 yoy ZH.png', plot = p)

write_csv(prov_average, 'provincial_average_pm25_yoy.csv')

