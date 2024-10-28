
paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       '&region_type=gadm1',
       #'&region_id=CN',
       '&region_iso2=CN') %>% read_csv ->
  met_prov

paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       #'&region_type=gadm1',
       '&region_id=CN') %>% read_csv ->
  met_natl

pop <- read_csv(get_data_file('population_by_province_2022.csv'))

power_demand <- readwindEN(get_data_file('Electricity Consumption by province_2023.xlsx'),
                           c('prov', 'var'), skip=3,
                           read_vardata = T) %>%
  rename(region_name=prov)

met_prov %>% mutate(date = date %>% 'day<-'(days_in_month(.)),
                    region_name=fix_province_names(region_name)) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  met_prov_monthly

met_natl %>% mutate(date = date %>% 'day<-'(days_in_month(.))) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  met_natl_monthly2

met_prov_monthly %>% left_join(pop) %>%
  group_by(variable, date) %>%
  summarise(across(value, list(value=~weighted.mean(.x, population_total),
                               urban=~weighted.mean(.x, population_urban)))) %>%
  rename(value=value_value) ->
  met_natl_monthly

met_prov_monthly %>% filter(date<=last_month) %>%
  spread(variable, value) %>%
  left_join(power_demand) ->
  power_demand

power_demand %>% filter(year(date)>2017) %>%
  lm(Value~(CDD+HDD)*date:region_name, data=.) -> m

m %>% summary

power_demand %<>% ungroup %>% mutate(Value_predicted=predict(m, .))

power_demand %>%
  pivot_longer(c(Value, Value_predicted)) %>%
  group_by(name, date) %>%
  summarise(across(value, sum)) %>%
  filter(year(date)>=2015) %>%
  #filter(month(date)==9) %>%
  ggplot(aes(date, value, col=name)) + geom_line()

power_demand %>%
  pivot_longer(c(Value, Value_predicted)) %>%
  group_by(name, date) %>%
  summarise(across(value, sum)) %>%
  group_by(name, month(date)) %>%
  mutate(yoy=value-lag(value)) %>%
  filter(year(date)>=2015) %>%
  #filter(month(date)==9) %>%
  ggplot(aes(date, yoy, col=name)) + geom_line()


met_prov_monthly %>% filter(year(date)>=2019, variable=='CDD') %>%
  mutate(year=as.factor(year(date)),
         plotdate=date %>% 'year<-'(2022)) %>%
  ggplot(aes(plotdate, value, col=year)) + geom_line() + facet_wrap(~region_name)


met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  group_by(region_name) %>% mutate(value_rel=value/mean(value)) %>%
  ggplot(aes(date, value-273.15, fill=value_rel)) +
  geom_col() + facet_wrap(~region_name) +
  scale_fill_crea_c('change', guide='none') + theme_crea()


met_prov_monthly %>% filter(month(date)==9, variable=='CDD') %>%
  group_by(region_name) %>% mutate(value_rel=value-mean(value)) %>%
  ggplot(aes(date - 9*30, value, fill=value_rel)) +
  geom_col() + facet_wrap(~region_name) +
  x_at_zero() +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) + theme_crea() +
  labs(title='Population-weighted average cooling degree days in China',
       subtitle='in September of each year, by province',
       y='degrees Celcius', x='')


met_natl_monthly %>% select(-value, value=value_urban) %>%
  filter(month(date) %in% 6:9, variable=='CDD') %>%
  mutate(value_rel=value-mean(value),
         month=factor(month.name[month(date)], levels=month.name)) %>%
  ggplot(aes(year(date), value, fill=value_rel)) +
  geom_col() +
  x_at_zero() +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dashed', color='black', alpha=.7,
              linewidth=1) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024), expand = expansion(mult=c(.02, .02))) +
  facet_wrap(~month) +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Cooling loads in China by year and month',
       subtitle='population-weighted average cooling degree days',
       y='degrees Celcius', x='')



met_natl_monthly %>%
  filter(month(date) %in% 6:9, variable=='temperature') %>%
  mutate(value_rel=value-mean(value),
         month=factor(month.name[month(date)], levels=month.name)) %>%
  ggplot(aes(year(date), value-273.15, col=value_rel)) +
  geom_line(linewidth=2) +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dashed', color='black', alpha=.7,
              linewidth=1) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024), expand = expansion(mult=c(.02, .02))) +
  facet_wrap(~month) +
  scale_color_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(panel.spacing = unit(1.5, "lines"),
             plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Temperatures in China by year and month',
       subtitle='population-weighted average',
       y='degrees Celcius', x='')




met_prov_monthly %>% filter(month(date)==9, variable=='CDD') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  summarise(value=value[year(date)==2024]-value[year(date)==2023]) %>%
  arrange(value) %>%
  mutate(region_name=factor(region_name, levels=region_name)) %>%
  ggplot(aes(region_name, value, fill=value)) +
  geom_col() +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Year-on-year change in population-weighted average cooling degree days',
       subtitle='September 2024, by province',
       y='degrees Celcius', x='') +
  coord_flip()



met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  ggplot(aes(year(date), value, col=value)) +
  geom_line(linewidth=1) +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dotted', color='black', alpha=.3,
              linewidth=.7) +
  facet_wrap(~region_name, scales='free_y') +
  scale_color_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Year-on-year change in population-weighted average cooling degree days',
       subtitle='September 2024, by province',
       y='degrees Celcius', x='')


met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  summarise(anomaly=value[year(date)==2024]-mean(value[year(date)<=2023]),
            difference_with_previous_record=value[year(date)==2024]-max(value[year(date)<=2023])) %>%
  arrange(anomaly) %>%
  mutate(region_name=factor(region_name, levels=region_name)) %>%
  pivot_longer(-region_name) %>%
  ggplot(aes(region_name, value, fill=value)) +
  geom_col() +
  facet_wrap(~name) +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Temperature anomalies by province',
       subtitle='September 2024',
       y='degrees Celcius', x='') +
  coord_flip()
