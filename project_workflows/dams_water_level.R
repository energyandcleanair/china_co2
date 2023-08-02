
readwindEN('data/China_Water_Level_Reservoirs.xlsx', c('var', 'V1', 'dam'), force_last_of_month = F, read_vardata = T, zero_as_NA = T) -> dams

mad_filter=10

dams %>% filter(!is.na(Value)) %>% 
  group_by(dam, var) %>% 
  mutate(change = Value-lag(Value),
         deviation = Value - rollapply(Value, width=21,FUN=median,fill=NA,align='center', na.rm=T),
         is_mad = is_outlier(change, mad_filter*2, na.rm=T) | 
           #is_outlier(deviation, mad_filter, na.rm=T) | 
           is_outlier(Value, mad_filter, na.rm=T)) %>% 
  filter(!is_mad | var!='Water Level') %>% 
  ungroup ->
  dams_nomad

dams_nomad %>% summary

dams_nomad %>% mutate(var=var %>% tolower %>% gsub(' ', '_', .)) %>% 
  select(dam, date, var, Value) %>% 
  pivot_wider(names_from=var, values_from=Value) -> dams_wide

dams_wide %<>% 
  filter(year(date)>=2012) %>% 
  complete(dam, date) %>% 
  group_by(dam) %>% 
  fill(water_storage_capacity, .direction='downup')

dams_wide %<>% 
  mutate(water_level=na.approx(water_level, date, date, na.rm=F), 
         water_level_deviation=water_level-mean(water_level, na.rm=T)) %>% 
  filter(!is.na(water_level)) %>% filter(2023 %in% year(date))

dams_wide  %>% filter(date>='2017-01-07') %>% 
  mutate(plotdate=date %>% 'year<-'(2022), year=as.factor(year(date)),
         dam = dam %>% gsub(' \\(.*| Power| Reserv', '', .)) %>% 
  ggplot(aes(plotdate, water_level_deviation)) + 
  geom_line(aes(col=year, linewidth=year, alpha=year)) + 
  facet_wrap(~dam) +
  theme_crea() +
  labs(title='Water level in 13 hydropower reservoirs across China',
       subtitle = 'Deviation from mean',
       y='meters', x='') +
  scale_x_date(date_labels = '%b') + 
  scale_linewidth_discrete(range=c(.5,1.25)) +
  scale_alpha_discrete(range=c(.5,1)) +
  scale_color_manual(values=colorspace::darken(crea_palettes$change, .2)) -> p
quicksave('outputs/Water level in 16 hydropower reservoirs across China.png', plot=p)

dams_wide %>% group_by(date) %>% filter(!grepl('14 pm', dam)) %>% 
  summarise(across(water_level_deviation, ~weighted.mean(.x, water_storage_capacity, na.rm=T))) ->
  overall

overall %>% filter(date>='2017-01-07') %>% 
  mutate(plotdate=date %>% 'year<-'(2022), year=as.factor(year(date))) %>% 
  ggplot(aes(plotdate, water_level_deviation, col=year)) + 
  geom_line(aes(col=year, linewidth=year, alpha=year)) + 
  labs(title='Water level in 13 hydropower reservoirs across China',
       subtitle = 'Average deviation from mean, weighted by reservoir capacity',
       y='meters', x='') +
  theme_crea() +
  scale_x_date(date_labels = '%b') +
  scale_linewidth_discrete(range=c(1,2)) +
  scale_alpha_discrete(range=c(.5,1)) +
  scale_color_manual(values=colorspace::darken(crea_palettes$change, .2)) -> p
quicksave('outputs/Average water level in 16 hydropower reservoirs across China.png', plot=p)
