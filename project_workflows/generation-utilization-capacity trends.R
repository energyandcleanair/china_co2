pwr_data$monthly %>%
  filter(var=='Utilization',
         year(date)>=2015,
         Value1m>0) %>%
  group_by(label = na.cover(subtype, source)) %>%
  mutate(Value12m=rollapplyr(Value1m, 12, sum, na.rm=T, fill=NA)) %>%
  filter(label %notin% c('Biomass')) %>%
  ggplot(aes(date, Value12m/8760)) +
  facet_wrap(~ trans(label), scales='free_y') +
  geom_line(linewidth=1, color=crea_palettes$CREA[1]) +
  labs(title = 'Capacity factors by energy source', x = '', col = '',
       subtitle = '12-month moving mean') +
  #scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  x_at_zero(labels=scales::percent) +
  theme_crea() +
  scale_color_crea_d('change') +
  lang_theme()


pwr_data$monthly %>%
  filter(var=='Capacity',
         year(date)>=2019,
         Value1m>0) %>%
  group_by(label = na.cover(subtype, source)) %>%
  filter(label %notin% c('Thermal', 'Conventional Hydropower'),
         Value1m!=Value1m[1]) %>%
  ggplot(aes(date, Value1m/100)) +
  facet_wrap(~ trans(label), scales='free_y') +
  geom_line(linewidth=1, color=crea_palettes$CREA[1]) +
  labs(title = 'Generation capacity by energy source', x = '', col = '', y='GW') +
  #scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  x_at_zero() +
  theme_crea() +
  scale_color_crea_d('change') +
  lang_theme()

pwr_data$monthly %>%
  filter(var=='Generation, hybrid',
         year(date)>=2019,
         Value1m>0) %>%
  group_by(label = na.cover(subtype, source)) %>%
  mutate(Value12m=rollapplyr(Value1m, 12, mean, na.rm=T, fill=NA)) %>%
  #filter(label %notin% c('Biomass')) %>%
  ggplot(aes(date, Value12m/10*12)) +
  facet_wrap(~ trans(label), scales='free_y') +
  geom_line() +
  labs(title = 'Power generation by energy source', x = '', y = trans('TWh/year'), col = '',
       subtitle = '12-month moving mean') +
  #scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  x_at_zero() +
  theme_crea() +
  scale_color_crea_d('change') +
  lang_theme()
