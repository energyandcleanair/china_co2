source('scripts/load_package.R')
lang='EN'

pwr_data <- read_power_generation(predict_solar_wind = T)


pwr_data$monthly %>% filter(var == 'Capacity',
                            is.na(subtype)) %>%
  mutate(Value1m=case_when(date!='2024-12-31'~Value1m,
                           source=='Wind'~52068,
                           source=='Solar'~88666,
                           source=='Thermal'~144445,
                           source=='Hydro'~43595,
                           source=='Nuclear'~6083)) %>%
  group_by(source, subtype) %>%
  mutate(change = Value1m - lag(Value1m),
         plotdate = date %>% 'year<-'(2022),
         year = as.factor(year(date))) %>%
  group_by(source, subtype, year) %>%
  mutate(change_cumulative = cumsum(change)) %>%
  write_csv(file.path(output_dir, 'Newly added power capacity.csv')) ->
  cap

cap %>%
  group_by(month(date)) %>%
  mutate(added_yoy=change/lag(change)-1,
         added_yoy_ytd=change_cumulative/lag(change_cumulative)-1) %>%
  ungroup %>% filter(date==max(date)) %>%
  select(date, source, matches('change|yoy'))

cap %>% filter(month(date)==month(max(date)), change_cumulative>0) %>%
  ggplot(aes(year(date), change_cumulative / 100, fill = source)) +
  geom_col() +
  facet_wrap(~ trans(source), ncol = 2, scales = 'free_y') +
  theme_crea() +
  x_at_zero() +
  scale_x_continuous(breaks=function(x) round(seq(x[1], x[2], 4), 0)) +
  scale_fill_crea_d(col.index = c(1,9,10,12,4), guide='none') +
  labs(title = trans('Newly added power capacity by year'),
       x = '', y = trans('GW'), col = '') +
  lang_theme()
quicksave(file.path(output_dir, paste0('Newly added power capacity by year', ' ', lang, '.png')), scale = .8)


cap %>% filter(year(date)>=2020, source %in% c('Solar', 'Wind')) %>%
  ggplot(aes(plotdate, change_cumulative / 100, col = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ trans(source), ncol = 1, scales = 'free_y') +
  theme_crea() +
  scale_color_crea_d('change', col.index = c(1:3, 5:7)) +
  x_at_zero() +
  scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  labs(title = trans('Newly added power capacity, year-to-date'),
       x = '', y = trans('GW'), col = '') +
  lang_theme() ->
  p

basename <- "Newly added capacity"
quicksave(
  file.path(output_dir, paste0(basename, ' ', lang, '.png')),
  plot = p,
  logo = T,
  scale = .8)

update_date='2024-12-31' %>% ymd
pwr_data$monthly %>% group_by(var, source) %>%
  filter(date %in% c(update_date, update_date %>% 'year<-'(year(.)-1)),
         source %in% c('Hydro', 'Wind')) %>%
  filter(all(is.na(subtype)) | subtype=='Conventional Hydropower') %>%
  group_by(source) %>%
  summarise(Value1m = Value1m[var=='Utilization' & date<update_date] /
              (Value1m[var=='Capacity' & date==update_date]/Value1m[var=='Capacity' & date<update_date]) *
              (Value1m[var=='Generation' & date==update_date]/Value1m[var=='Generation' & date<update_date])) %>%
  mutate(var='Utilization', date=update_date) %>%
  bind_rows(pwr_data$monthly %>% filter(date<update_date), .) %>%
  filter(var=='Utilization', source %in% c('Wind', 'Hydro'), year(date)>=2015) %>%
  mutate(plotdate = date %>% 'year<-'(2022) %>% 'day<-'(1),
         year = as.factor(year(date)),
         label = na.cover(subtype, source),
         month=factor(month.abb[month(date)], levels=month.abb)) ->
  utilization_plotdata


utilization_plotdata %>% group_by(label, plotdate, month) %>%
  summarise(across(Value1m, mean)) ->
  utilization_mean

#p <-
utilization_plotdata %>% filter(year(date)>=2020) %>%
  ggplot(aes(plotdate, Value1m)) +
  facet_wrap(~ trans(label), scales='free_y') +
  geom_line(aes(col = year, linewidth=year==year(update_date))) +
  geom_line(data=utilization_mean, aes(linetype='10-year mean'), linewidth=2, col='black') +
  labs(title = trans('Monthly running hours'), x = '', y = trans('hours'), col = '') +
  scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  theme_crea_new() + theme(legend.position='right', legend.direction = 'vertical', legend.box='vertical') +
  scale_color_crea_d('change', col.index=c(2:3,5:7)) +
  scale_linewidth_manual(values=c(1,1.5), guide='none') +
  scale_linetype_manual(values='dashed', name='') +
  lang_theme(lang = lang)

utilization_plotdata %>% filter(year(date)>=2015, month(date)<=month(update_date)) %>%
  ggplot(aes(year, Value1m, fill=year==year(update_date))) +
  facet_grid(trans(label)~month, scales='free_y') +
  geom_col() +
  geom_hline(data=utilization_mean %>% filter(month(plotdate)<=month(update_date)),
             aes(yintercept=Value1m, linetype='10-year mean'), linewidth=1, col='black') +
  labs(title = trans('Monthly running hours'), x = '', y = trans('hours'), col = '') +
  theme_crea_new() +
  theme(axis.text.x=element_text(angle=90, hjust=.5, size=rel(.8)),
        panel.spacing.x = unit(.7,'cm')) +
  scale_x_discrete(breaks=c('2015', '2024')) +
  scale_fill_crea_d('change', col.index=c(5,7), guide='none') +
  scale_linewidth_manual(values=c(1,1.5), guide='none') +
  scale_linetype_manual(values='dashed', name='') +
  x_at_zero() +
  lang_theme(lang = lang)



basename <- "Monthly running hours"
quicksave(
  file.path(output_dir, paste0(basename, ' ', lang, '.png')),
  plot = p,
  logo = T,
  scale = 1)
