pwr_data$monthly %>% filter(var == 'Capacity',
                            source %in% c('Wind', 'Solar')) %>%
  mutate(Value1m=case_when(date!='2024-08-31'~Value1m,
                           source=='Wind'~47403,
                           source=='Solar'~75235)) %>%
  group_by(source, subtype) %>%
  mutate(change = Value1m - lag(Value1m),
         plotdate = date %>% 'year<-'(2022),
         year = as.factor(year(date))) %>%
  group_by(source, subtype, year) %>%
  mutate(change_cumulative = cumsum(change)) %>%
  filter(year(date) >= 2020) %>%
  write_csv(file.path(output_dir, 'Newly added wind and solar.csv')) %>%
  (function(df) {
    df %>%
      group_by(month(date)) %>%
      mutate(added_yoy=change/lag(change)-1,
             added_yoy_ytd=change_cumulative/lag(change_cumulative)-1) %>%
      ungroup %>% filter(date==max(date)) %>%
      select(date, source, matches('change|yoy')) %>%
      print()
    df
  }) %>%
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

basename <- "Newly added wind and solar"
quicksave(
  file.path(output_dir, paste0(basename, ' ', lang, '.png')),
  plot = p,
  logo = T,
  scale = .8)
