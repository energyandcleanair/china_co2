output_dir <- 'outputs'
lang<-'EN'

pwr_data$monthly %>%
  filter(var == 'Capacity', source %in% c('Wind', 'Solar'),
         date<'2024-07-31') %>%
  bind_rows(tibble(date=ymd('2024-07-31'), source=c('Wind', 'Solar'),
                   Value1m=c(47053,73557))) %>%
  group_by(source, subtype) %>%
  mutate(change = Value1m - lag(Value1m),
         plotdate = date %>% 'year<-'(2022),
         year = as.factor(year(date))) %>%
  group_by(source, subtype, year) %>%
  mutate(change_cumulative = cumsum(change)) %>%
  filter(year(date) >= 2020) ->
#write_csv(file.path(output_dir, 'Newly added wind and solar.csv')) %>%
 plotdata

plotdata %>%
  ggplot(aes(plotdate, change_cumulative / 100, col = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ trans(source), ncol = 1, scales = 'free_y') +
  theme_crea() +
  scale_color_crea_d('change', col.index = c(1:3, 5:7)) +
  x_at_zero() +
  scale_x_date(date_labels = ifelse(lang == 'EN', '%b', '%m\u6708')) +
  labs(title = trans('Newly added power capacity, year-to-date'),
       x = '', y = trans('GW'), col = '') -> p
quicksave(file.path(output_dir, paste0('Newly added wind and solar ', lang, '.png')),
          plot = p, logo = F, scale = .9)



plotdata %>% filter(month(date)==7, year(date)>=2023) %>% tail %>% select(source, date, change)
