read_xlsx('~/../Downloads/China daily imports (Coal, by origin country) — 2024-05-13 11h31.xlsx') -> imp_bycounty
read_xlsx('~/../Downloads/China daily imports (Coal, by grade) — 2024-05-13 11h32.xlsx') -> imp_bygrade

imp_bycounty %>% pivot_longer(-date, names_to = 'origin_country') %>%
  mutate(across(date, ymd), imported=ifelse(origin_country=='China', 'domestic', 'imported')) %>%
  group_by(imported, date) %>% summarise(across(value, sum)) %>%
  #mutate(value_rollmean=rollmean_date(value, date, 30)) %>%
  mutate(value_rollmean=rollmeanr(value, 30, fill=NA)) %>%
  #ggplot(aes(date, value_rollmean, fill=imported)) + geom_area()
  group_modify(function(df, group) get.yoy.df(df, 'value_rollmean')) %>%
  ggplot(aes(date, YoY, col=imported)) + geom_line()


imp_bygrade %>% pivot_longer(-date, names_to = 'grade') %>%
  mutate(across(date, ymd), met=ifelse(grepl('Metal|Coke|Met Coke', grade), 'met', 'thermal')) %>%
  group_by(met, date) %>% summarise(across(value, sum)) %>%
  #mutate(value_rollmean=rollmean_date(value, date, 30)) %>%
  mutate(value_rollmean=rollmeanr(value, 30, fill=NA)) %>%
  ggplot(aes(date, value_rollmean, fill=met)) + geom_area()
