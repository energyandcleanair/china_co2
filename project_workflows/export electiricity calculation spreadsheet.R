pwr_data$monthly %>% ungroup %>%
  filter(date %in% c(focus_month, focus_month %>% 'year<-'(year(.)-1))) %>%
  mutate(source=ifelse(var=='Heat rate', 'Thermal', source),
         var=paste0(var, ' (', Unit, ')'),
         subtype=ifelse(source=='Hydro' & var != 'Capacity', 'Conventional Hydropower', subtype)) %>%
  select(var, source, subtype, date, Value1m) %>%
  spread(var, Value1m) %>%
  arrange(source, subtype, date) %>% View
