read_power_generation() -> pwr_data2

pwr_data2$monthly %<>%
  select(date, variable=var, source, subtype, value=Value1m, Unit, data_source=Source, basis_for_data) %>%
  group_by(source, subtype) %>%
  mutate(YoY_change_absolute_1m = get.yoy(value, date, 'absolute'),
         YoY_change_percent_1m = get.yoy(value, date, 'relative'),
         label = na.cover(subtype, source),
         broad_label = case_when(label %in% c('Solar', 'Wind') ~ 'Solar & wind',
                                 label %in% c('Nuclear', 'Hydro', 'Biomass') ~
                                   'Hydro, nuclear & biomass',
                                 label %in% c('Coal', 'Gas') ~ 'Coal & gas',
                                 label == 'Total' ~ label)) %>%
  write_csv('outputs/power_data.csv')

system('git add outputs/power_data.csv')
system(paste0('git commit -m "power data until ', focus_month, '"'))
system('git push')




pwr_data2$monthly %>%
  filter(grepl('Wind|Solar|Nuclear|Hydro', label), variable=='Utilization', year(date) %in% 2015:2023) ->
  cf

cf %>%
  ggplot(aes(month(date), value, col=as.factor(year(date)))) + geom_line() + facet_wrap(~label)

cf %>% group_by(label, month=month(date)) %>%
  summarise(across(date, ~paste0(min(year(.x)), '-', max(year(.x)))),across(value, ~mean(.x, na.rm=T))) %>%
  spread(month, value) %>% copy.xl()

pwr_data2$monthly %>%
  filter(variable %in% c('Generation, hybrid', 'Heat rate'),
         year(date) %in% 2023:2024) %>%
  mutate(label=ifelse(variable=='Heat rate', 'Heat rate, coal', label)) -> gen_cons

gen_cons %>%
  ggplot(aes(month(date), value, col=as.factor(year(date)))) + geom_line() + facet_wrap(~label)

gen_cons %>% ungroup %>% mutate(year=year(date), month=month(date)) %>%
  select(label, year, month, value) %>%
  spread(month, value) %>%
  arrange(grepl('Heat rate', label), label) %>% copy.xl()


pwr_data2$monthly %>% ungroup %>% mutate(year=year(date), month=month(date)) %>%
  filter(variable == 'Capacity', label %in% c('Wind', 'Solar'),
         year(date) %in% 2023:2024) %>%
  select(label, year, month, value) %>%
  spread(month, value) %>%
  copy.xl()
