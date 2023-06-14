in_file = "data/fuel supply.xlsx"

readwindEN(in_file, c('var', 'prod'), columnFilter = "YTD", read_vardata = T, zero_as_NA = T) -> fuelsupply

fuelsupply %<>% arrange(date) %>% 
  mutate(Value=Value*ifelse(Unit=="ton", 1e-4,1),
         Unit=recode(Unit, ton="10000 tons"),
         type=case_when(V4=='YTD'~'YTD', T~type))

fuelsupply %<>% group_by(var, prod, type) %>% unYTD %>% roll12m()

fuelsupply %<>% mutate(var = ifelse(var=='Apparent Consumption' & prod=='Natural Gas',
                           'Apparent Consumption WIND', var),
              Value1m=Value1m*ifelse(Unit %in% c('10k ton', '10000 tons') & prod=='Natural Gas', 1e4/0.657e-3/1e8, 1),
              Unit=ifelse(prod=='Natural Gas', '100M cu.m', Unit))

fuelsupply %<>% filter(grepl('Finished Oil Products', prod)) %>% 
  mutate(prod='Oil Products', across(starts_with('Value'), ~.x * ifelse(grepl('Exports', var), -1, 1))) %>% 
  group_by(prod, date) %>% summarise(across(starts_with('Value'), sum)) %>%
  mutate(var='Net Imports') %>% bind_rows(fuelsupply %>% filter(!grepl('Imports',var) | !grepl('Oil Products', prod)))

fuelsupply %>% 
  filter(!grepl('Exports', var)) %>% 
  mutate(prod = case_when(grepl('Coal', prod)~'Coal',
                                       grepl('Natural Gas', prod)~'Fossil Gas',
                                       grepl('Crude Oil', prod)~'Crude Oil',
                                       grepl('Oil Products|Processing of Petroleum', prod)~'Oil Products',
                                       T~prod),
                      prod_group = case_when(grepl('Diesel|Gasoline|Kerosene', prod)~'Oil Products', T~prod),
                      var = case_when(grepl('Imports', var)~'Net Imports', T~var)) ->
  fuelsupply_plot

fuelsupply_plot %<>% group_by(date, prod_group, var) %>% 
  filter(prod_group == 'Oil Products', var=='Output') %>% 
  summarise(across(starts_with('Value'), ~.x[prod=='Oil Products'] - sum(.x[grepl('Diesel|Gasoline|Kerosene', prod)]))) %>% 
  mutate(prod='Other Oil Products') %>% 
  bind_rows(fuelsupply_plot)

fuelsupply_plot %<>% 
  filter(!grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>% 
  group_by(prod_group, date) %>% 
  summarise(across(starts_with('Value'), sum)) %>% 
  mutate(var='Total Supply') %>% 
  bind_rows(fuelsupply_plot)

fuelsupply_plot %>% filter(year(date)>=2017, !grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>% 
  mutate(prod_group = paste0(prod_group, ', ', case_when(prod_group=='Fossil Gas'~'bcm', T~'Mt')),
         across(starts_with('Value'), ~.x * case_when(prod_group=='Fossil Gas'~1/10, T~1/100))) %>% 
  write_csv(file.path(output_dir, 'fossil fuel supply.csv')) %>% 
  ggplot(aes(date, Value12m*12, col=var)) + geom_line(aes(linewidth=var=='Total Supply')) + facet_wrap(~prod_group, scales='free_y') +
  theme_crea() +
  labs(title='Fossil fuel supply', subtitle='12-month moving sum', y='', x='') +
  scale_linewidth_discrete(range=c(1,2), guide='none') +
  scale_color_crea_d('dramatic', name='') +
  expand_limits(y=0) +
  scale_x_date(expand=expansion(mult=c(0,.05))) -> p
quicksave(file.path(output_dir, 'fossil fuel supply.png'), plot=p)

fuelsupply_plot %>% filter(year(date)>=2017, grepl('Diesel|Gasoline|Kerosene|Other', prod)) %>% 
  write_csv(file.path(output_dir, 'oil products output.csv')) %>% 
  ggplot(aes(date, Value12m/100*12, col=prod)) + geom_line(linewidth=1.5) + 
  theme_crea() +
  labs(title='Output of different oil products', subtitle='12-month moving sum', y='', x='') +
  scale_color_crea_d('dramatic', name='') +
  expand_limits(y=0) + x_at_zero() +
  scale_x_date(expand=expansion(mult=c(0,.05))) -> p
quicksave(file.path(output_dir, 'oil products output.png'), plot=p)
