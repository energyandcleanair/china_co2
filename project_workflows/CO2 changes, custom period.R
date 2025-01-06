output_dir='outputs/2024Q3'
variant_to_highlight = 'NBS' #'predicted'
period_name_short='Mar-Sep'
period_name_long="in March-September 2024, compared with 2023"

d.quarter %>% filter(!(sector == 'Total' & prod %in% c('Steam Coal', 'Natural Gas')),
                     grepl(variant_to_highlight, name)) %>%
  mutate(sector = case_when(prod=='Cement'~'Building Materials',
                            prod %in% c('Crude Oil', oilprod)~'Oil Consumption',
                            is.na(sector)&prod!='Total'~paste(prod,'Consumption'),
                            prod=='Total'~'Total',
                            T~sector)) %>%
  group_by(broad_prod, sector, date) %>%
  summarise(across(matches('^YoY_change|CO2'), sum)) %>%
  mutate(sector=recode(sector, Total='All Sectors', 'Oil Consumption'='All Sectors', 'Natural Gas Consumption'='All Sectors')) ->
  d.changes


d.changes %>% filter(broad_prod != 'Total', year(date)==2024, month(date) %in% 3:9) %>%
  group_by(broad_prod, sector) %>%
  summarise(across(YoY_change_1m, sum),
            across(date, list(min=min, max=max))) -> change_plot

change_plot %>%
  ggplot(aes(broad_prod, YoY_change_1m, fill=sector)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_crea_d(name='Sector') +
  labs(title="Contributions to changes in emissions",
       subtitle=period_name_long, y='Mt CO2', x='') +
  coord_flip() +
  #x_at_zero() +
  scale_x_discrete(limits=rev) -> plt
quicksave(file.path(output_dir, paste0('Contributions to emissions growth, ',period_name_short,'.png'), plot=plt, scale=1.33)


change_plot %>%
  select(sector, broad_prod, starts_with('date'), YoY_change_1m) %>%
  write_csv(file.path(output_dir, paste0('Contributions to emissions growth, by sector, ',period_name_short,'.csv'))) %>%
  ggplot(aes(sector, YoY_change_1m, fill=broad_prod)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  labs(title="Contributions to changes in emissions",
       subtitle=period_name_long, y='Mt CO2', x='') +
  coord_flip() +
  #x_at_zero() +
  scale_x_discrete(limits=rev) -> plt
quicksave(file.path(output_dir, paste0('Contributions to emissions growth, by sector, ',period_name_short,'.png')),
          plot=plt, scale=1, logo=F)
