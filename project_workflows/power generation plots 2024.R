pwr_data <- read_power_generation()

ember %>% filter(Area=='China', Unit=='TWh',
                 Variable %in% c('Wind', 'Solar', 'Coal', 'Gas', 'Hydro', 'Nuclear',
                         'Biomass')) %>%
  mutate(Value=Value*10,
         Variable=ifelse(Variable=='Bioenergy', 'Biomass', Variable),
         source=ifelse(Variable %in% c('Coal', 'Gas', 'Biomass'), 'Thermal', Variable),
         subtype=ifelse(Variable %in% c('Coal', 'Gas', 'Biomass'), Variable, NA),
         Date=Date %>% 'day<-'(days_in_month(Date))) %>%
  select(date=Date, Value1m=Value, source, subtype) %>%
  mutate(var='Generation, hybrid', Unit="100 million kwh", data_source='Ember') ->
  ember_chn

pwr_data$monthly %>%
  filter(date<=last_month, var=='Generation, hybrid') %>%
  mutate(data_source='CREA') %>%
  bind_rows(ember_chn) %>% filter(!is.na(Value1m)) %>%
  group_by(date, source, subtype, var) %>%
  filter(data_source=='CREA' | 'CREA' %notin% data_source) ->
  pwr_growth_plot

pwr_growth_plot %<>%
  group_by(source, subtype) %>%
  mutate(YoY_change_absolute_1m=get.yoy(Value1m, date, 'absolute'),
         label=na.cover(subtype, source)) %>%
  filter(year(date)>2015, !is.na(YoY_change_absolute_1m)) -> #%>%
  #filter(source != "Thermal" | (is.na(subtype) == (year(date)<2021)))
  pwr_growth_plot


pwr_growth_plot %<>% group_by(date, Unit) %>%
  summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
  mutate(label='Total', source='Total') %>% bind_rows(pwr_growth_plot) %>%
  mutate(label=factor(label, levels=c('Coal', 'Gas', 'Thermal',
                                      'Biomass','Hydro', 'Nuclear',
                                      'Wind', 'Solar', 'Total')),
         broad_label=case_when(label %in% c('Solar', 'Wind')~'Solar & wind',
                               label %in% c('Nuclear', 'Hydro', 'Biomass')~'Hydro, nuclear & biomass',
                               label %in% c('Coal', 'Gas')~'Coal & gas',
                               label=='Total'~label))

pwr_cols = c(fuel_cols[-5],
             Nuclear=unname(crea_palettes$CREA['Orange']),
             Biomass=unname(crea_palettes$CREA['Green']),
             Coal=unname(crea_palettes$CREA['Black']),
             Gas=unname(crea_palettes$CREA['Light.gray']))

pwr_growth_plot %>% filter(label %notin% c('Total', 'Thermal')) %>%
  ggplot(aes(date, YoY_change_absolute_1m/10, fill=label)) + geom_col() +
  scale_fill_manual(values=pwr_cols) +
  theme_crea() +
  labs(title='Growth in monthly power generation by source', y='TWh', fill='')

pwr_growth_plot %>% filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass')) %>%
  ggplot(aes(date, YoY_change_absolute_1m/10, fill=label)) + geom_col() +
  geom_point(data=pwr_growth_plot %>% filter(label=='Total')) +
  scale_fill_manual(values=pwr_cols) +
  theme_crea() +
  labs(title='Growth in monthly power generation by source', y='TWh', fill='')


pwr_growth_plot %>%
  filter(label %in% c('Solar', 'Wind', 'Nuclear', 'Biomass', 'Coal', 'Gas', 'Hydro', 'Total')) %>%
  ggplot(aes(date, YoY_change_absolute_1m/10, fill=label)) +
  facet_wrap(~label) +
  geom_col() +
  scale_fill_manual(values=pwr_cols) +
  theme_crea() +
  labs(title='Growth in monthly power generation by source', y='TWh', fill='')

pwr_growth_plot %>%
  filter(!is.na(broad_label)) %>%
  group_by(date, broad_label) %>%
  summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
  ggplot(aes(date, YoY_change_absolute_1m/10, fill=broad_label)) +
  facet_wrap(~broad_label) +
  geom_col() +
  theme_crea() +
  labs(title='Growth in monthly power generation by source', y='TWh', fill='') +
  scale_fill_crea_d('change', col.index = c(7,5,2,1), guide='none')

pwr_growth_plot %>%
  filter(!is.na(broad_label), broad_label!='Total') %>%
  group_by(date, broad_label, Unit) %>%
  summarise(across(c(Value1m, YoY_change_absolute_1m), sum)) %>%
  write_csv(file.path(output_dir, 'Growth in monthly power generation by source.csv')) %>%
  ggplot(aes(date, YoY_change_absolute_1m/10, fill=broad_label)) +
  geom_col() +
  theme_crea(legend.position='top') +
  labs(title='Growth in monthly power generation by source', y='TWh', fill='', x='') +
  scale_fill_crea_d('change', col.index = c(7,5,2,1),
                    guide=guide_legend(nrow=1)) +
  scale_x_date(expand=expansion(mult=c(.01, .01))) -> p
quicksave(file.path(output_dir, 'Growth in monthly power generation by source.png'),
          plot=p, logo=F, scale=1)

pwr_growth_plot %>%
  select(date, label, broad_label, source, subtype, value=Value1m, contains('YoY'), Unit, data_source) %>%
  write_csv('outputs/power_data.csv')

pwr_data$monthly %>% filter(var=='Capacity', source %in% c('Wind', 'Solar')) %>%
  group_by(source, subtype) %>%
  mutate(change=Value1m-lag(Value1m),
         plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1), year=as.factor(year(date))) %>%
  group_by(source, subtype, year) %>%
  mutate(change_cumulative=cumsum(change)) %>%
  filter(year(date)>=2020) %>%
  ggplot(aes(plotdate, change_cumulative/100, col=year)) + geom_line(linewidth=1) +
  facet_wrap(~source, ncol=1, scales='free_y') +
  theme_crea() + scale_color_crea_d('change', col.index = c(1:3,5:7)) +
  x_at_zero() +
  scale_x_date(date_labels = '%b') +
  labs(title='Newly added power capacity, year-to-date', x='', y='GW') -> p
quicksave(file.path(output_dir, 'Newly added wind and solar.png'),
          plot=p, logo=F, scale=.8)

if(F) {
  system('git add outputs/power_data.csv')
  system(paste0('git commit -m "power data until ',last_month,'"'))
  system('git push')
}
