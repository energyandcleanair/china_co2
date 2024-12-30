source('scripts/load_package.R')

output_dir <- 'outputs/province_clean_power_analysis'

getwindvars(get_data_file('wind_solar_utilization_capacity_by_province.xlsx'), skip=2)

readwindEN(get_data_file('power generation by province and source.xlsx'),
           c('var', 'source', 'prov'), read_vardata = T,
           columnExclude = 'Consumption') %>%
  mutate(var='Generation') -> gen

readwindEN(get_data_file('power generation by province and source.xlsx'),
           c('var', 'prov'), read_vardata = T,
           columnFilter = 'Consumption') -> cons

readwindEN(get_data_file('wind_solar_utilization_capacity_by_province.xlsx'),
           c('prov', 'var', 'source'),
           columnExclude = 'National',
           skip=2, read_vardata = T) -> cap_util

readwindEN(get_data_file('power capacity by province&type.xlsx'),
           c('prov', 'var', 'source'),
           columnExclude = 'Wind|Solar',
           skip=2, read_vardata = T) -> cap_all

#fill nuclear forward
cap_all %<>% mutate(Value=ifelse(Value==0, NA, Value)) %>% group_by(prov, source) %>% fill(Value, .direction='down')

#combine
bind_rows(gen, cons, cap_util, cap_all) %>%
  mutate(source=disambiguate(source, c('Thermal', 'Wind', 'Solar', 'Hydro', 'Nuclear')),
         var=disambiguate(var, c('Utilization', 'Capacity', 'Consumption'))) %>%
  group_by(var, prov, source) %>%
  mutate(Value=ifelse(Value==0, NA, Value)) %>%
  filter(!grepl('6,000 Kilowatts', source)) ->
  provdata

#fill solar backward
read_bp('G:/My Drive/CO2data/BPreviews/Statistical Review of World Energy Data.xlsx', sheet='(Solar|Wind) Capacity', startRow = 4, read_multiple = T) %>%
  bind_rows() %>% filter(country=='China') -> ws_cap_natl

ws_cap_natl %>% mutate(date=ymd(paste(year, 12, 31)),
              source=disambiguate(variable, c('Solar', 'Wind'))) %>%
  select(source, date, national_total_gw=value) %>%
  left_join(provdata %>% filter(var=='Capacity', source %in% c('Solar', 'Wind')), .) %>%
  group_by(prov, source) %>%
  mutate(across(national_total_gw, ~na.approx(.x, na.rm=F)),
         Value=na.cover(Value,
                        Value[which(!is.na(Value))[1]] * national_total_gw / national_total_gw[which(!is.na(Value))[1]])) %>%
  select(-national_total_gw) %>%
  bind_rows(provdata %>% filter(!(var=='Capacity' & source %in% c('Solar', 'Wind')))) ->
  provdata


#calculate monthly values from YTD
provdata %<>% filter(var %in% c('Generation', 'Utilization')) %>%
  group_by(prov, source, var) %>% unYTD %>%
  bind_rows(provdata %>% filter(var=='Capacity') %>% mutate(Value1m=Value))

#eliminate nonsensical utilization values
provdata %<>% mutate(Value1m=case_when(var=='Utilization' & Value1m>days_in_month(date)*24~NA,
                                       Value1m<0~NA,
                                       T~Value1m))

#fill gaps in time series
provdata %>% ungroup %>%
  complete(prov, nesting(source, var), date) %>%
  group_by(prov, source, var) %>%
  mutate(Value1m=ifelse(Value1m<=0, NA, Value1m) %>% na.approx(na.rm=F)) ->
  provdata_filled

#add utilization when missing
provdata_filled %<>% group_by(prov, source, date) %>%
  filter('Utilization' %notin% var) %>%
  summarise(Value1m=Value1m[var=='Generation']/Value1m[var=='Capacity']) %>%
  mutate(var='Utilization') %>%
  bind_rows(provdata_filled)

#add a variant of generation keeping utilization at trend
provdata_filled %<>%
  filter(var=='Utilization') %>%
  group_by(prov, source) %>%
  filter(!all(is.na(Value1m))) %>%
  group_modify(function(df, group) {
    data_start=min(df$date[!is.na(df$Value1m)])
    lm(Value1m~poly(pmax(ymd('2012-01-31'), data_start, date),2)+as.factor(month(date)),
                    data=df %>% filter(year(date)>=2012)) -> m
    df %>% mutate(Value1m=predict(m, .))
  }) %>% bind_rows(provdata_filled %>% filter(var=='Capacity')) %>%
  mutate(variant='Utilization at trend') %>%
  bind_rows(provdata_filled %>% mutate(variant='Actual utilization'))

provdata_filled %>% filter(variant=='Utilization at trend', var=='Utilization',
                           source %in% c('Wind', 'Solar'), Value1m<30*24) %>%
  ggplot(aes(date, Value1m, col=source)) + geom_line() + facet_wrap(~prov)

#use trend utilization when actual not available
provdata_filled %<>% filter(var=='Utilization') %>%
  group_by(prov, date, source) %>%
  fill(Value1m) %>%
  bind_rows(provdata_filled %>% filter(var!='Utilization'))

#calculate generation from capacity and utilization
provdata_filled %<>% filter(var %in% c('Capacity', 'Utilization')) %>%
  group_by(prov, source, date, variant) %>%
  summarise(Value1m=Value1m[var=='Capacity']*Value1m[var=='Utilization']) %>%
  mutate(var='Generation, calculated') %>%
  bind_rows(provdata_filled)

#adjust thermal generation in the trend utilization case
provdata_filled %>%
  filter(source != 'Thermal', var=='Generation, calculated') %>%
  group_by(prov, date, source) %>%
  summarise(deviation=Value1m[grepl('Actual', variant)]-Value1m[grepl('trend', variant)]) %>%
  group_by(prov, date) %>%
  summarise(across(deviation, ~sum(.x, na.rm=T))) ->
  deviation

provdata_filled %<>% left_join(deviation) %>%
  group_by(prov, date) %>%
  mutate(Value1m=case_when(source=='Thermal' & var=='Generation, calculated' & variant=='Utilization at trend' ~
                             pmax(0, Value1m[source=='Thermal' & var=='Generation'] + deviation),
                           T~Value1m))

#fill trend generation when not available, using actual generation
provdata_filled %<>% ungroup %>% filter(grepl('Generation', var)) %>%
  complete(nesting(prov, source), variant, date, var) %>%
  group_by(prov, date, source) %>%
  fill(Value1m) %>%
  bind_rows(provdata_filled %>% filter(!grepl('Generation', var)))


provdata_filled %<>% group_by(var, prov, source, variant) %>% mutate(Value_rollmean_12m=zoo::rollapplyr(Value1m, 12, mean, fill=NA))



provdata_filled %>%
  filter(var=='Generation, calculated', year(date)>=2013) %>%
  mutate(source=factor(source, levels=c('Thermal', 'Hydro', 'Nuclear', 'Wind', 'Solar'))) ->
  plotdata

fuel_cols <- crea_palettes$CREA[c('Light.gray', 'Blue', 'Dark.red', 'Dark.violet', 'Orange')]
names(fuel_cols) <- c('Thermal', 'Hydro', 'Nuclear', 'Wind', 'Solar')

plotdata %>% filter(variant=='Utilization at trend') %>% #Utilization at trend | Actual utilization
  ggplot(aes(date, Value_rollmean_12m, fill=source)) + geom_area(position='fill') + facet_wrap(~prov) +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(expand=expansion(mult=0), labels=scales::percent) +
  snug_x_date +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title='Power generation mix by province', subtitle='12-month rolling mean', x='', y='') -> p
quicksave(file.path(output_dir, 'Power generation mix by province.png'), plot=p)

plotdata %>%
  filter(variant=='Utilization at trend') %>% #Utilization at trend
  replace_na(list(Value_rollmean_12m=0)) %>%
  group_by(prov, date) %>%
  mutate(share=Value_rollmean_12m/sum(Value_rollmean_12m)) ->
  shares

shares %>%
  group_by(variant, prov, source) %>%
  summarise(across(c(Value_rollmean_12m, share),
                   ~.x[date==max(date)]-.x[date=='2020-12-31'])) ->
  changes

changes %>% filter(source!='Thermal') %>% group_by(prov) %>% summarise(across(is.numeric, sum)) %>%
  mutate(source='Non-fossil total') -> changes_total

changes %>% filter(source!='Thermal') %>%
  ggplot(aes(prov, share, fill=source)) +
  geom_col() +
  geom_point(data=changes_total, aes(shape='Total'), size=2, col=crea_palettes$CREA['Red']) +
  coord_flip() +
  scale_x_discrete(limits=changes_total %>% arrange(share) %>% use_series(prov)) +
  scale_shape_manual(values=16, name='') +
  labs(title='Changes in the share of clean power generation', subtitle='From 2020 to latest 2024 months of data', x='', y='') +
  theme_crea() +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(labels=scales::percent) -> p
quicksave(file.path(output_dir, 'Changes in power generation mix.png'), plot=p)

require(sf)

#Sys.setenv(GIS_DIR='~/GIS')
get_adm(1, 'low') %>% st_as_sf() %>% filter(NAME_0=='China') -> adm1
get_adm(0, 'low') %>% st_as_sf() -> adm0
adm1 %>% mutate(prov=fix_province_names(NAME_1)) %>%
  left_join(changes_total) %>%
  ggplot() +
  geom_sf(data=adm0, fill='white') +
  geom_sf(aes(fill=share)) +
  coord_sf(xlim=st_bbox(adm1)[c(1,3)], st_bbox(adm1)[c(2,4)],
           expand=T) +
  scale_x_continuous(expand=expansion(mult=c(.01, .01))) +
  scale_y_continuous(expand=expansion(mult=c(.01, .01))) +
  theme_crea() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='steelblue'),
        panel.border = element_rect(color='gray', linewidth = .5)) +
  scale_fill_crea_c('CREA', col.index = c(11:9,3:4), guide=guide_colorbar(),
                    labels=scales::percent) +
  labs(title='Change in the share of non-fossil generation',
       subtitle='from 2020 to 2024, by province',
       fill='%-points') -> p
quicksave(file.path(output_dir, 'Changes in clean share, map.png'), plot=p)

adm1 %>% mutate(prov=fix_province_names(NAME_1)) %>%
  left_join(changes %>% ungroup %>% filter(source!='Thermal') %>%
              mutate(across(source, droplevels)) %>%
              complete(prov, source, variant) %>%
              replace_na(list(share=0))) %>%
  filter(variant=='Utilization at trend') %>%
  ggplot() +
  facet_wrap(~source) +
  geom_sf(data=adm0, fill='white') +
  geom_sf(aes(fill=share)) +
  coord_sf(xlim=st_bbox(adm1)[c(1,3)]+c(-.1,.1), st_bbox(adm1)[c(2,4)]+c(-.1,.1),
           expand=F) +
  theme_crea() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='steelblue'),
        panel.border = element_rect(color='gray', linewidth = .5)) +
  scale_fill_crea_c('CREA', col.index = c(11:9,3:4), guide=guide_colorbar(),
                    labels=scales::percent) +
  labs(title='Change in the share of non-fossil generation',
       subtitle='from 2020 to 2024, by province',
       fill='%-points') -> p
quicksave(file.path(output_dir, 'Changes in clean share by technology, map.png'), plot=p)


shares %>% filter(source!='Thermal') %>% group_by(prov, date) %>%
  summarise(across(share, sum)) %>%
  filter(date %in% c(ymd('2020-12-31'), max(date))) ->
  shares_plotdata

shares_plotdata %>% group_by(prov) %>% mutate(change=share[2]-share[1]) %>%
  ggplot(aes(prov, share, col=change>0)) +
  geom_point(aes(shape=date=='2020-12-31'), size=3) +
  geom_line(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), linewidth=1) +
  scale_shape_manual(values=c(NA,16), guide='none') +
  coord_flip() +
  scale_x_discrete(limits=shares_plotdata %>% filter(date==max(date)) %>% arrange(share) %>% use_series(prov)) +
  labs(title='Share of clean power generation',
       subtitle='Changes from 2020 to latest 12 months of data', x='', y='') +
  theme_crea() + scale_color_crea_d('change', col.index=c(6,2), guide='none') +
  scale_y_continuous(labels=scales::percent) -> p
quicksave(file.path(output_dir, 'Clean power shares, change.png'), plot=p)

provdata_filled %>% filter(var=='Capacity', source %in% c('Solar'), year(date)>=2018) %>%
  ggplot(aes(date, Value1m/100)) + geom_line(linewidth=1, col='darkred') +
  facet_wrap(~prov, scales='free_y') + x_at_zero() + theme_crea() +
  labs(y='GW', x='', title='Solar power capacity in China by province') -> p
quicksave(file.path(output_dir, 'Solar power capacity in China by province.png'), plot=p)



#Analysis of factors
readwindEN(get_data_file('Electricity Consumption by province_2023.xlsx'), c('prov', 'var'),
           read_vardata = T, skip=3) -> power_demand

readwindEN(get_data_file('Regional GDP complete.xlsx'), c('var', 'price_basis', 'sector', 'subsector'),
           read_vardata = T, columnFilter = 'Constant|Current') -> gdp_natl

readwindEN(get_data_file('Regional GDP complete.xlsx'), c('prov', 'var', 'sector', 'subsector'),
           read_vardata = T, columnExclude = 'Constant|Current') %>%
  mutate(subsector=disambiguate(subsector,
                                c('Hotels and Catering Services'='Hotel',
                                  Finance='Financ',
                                  'Wholesale and Retail Trade'='Trade',
                                  'Transport, Storage and Post'='Transport')),
         across(contains('sector'), ~ifelse(.x=='YTD', NA, .x)),
         type=ifelse(grepl('YTD', Name), 'YTD', 'single month')) -> gdp_prov

readwindEN(get_data_file('iron&steel output by province.xlsx'),
           c('prov', 'var', 'prod'),
           read_vardata = T) %>% filter(!grepl('Taiwan', Name)) -> i_s

readwindEN(get_data_file('Output of electricity intensive products by province.xlsx'),
           c('prov', 'var', 'prod'),
           read_vardata = T) -> ind

ind %<>% bind_rows(i_s)

read_xlsx('inst/extdata/population by province.xlsx') %>%
  pivot_longer(-Region, names_to='year', values_to='pop') %>%
  rename(prov=Region) -> pop

#target dataset: increase in capacity, increase in generation, GDP growth, GDP per capita, industrial output & growth

#analyses:
#coal capacity additions vs coal power generation growth
provdata_filled %>% group_by(prov) %>%
  filter(source=='Thermal', var %in% c('Capacity', 'Generation'), variant=='Utilization at trend') %>%
  mutate(value_to_plot = ifelse(var=='Capacity', Value1m, Value_rollmean_12m),
         Generation_volume=value_to_plot[var=='Generation' & date==max(date)]) %>%
  group_by(prov, var, Generation_volume) %>% summarise(change=value_to_plot[date==max(date)]/value_to_plot[date=='2015-12-31']-1) %>%
  spread(var, change) %>% ggplot(aes(Capacity, Generation)) + geom_point(aes(size=Generation_volume)) + geom_text_repel(aes(label=prov)) +
  geom_abline(linetype='dashed')

#wind and solar utilization vs additions
provdata_filled %>% #filter(prov!='Tibet') %>%
  group_by(prov, var, variant, date) %>%
  mutate(share=Value_rollmean_12m/sum(Value_rollmean_12m, na.rm=T),
         value_to_plot = ifelse(var %in% c('Capacity', 'Generation'), share, Value_rollmean_12m)) %>% #Value1m
  ungroup %>%
  filter(source %in% c('Wind', 'Solar'), var %in% c('Capacity', 'Generation', 'Utilization'), date==max(date), variant=='Utilization at trend') %>%
  select(prov, source, var, value_to_plot) %>%
  spread(var, value_to_plot) %>%
  ggplot(aes(Utilization, Generation)) + geom_point() + facet_wrap(~source, scales='free') +
  geom_text_repel(aes(label=prov))

#wind and solar additions vs population density

#generation growth rate vs increase in share of clean
provdata_filled %>% #filter(prov!='Tibet') %>%
  group_by(prov, var, variant, date) %>%
  summarise(across()) %>%
  bind_rows() #add totals


#demand growth vs GDP, industrial output growth
#demand gowrth vs starting level per capita


