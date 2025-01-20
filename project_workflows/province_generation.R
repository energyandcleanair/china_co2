source('scripts/load_package.R')
require(units)
require(sf)
#Sys.setenv(GIS_DIR='~/GIS')

output_dir <- 'outputs/province_clean_power_analysis'

source('scripts/load_province_generation_data.R')
save(provdata_filled, plotdata, changes, adm1, shares,
     file=file.path(output_dir, 'alldata.RData'))
load(file.path(output_dir, 'alldata.RData'))


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


changes %>% filter(source!='Thermal') %>%
  ggplot(aes(prov, share, fill=source)) +
  geom_col() +
  geom_point(data=changes_total, aes(shape='Total'), size=2, col=crea_palettes$CREA['Red']) +
  coord_flip() +
  scale_x_discrete(limits=changes_total %>% arrange(share) %>% use_series(prov)) +
  scale_shape_manual(values=16, name='') +
  labs(title='Changes in the share of clean power generation',
       subtitle='From 2020 to latest 2024 months of data',
       x='', y='percentage-points') +
  theme_crea() +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(labels=scales::percent) -> p
quicksave(file.path(output_dir, 'Changes in power generation mix.png'), plot=p)


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


#target dataset: increase in capacity, increase in generation, GDP growth, GDP per capita, industrial output & growth
#table: clean change, coal power capacity change, starting share of hydro, GDP growth, power demand growth, population density, wind&solar conditions..?

#coal capacity changes
provdata_filled %>% group_by(prov) %>% filter(!is.na(Value1m)) %>%
  filter(source=='Thermal', var %in% c('Capacity', 'Generation'), variant=='Utilization at trend') %>%
  mutate(value_to_plot = ifelse(var=='Capacity', Value1m, Value_rollmean_12m),
         Generation_volume=value_to_plot[var=='Generation' & date==max(date)]) %>%
  group_by(prov, var, Generation_volume) %>% summarise(change=value_to_plot[date==max(date)]/value_to_plot[date=='2015-12-31']-1) %>%
  spread(var, change) -> coal_changes

#wind&solar utilization
provdata_filled %>%
  filter(date<'2024-10-30') %>%
  mutate(value_to_plot = ifelse(var == 'Capacity', Value1m, Value_rollmean_12m)) %>% #Value1m
  ungroup %>%
  filter(source %in% c('Wind', 'Solar'), var %in% c('Capacity', 'Utilization'),
         date==max(date), variant=='Utilization at trend') %>%
  select(prov, source, var, value_to_plot) %>%
  spread(var, value_to_plot) %>%
  group_by(source) %>%
  mutate(ratio=Utilization/weighted.mean(Utilization, Capacity)) %>%
  select(prov, source, ratio) %>%
  mutate(source=paste0(source, '_utilization_percent_of_national_average')) %>%
  spread(source, ratio) ->
  utilization_rating

#population density
adm1 %>% mutate(area_km2 = as.numeric(st_area(.)/1e6),
                prov=fix_province_names(NAME_1)) %>%
  st_drop_geometry() %>%
  select(prov, area_km2) %>%
  left_join(pop %>% filter(year==2023)) %>%
  mutate(pop_per_km2=pop*1e4/area_km2) -> pop_density


#generation growth
provdata_filled %>% filter(prov!='Tibet') %>%
  filter(var=='Generation', variant=='Utilization at trend', !is.na(Value_rollmean_12m)) %>%
  group_by(prov, var, variant, date) %>%
  summarise(across(Value_rollmean_12m, sum)) %>%
  summarise(generation_growth=Value_rollmean_12m[date==max(date)]/Value_rollmean_12m[date=='2020-12-31']-1,
            total_generation=Value_rollmean_12m[date==max(date)]*12) ->
  generation_growth

#GDP growth
base_date=ymd('2020-12-31')
years = as.numeric(round((max(gdp_prov$date) - base_date)/365*4, 0)/4)
gdp_prov %>% ungroup %>% filter(sector=='Total') %>%
  group_by(prov) %>%
  summarise(gdp_growth_rate=(gdp_12m[date==max(date)]/gdp_12m[date==base_date])^(1/years)-1,
            gdp_per_capita=(gdp_12m/pop)[date==max(date)]) %>%
  ungroup  ->
  gdp_growth

#join
list(coal_changes %>% select(prov, thermal_capacity_increase=Capacity, thermal_generation_increase=Generation),
     changes_total %>% select(prov, clean_share_increase=share),
     shares %>% filter(variant=='Utilization at trend', date==base_date, source!='Thermal') %>%
       summarise(across(share, sum)) %>% select(prov, clean_share_2020=share),
     utilization_rating,
     pop_density %>% select(prov, pop_per_km2),
     generation_growth %>% ungroup %>% select(prov, generation_growth, total_generation),
     gdp_growth) %>%
  Reduce(full_join, .) ->
  prov_table

prov_table %>% copy.xl()

prov_table %>% pivot_longer(-c(prov, clean_share_increase)) %>%
  group_by(name) %>%
  mutate(cor=cor(value, clean_share_increase, use='complete.obs'),
         name=gsub('_percent_of.*', '', name)) %>%
  ggplot(aes(value, clean_share_increase)) +
  geom_point(size=.5, color='darkred') +
  geom_smooth(method='gam') +
  geom_text_repel(aes(label=prov), size=1, alpha=.5) +
  facet_wrap(~name, scales='free_x') +
  geom_text(aes(label = paste0("Correlation: ", round(cor, 2))), x = Inf, y = Inf, hjust = 1.1, vjust = 1.3, inherit.aes = FALSE,
            size=2, alpha=.5)



#analyses:
#coal capacity additions vs coal power generation growth
provdata_filled %>% group_by(prov) %>% filter(!is.na(Value1m)) %>%
  filter(source=='Thermal', var %in% c('Capacity', 'Generation'), variant=='Utilization at trend') %>%
  mutate(value_to_plot = ifelse(var=='Capacity', Value1m, Value_rollmean_12m),
         Generation_volume=value_to_plot[var=='Generation' & date==max(date)]) %>%
  group_by(prov, var, Generation_volume) %>% summarise(change=value_to_plot[date==max(date)]/value_to_plot[date=='2015-12-31']-1) %>%
  spread(var, change) %>%
  ggplot(aes(Capacity, Generation)) + geom_point(aes(size=Generation_volume)) + geom_text_repel(aes(label=prov)) +
  geom_abline(linetype='dashed')

#wind and solar utilization vs additions
provdata_filled %<>% group_by(prov, var, variant, date) %>%
  mutate(share=Value_rollmean_12m/sum(Value_rollmean_12m, na.rm=T)) %>%
  ungroup


provdata_filled %>% #filter(prov!='Tibet') %>%
  filter(date<'2024-10-30') %>%
  mutate(value_to_plot = ifelse(var %in% c('Capacity', 'Generation'), share, Value_rollmean_12m)) %>% #Value1m
  ungroup %>%
  filter(source %in% c('Wind', 'Solar'), var %in% c('Capacity', 'Generation', 'Utilization'),
         date==max(date), variant=='Utilization at trend') %>%
  select(prov, source, var, value_to_plot) %>%
  spread(var, value_to_plot) %>%
  ggplot(aes(Utilization, Generation)) + geom_point() + facet_wrap(~source, scales='free') +
  geom_text_repel(aes(label=prov), size=2)


provdata_filled %>% #filter(prov!='Tibet') %>%
  filter(date<'2024-10-30') %>%
  group_by(prov, var, variant, date) %>%
  mutate(share=Value_rollmean_12m/sum(Value_rollmean_12m, na.rm=T),
         value_to_plot = ifelse(var %in% c('Capacity'), Value1m, Value_rollmean_12m)) %>% #Value1m
  ungroup %>%
  filter(source %in% c('Wind', 'Solar'), var %in% c('Capacity', 'Utilization'),
         date==max(date), variant=='Actual utilization') %>%
  select(prov, source, var, value_to_plot) %>%
  spread(var, value_to_plot) %>%
  ggplot(aes(Utilization, Capacity)) + geom_point() + facet_wrap(~source, scales='free') +
  geom_text_repel(aes(label=prov), size=2)


#wind and solar additions vs population density
provdata_filled %>% filter(var=='Capacity', source %in% c('Wind', 'Solar'), !is.na(Value1m)) %>%
  filter(date==max(date)) %>% ungroup %>% distinct(prov, source, .keep_all = T) %>%
  select(prov, source, Capacity=Value1m) %>%
  left_join(pop_density %>% select(-pop)) %>%
  left_join(gdp_prov %>% filter(is.na(subsector)) %>%
              select(prov, date, sector, pop, gdp_12m) %>%
              mutate(sector=paste('GDP', sector) %>% make_names) %>%
              spread(sector, gdp_12m)) %>%
  mutate(across(starts_with('gdp'), list(percap=~.x/pop))) -> gdp_pop

gdp_pop %>% lm(Capacity~(pop_per_km2+area_km2+pop+gdp_total_percap):source, data=.) %>% summary

gdp_pop %>%
  ggplot(aes(pop_per_km2, Value1m)) +
  geom_point() + geom_text_repel(aes(label=prov)) + facet_wrap(~source) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method='lm')


#coal capacity growth vs increase in share of clean


#generation growth rate vs increase in share of clean
generation_growth %>%
  left_join(changes_total %>% select(prov, change_in_clean_share=share)) %>%
  ggplot(aes(generation_growth, change_in_clean_share)) + geom_point() +
  geom_text_repel(aes(label=prov))


#demand growth vs GDP, industrial output growth
power_demand %<>% group_by(prov) %>%
  mutate(Value12m=zoo::rollapplyr(Value, 10, mean, fill=NA))


earlier_base_date=ymd('2015-12-31')
earlier_years=as.numeric(round((base_date - earlier_base_date)/365*4, 0)/4)




gdp_prov %>%
  left_join(power_demand %>% select(prov, date, power_demand_12m=Value12m)) ->
  gdp_pwr_prov

gdp_pwr_prov %>% filter(year(date)>=2019) %>%
  group_by(prov, sector, subsector) %>%
  summarise(across(c(gdp_12m, power_demand_12m), ~(.x[date==max(date)]/.x[date==base_date])^(1/years)-1)) %>%
  filter(sector=='Total', is.na(subsector)) %>%
  ggplot(aes(gdp_12m, power_demand_12m)) + geom_point() + geom_abline() + geom_text_repel(aes(label=prov))

#demand growth vs economic structure
gdp_pwr_prov %>% group_by(prov, date) %>%
  mutate(gdp_share_12m = gdp_12m/if_null(gdp_12m[sector=='Total'])) %>%
  group_by(prov, sector, subsector) %>%
  summarise(across(c(gdp_12m, power_demand_12m),
                   list(growth=~(.x[date==max(date)]/.x[date==base_date])^(1/years)-1,
                        earlier_growth=~(.x[date==base_date]/.x[date==earlier_base_date])^(1/earlier_years)-1)),
            across(gdp_share_12m, list(change=~.x[date==max(date)] - .x[date==base_date]))) %>%
  mutate(power_demand_12m_change=power_demand_12m_growth-power_demand_12m_earlier_growth,
         gdp_12m_change=gdp_12m_growth-gdp_12m_earlier_growth) %>%
  filter(sector=='Tertiary Industry', is.na(subsector)) %>%
  ggplot(aes(gdp_share_12m_change, power_demand_12m_change)) + geom_point() +
  geom_text_repel(aes(label=prov))


#demand growth vs starting level per capita




gdp_natl %>% group_by(price_basis, sector, subsector) %>%
  mutate(gdp_12m=zoo::rollapplyr(Value, 4, sum, fill=NA)) %>%
  filter(price_basis=='Current Prices') %>% filter(sector!='Total', grepl(' Industry', sector)) %>%
  ggplot(aes(date, gdp_12m, fill=sector)) + geom_area(position='fill')
