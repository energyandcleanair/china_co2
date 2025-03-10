source('scripts/load_package.R')
require(units)
require(sf)
#Sys.setenv(GIS_DIR='~/GIS')

output_dir <- 'outputs/province_clean_power_analysis'
last_month <- '2024-12-31'

#define function to extrapolate wind capacity and generation
pwr_data <- read_power_generation()

extrapolate_last_month <- function(provdata_filled) {
  provdata_filled %>% filter(var=='Capacity') %>% mutate(Value=na.cover(Value, Value1m)) -> cap

  cap %>% filter(month(date)==12) %>% mutate(last_year=year(date)) %>%
    select(prov, source, last_year, year_end_capacity=Value) -> year_end_cap

  last_month_added_cap <- pwr_data$monthly %>% filter(var=='Capacity', source=='Wind') %>%
    ungroup %>%
    mutate(added = Value-lag(Value)) %>%
    filter(date == last_month) %>% select(added)

  cap %<>% mutate(last_year=year(date)-1) %>% left_join(year_end_cap) %>%
    group_by(prov, source, month=month(date)) %>%
    mutate(capacity_added_YTD=pmax(0, Value-year_end_capacity)) %>%
    group_by(source, date) %>%
    mutate(share_of_capacity_added_YTD=capacity_added_YTD/sum(capacity_added_YTD)) %>%
    group_by(source, year(date)) %>%
    mutate(capacity_added_YTD=na.cover(capacity_added_YTD,
                                       lag(capacity_added_YTD) +
                                         last_month_added_cap$added *
                                         lag(share_of_capacity_added_YTD)),
           Value=na.cover(Value, year_end_capacity+capacity_added_YTD)) %>%
    group_by(prov, source) %>% fill(Value, .direction='down') %>%
    mutate(Value1m=Value) %>%
    filter(source=='Wind', date==last_month) %>% ungroup

  util_yoy <- pwr_data$monthly %>% ungroup %>%
    filter(var=='Utilization', source=='Wind', month(date)==month(last_month)) %>%
    mutate(yoy = Value1m/lag(Value1m)) %>%
    filter(date == last_month) %>% select(yoy)

  util <- provdata_filled %>% filter(var=='Utilization', source=='Wind', date=='2023-12-31') %>%
    mutate(date=ymd('2024-12-31'), Value1m=Value1m*util_yoy$yoy)

  cap %>% filter(date=='2024-12-31', source=='Wind') %>%
    select(-contains('capacity_added')) %>%
    bind_rows(util) -> newdata

  provdata_filled %>% anti_join(newdata  %>% distinct(var, source, date)) %>% bind_rows(newdata)
}

source('scripts/load_province_generation_data.R')


save.image(file.path(output_dir, 'alldata.RData'))
load(file.path(output_dir, 'alldata.RData'))



fuel_cols <- crea_palettes$CREA[c('Light.gray', 'Blue', 'Dark.red', 'Dark.violet', 'Orange', 'Green')]
names(fuel_cols) <- c('Fossil', 'Hydro', 'Nuclear', 'Wind', 'Solar', 'Bioenergy')

plotdata %>% filter(variant=='Utilization at trend', source!='Thermal', date<=last_month) %>% #Utilization at trend | Actual utilization
  ggplot(aes(date, Value_rollmean_12m, fill=source)) + geom_area(position='fill') + facet_wrap(~prov) +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(expand=expansion(mult=0), labels=scales::percent) +
  snug_x_date +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title='Power generation mix by province', subtitle='12-month rolling mean', x='', y='') -> p
quicksave(file.path(output_dir, 'Power generation mix by province.png'), plot=p)

plotdata %>% filter(variant=='Actual utilization', source!='Thermal', date<=last_month) %>% #Utilization at trend | Actual utilization
  ggplot(aes(date, Value1m, fill=source)) + geom_area(position='fill') + facet_wrap(~prov) +
  scale_fill_manual(values=fuel_cols) +
  scale_y_continuous(expand=expansion(mult=0), labels=scales::percent) +
  snug_x_date +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title='Power generation mix by province', subtitle='monthly', x='', y='') -> p
quicksave(file.path(output_dir, 'Power generation mix by province, monthly.png'), plot=p)


changes_total %>%
  select(prov, source, clean_energy_share_change=share) %>%
  mutate(north_south=add_region(prov)) %>%
  write_csv(file.path(output_dir, 'change by province, north vs south.csv')) %>%
  ggplot(aes(prov, clean_energy_share_change, fill=north_south)) + geom_col() + coord_flip() +
  scale_x_discrete(limits=changes_total$prov[order(changes_total$share)]) +
  ggthemes::theme_tufte() +
  theme(panel.grid.major.y = element_line(color='gray'),
        axis.ticks.y = element_blank()) +
  scale_fill_crea_d(col.index = c('Orange', 'Dark.red'), name='') +
  scale_y_continuous(labels=scales::percent) +
  labs(title='Change in the share of clean power generation by province',
       subtitle='from 2020 to 2024',
       x='', y='percentage-points')
quicksave(file.path(output_dir, 'change by province, north vs south.png'), logo=F, scale=.7)


plotdata %>% filter(variant=='Actual utilization', date<=last_month, source!='Thermal') %>% #Utilization at trend | Actual utilization
  mutate(region=add_region(prov), Value_rollmean_12m=Value_rollmean_12m*12/100e3) %>%
  group_by(region, source, date) %>% summarise(across(Value_rollmean_12m, ~sum(.x, na.rm=T))) %>%
  select(region, source, date, Value_rollmean_12m) %>%
  write_csv(file.path(output_dir, 'Power generation mix, north vs south.csv')) %>%
  ggplot(aes(date, Value_rollmean_12m, fill=source, col=source)) +
  geom_area(position='fill') +
  scale_y_continuous(expand=expansion(mult=0), labels=scales::percent) +
  #geom_line(linewidth=1) +
  #geom_area() +
  #x_at_zero(labels=scales::comma) +
  facet_wrap(~region) +
  scale_fill_manual(values=fuel_cols) +
  scale_color_manual(values=fuel_cols) +
  snug_x_date +
  ggthemes::theme_tufte() + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title='Power generation mix in Northern and Southern China', subtitle='12-month rolling mean', x='', y='TWh') -> p
quicksave(file.path(output_dir, 'Power generation mix, north vs south.png'), plot=p, logo=F, scale=.7)

changes %>% filter(source!='Fossil') %>%
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


#changes by region

for(reg in unique(region_dict$region)) {
  provs <- region_dict$prov[region_dict$region==reg]
  changes %>% filter(source!='Fossil', prov %in% provs) %>%
    ggplot(aes(prov, share, fill=source)) +
    geom_col() +
    geom_point(data=changes_total %>% filter(prov %in% provs),
               aes(shape='Total'), size=2, col=crea_palettes$CREA['Red']) +
    coord_flip() +
    scale_x_discrete(limits=changes_total %>% filter(prov %in% provs) %>% arrange(share) %>% use_series(prov)) +
    scale_shape_manual(values=16, name='') +
    labs(title=paste('Changes in the share of clean power generation in the', reg),
         subtitle='From 2020 to 2024',
         x='', y='percentage-points') +
    ggthemes::theme_tufte() +
    scale_fill_manual(values=fuel_cols) +
    scale_y_continuous(labels=scales::percent) -> p
  quicksave(file.path(output_dir, paste0('Changes in power generation mix, ',reg,'.png')), plot=p, logo=F, scale=.6)
}



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


shares %>% filter(source!='Fossil') %>% group_by(prov, date) %>%
  summarise(across(share, sum)) %>%
  filter(date %in% c(ymd('2020-12-31'), last_month)) ->
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


changes %>% filter(variant=='Utilization at trend') %>%
  select(prov, source, share) %>%
  spread(source, share) %>% copy.xl()

changes %>% filter(variant=='Utilization at trend', prov %in% c('Heilongjiang', 'Jilin', 'Liaoning')) %>%
  group_by(source) %>% summarise(across(Value_rollmean_12m, sum)) %>% arrange(desc(Value_rollmean_12m))


#clean share on national level
ember <- get_ember_monthly_data()
ember %>% filter(Area=='China', Unit=='TWh', Variable %in% c('Clean', 'Total Generation'), year(Date) %in% c(2015, 2020, 2024)) %>%
  group_by(year=year(Date), Variable) %>% summarise(across(Value, sum)) %>% summarise(share=round(Value[1]/Value[2], 2))


plotdata %>% filter(variant=='Actual utilization', date<=last_month, source!='Thermal') %>% #Utilization at trend | Actual utilization
  mutate(prov=add_region(prov)) %>%
  group_by(prov, date, source) %>%
  summarise(across(Value_rollmean_12m, ~sum(.x, na.rm=T))) %>%
  mutate(clean_share=1-Value_rollmean_12m/sum(Value_rollmean_12m)) %>%
  filter(source=='Fossil', date %in% c('2015-12-31','2020-12-31',last_month))

#Analysis of factors

#[For each province section, look at GDP growth, demand growth, generation growth, added capacity by technology and notable projects if any]
#TO DO: extrapolate wind; extract added capacity

#target dataset: increase in capacity, increase in generation, GDP growth, GDP per capita, industrial output & growth
#table: clean change, coal power capacity change, starting share of hydro, GDP growth, power demand growth, population density, wind&solar conditions..?

#coal capacity changes
provdata_filled %>% group_by(prov) %>% filter(!is.na(Value1m)) %>%
  filter(source=='Thermal', var %in% c('Capacity', 'Generation'), variant=='Utilization at trend') %>%
  mutate(value_to_plot = ifelse(var=='Capacity', Value1m, Value_rollmean_12m),
         Generation_volume=value_to_plot[var=='Generation' & date==last_month]) %>%
  group_by(prov, var, Generation_volume) %>% summarise(change=value_to_plot[date==max(date)]/value_to_plot[date=='2015-12-31']-1) %>%
  spread(var, change) -> coal_changes

#wind&solar utilization
provdata_filled %>%
  mutate(value_to_plot = ifelse(var == 'Capacity', Value1m, Value_rollmean_12m)) %>% #Value1m
  ungroup %>%
  filter(source %in% c('Wind', 'Solar'), var %in% c('Capacity', 'Utilization'),
         date==last_month, variant=='Utilization at trend') %>%
  select(prov, source, var, value_to_plot) %>%
  spread(var, value_to_plot) %>%
  mutate(Utilization=Utilization*12/8760) ->
  utilization

utilization %>%
  group_by(source) %>%
  mutate(ratio=Utilization/weighted.mean(Utilization, Capacity)) %>%
  select(prov, source, ratio) %>%
  mutate(source=paste0(source, '_utilization_percent_of_national_average')) %>%
  spread(source, ratio) ->
  utilization_rating

utilization %>% mutate(source=paste0(source, '_utilization')) %>%
  select(prov, source, Utilization) %>%
  spread(source, Utilization) ->
  utilization_wide

#population density
adm1 %>% mutate(area_km2 = as.numeric(st_area(.)/1e6),
                prov=fix_province_names(NAME_1)) %>%
  st_drop_geometry() %>%
  select(prov, area_km2) %>%
  left_join(pop %>% filter(year==2023)) %>%
  mutate(pop_per_km2=pop*1e4/area_km2) -> pop_density


#generation growth
years=as.numeric(ymd(last_month) - ymd(base_date))/365
provdata_filled %>% filter(prov!='Tibet') %>%
  filter(var=='Generation', variant=='Utilization at trend', !is.na(Value_rollmean_12m)) %>%
  group_by(prov, var, variant, date) %>%
  summarise(across(Value_rollmean_12m, sum)) %>%
  summarise(generation_growth=(Value_rollmean_12m[date==last_month]/Value_rollmean_12m[date=='2020-12-31'])^(1/years)-1,
            total_generation_TWh=Value_rollmean_12m[date==last_month]*12/1e5) ->
  generation_growth

#demand growth
power_demand %>%
  filter(month(date) %in% 3:12, year(date) %in% c(2020, 2024)) %>%
  group_by(prov, var, year=year(date)) %>%
  summarise(across(Value, sum)) %>%
  summarise(demand_growth=(Value[year==2024]/Value[year==2020])^(1/4)-1) ->
  demand_growth

#GDP growth
base_date=ymd('2020-12-31')
years = as.numeric(round((max(gdp_prov_total$date) - base_date)/365*4, 0)/4)

gdp_prov_total %>% ungroup %>%
  group_by(prov) %>%
  summarise(gdp_latest=Value[date==max(date)],
            gdp_base=Value[date==base_date],
            gdp_growth_rate=(gdp_latest/gdp_base)^(1/years)-1,
            gdp_per_capita=(Value/pop)[date==max(date)]) %>%
  ungroup  ->
  gdp_growth

gdp_growth %>% summarise(across(is.numeric, sum)) %>% mutate(gdp_growth_rate=(gdp_latest/gdp_base)^(1/years)-1)

#predicted utilization of wind and solar, i.e. quality of the resource
atlas_cf <- read_csv('outputs/solar_wind_predicted_utilization_by_province_from_Global_Atlas.csv') %>%
  select(prov=province, energy_source, atlas_CF) %>%
  mutate(energy_source=energy_source %>% gsub(' ', '_', .) %>% paste0('_utilization_predicted')) %>%
  spread(energy_source, atlas_CF) %>%
  mutate(across(is.numeric, ~pmax(.x, 0, na.rm=T)))



#join
list(changes_total %>% select(prov, clean_share_increase=share),
     shares %>% filter(variant=='Utilization at trend', date==base_date, source!='Fossil') %>%
       summarise(across(share, sum)) %>% select(prov, clean_share_2020=share),
     coal_changes %>% select(prov, thermal_capacity_increase=Capacity, thermal_generation_increase=Generation),
     #utilization_rating,
     utilization_wide,
     pop_density %>% select(prov, pop_per_km2),
     generation_growth %>% ungroup %>% select(prov, generation_growth, total_generation_TWh),
     demand_growth %>% select(-var),
     gdp_growth %>% select(prov, gdp_growth_rate, gdp_per_capita),
     atlas_cf) %>%
  Reduce(full_join, .) ->
  prov_table

prov_table %<>% mutate(Solar_utilization_to_potential=Solar_utilization/Solar_utilization_predicted,
                       Wind_utilization_to_potential=Wind_utilization/Wind_utilization_predicted,
                       north_south=add_region(prov))

prov_table %>% arrange(desc(clean_share_increase)) %>% select(prov, north_south, everything()) %>% copy.xl()

prov_table %>% select(-Wind_utilization) %>%
  pivot_longer(-c(prov, north_south, clean_share_increase)) %>%
  group_by(name) %>%
  mutate(cor=cor(value, clean_share_increase, use='complete.obs'),
         name=gsub('_percent_of.*', '', name)) %>%
  ggplot(aes(value, clean_share_increase)) +
  geom_point(aes(col=north_south), size=.5) +
  geom_smooth(method='gam') +
  geom_text_repel(aes(label=prov), size=1, alpha=.5) +
  facet_wrap(~name, scales='free_x') +
  geom_text(aes(label = paste0("Correlation: ", round(cor, 2))), x = Inf, y = Inf, hjust = 1.1, vjust = 1.3, inherit.aes = FALSE,
            size=2, alpha=.5) -> p
quicksave(file.path(output_dir, 'correlations.png'), plot=p)



prov_table %>% select(-Wind_utilization) %>%
  pivot_longer(-c(prov, north_south, clean_share_increase)) %>%
  mutate(name=gsub('_percent_of.*', '', name)) %>%
  ggplot(aes(north_south, value)) +
  geom_violin() +
  facet_wrap(~name, scales='free_y')



utilization %>% select(prov, source, Capacity) %>% spread(source, Capacity) %>%
  full_join(prov_table, .) ->
  prov_table_w_capacity

prov_table_w_capacity %>%
  ggplot(aes(Solar, Solar_utilization_to_potential)) + geom_point() + geom_smooth(method='lm') +
  geom_text_repel(aes(label=prov), size=2)

prov_table_w_capacity %>%
  ggplot(aes(Solar_utilization_predicted, Solar)) + geom_point() + geom_smooth(method='lm') +
  geom_text_repel(aes(label=prov), size=2)

prov_table_w_capacity %>%
  ggplot(aes(Wind_Onshore_utilization_predicted, Wind)) + geom_point() + geom_smooth(method='lm') +
  geom_text_repel(aes(label=prov), size=2)

prov_table_w_capacity %>%
  ggplot(aes(Wind_utilization_predicted, Wind_utilization)) + geom_point() + geom_smooth(method='lm') +
  geom_text_repel(aes(label=prov), size=2)

prov_table_w_capacity %>%
  ggplot(aes(Wind_utilization_predicted, Wind_utilization_to_potential)) + geom_point() + geom_smooth(method='lm') +
  geom_text_repel(aes(label=prov), size=2)


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


#hydropower penetration vs increase in non-hydro clean
#todo: split into groups by hydro share
provdata_filled %>% ungroup %>%
  filter(source!='Thermal', var=='Generation', variant=='Utilization at trend', date=='2020-12-31') %>%
  group_by(prov) %>% mutate(hydro_share=Value_rollmean_12m/sum(Value_rollmean_12m, na.rm=T)) %>%
  replace_na(list(hydro_share=0)) %>%
  filter(source=='Hydro') %>% select(prov, hydro_share) %>%
  left_join(changes %>%
              filter(variant=='Utilization at trend') %>%
              select(prov, source, share_change=share)) ->
  hydro_share_analysis

hydro_share_analysis %>% lm(share_change~hydro_share:source+source, data=.) %>% summary

hydro_share_analysis %>%
  ggplot(aes(hydro_share, share_change)) + geom_point() + facet_wrap(~source) + geom_smooth(method='lm')

hydro_share_analysis %>% mutate(bin=cut(hydro_share, c(0,.4,1), include.lowest=T)) %>%
  group_by(bin, source) %>% summarise(across(share_change, mean)) %>%
  ggplot(aes(bin, share_change)) + geom_col() + facet_wrap(~source)

