source('scripts/load_package.R')

output_dir <- 'outputs/province_clean_power_analysis'


source('scripts/load_province_generation_data.R')
#OR: load(file.path(output_dir, 'alldata.RData'))

#paste.xl() %>% write_csv(file.path(output_dir, 'province_solar_capacity_by_subtype.csv'))

read_csv(file.path(output_dir, 'province_solar_capacity_by_subtype.csv')) %>%
  pivot_longer(-prov) %>% separate(name, c('variable', 'subtype', 'year'), '_') %>%
  mutate(across(c(year, value), as.numeric), unit='10MW') %>%
  filter(prov!='Xinjiang Corps') ->
  prov_solar_cap

#read WIND investment data
readwindEN(get_data_file("Completed_Investment_in_Power_Supply_Construction.xlsx"), c('var', 'energy_source'), read_vardata = T) -> inv



#https://globalenergymonitor.org/wp-content/uploads/2024/04/Global-Hydropower-Tracker-April-2024.xlsx
read_gem("Global-Hydropower-Tracker-April-2024.xlsx", 2:3) -> gem_hydro

#https://globalenergymonitor.org/wp-content/uploads/2024/09/Global-Bioenergy-Power-Tracker-GBPT-September-2024.xlsx
read_gem('Global-Bioenergy-Power-Tracker-GBPT-September-2024.xlsx', 2:3) %>%
  mutate(energy_source='Bioenergy') -> gem_bio

#https://globalenergymonitor.org/wp-content/uploads/2024/07/Global-Nuclear-Power-Tracker-July-2024.xlsx
read_gem("Global-Nuclear-Power-Tracker-July-2024.xlsx", 2) %>%
  mutate(energy_source='Nuclear',
         construction_start_year=construction_start_date %>% gsub('-.*', '', .) %>% as.numeric) -> gem_nuclear


#https://globalenergymonitor.org/wp-content/uploads/2025/02/Global-Wind-Power-Tracker-February-2025.xlsx
bind_rows(read_xlsx(file.path(gem_dir, "Global-Wind-Power-Tracker-February-2025.xlsx"), sheet=2),
          read_xlsx(file.path(gem_dir, "Global-Wind-Power-Tracker-February-2025.xlsx"), sheet=3)) %>%
  set_names(make_names(names(.))) %>%
  mutate(energy_source='Wind') -> gem_wind

gem_wind %>% filter(country_area=='China', status=='operating') %>%
  group_by(prov=state_province, energy_source=ifelse(grepl('Offshore', installation_type), 'Offshore wind', 'Onshore wind'), year=start_year) %>%
  summarise(across(c(MW=capacity__mw_), sum)) ->
  wind_construction

#solar and onshore wind based on capacity added
provdata_filled %>% filter(var=='Capacity', variant=='Actual utilization', month(date)==12) %>%
  group_by(prov, source) %>%
  mutate(MW=(Value-lag(Value))*10, year=year(date)) %>%
  select(prov, energy_source=source, year, MW) ->
  cap_yearly


#subtract offshore wind
cap_yearly %<>% filter(energy_source=='Wind') %>%
  bind_rows(wind_construction) %>% group_by(prov, year) %>%
  summarise(MW=MW[energy_source=='Wind']-if_null(MW[energy_source=='Offshore wind'], 0)) %>%
  mutate(energy_source='Onshore wind') %>%
  bind_rows(cap_yearly) %>%
  bind_rows(wind_construction %>% filter(energy_source=='Offshore wind'))

#add solar
prov_solar_cap %>% filter(prov!='Total', variable=='new', subtype %in% c('centralized', 'distributed')) %>%
  mutate(energy_source=paste(capitalize_first(subtype), 'solar'), MW=value*10) %>%
  select(prov, energy_source, year, MW) %>%
  bind_rows(cap_yearly) -> cap_yearly

cap_yearly %<>% mutate(investment_cost = case_when(energy_source=='Onshore wind'~5.5,
                                                   energy_source=='Offshore wind'~12,
                                                   energy_source=='Centralized solar'~3.4,
                                                   energy_source=='Distributed solar'~4.13),
                       investment = investment_cost * MW/100,
                       Unit='CNY 100mn')


#nuclear and hydro investment from wind
inv %<>% mutate(energy_source=disambiguate(energy_source, c('Wind', 'Solar', 'Nuclear', 'Hydro')))
inv %<>% filter(month(date)==12) %>%  mutate(year=year(date))

gem_hydro %>% filter(country_1=='China', status == 'construction' | start_year<=2024) -> t1


2009:2024 %>% lapply(function(yr) {
  t1 %>% filter((start_year>=yr & start_year<yr+4) | (is.na(start_year) & yr>2020 & status=='construction')) %>%
    group_by(prov=state_province_1, energy_source=ifelse(grepl('pumped', technology_type), 'Pumped storage', 'Hydro')) %>%
    summarise(across(capacity__mw_, sum)) %>% mutate(year=yr)
}) %>% bind_rows() -> hydro_uc



2009:2024 %>% lapply(function(yr) {
  gem_bio_scaled %>% filter((start_year>=yr & start_year<yr+2) | (is.na(start_year) & yr>2021 & status=='construction')) %>%
    group_by(prov=state_province, energy_source) %>%
    summarise(across(capacity__mw_, sum)) %>% mutate(year=yr)
}) %>% bind_rows() -> bio_uc

gem_nuclear %>% filter(country_area=='China', status == 'construction' | start_year<=2024) -> t2

2009:2024 %>% lapply(function(yr) {
  t2 %>% filter(start_year>=yr & construction_start_year<=yr) %>%
    group_by(prov=state_province) %>% summarise(across(capacity__mw_, sum)) %>% mutate(year=yr)
}) %>% bind_rows() -> nuclear_uc



bind_rows(hydro_uc %>% filter(energy_source=='Hydro'),
          nuclear_uc %>% mutate(energy_source='Nuclear')) %>%
  rename(MW=capacity__mw_) %>%
  group_by(energy_source, year) %>% mutate(share=MW/sum(MW)) %>%
  inner_join(inv %>% select(energy_source, year, investment_national=Value, Unit)) %>%
  mutate(investment=investment_national * share) ->
  inv_hydro_nuclear

#pumped hydro and biomass investment based on GEM
#https://www.irena.org/Publications/2024/Sep/Renewable-Power-Generation-Costs-in-2023
hydro_uc %>%
  filter(energy_source=='Pumped storage') %>%
  mutate(investment_cost=6.4/3) %>%
  bind_rows(bio_uc %>% mutate(investment_cost=13)) %>%
  rename(MW=capacity__mw_) %>%
  mutate(investment=investment_cost*MW/100,
         Unit='CNY 100mn') ->
  inv_pumped


#battery storage investment
#paste.xl() %>% write_csv(file.path(output_dir, 'battery_capacity_additions_MWh.csv'))

read_csv(file.path(output_dir, 'battery_capacity_additions_MWh.csv')) %>%
  pivot_longer(-prov, names_to='year', values_to='MW') %>%
  mutate(across(year, force_numeric),
         investment_cost=3.5,
         investment=investment_cost*MW/100,
         Unit='CNY 100mn',
         energy_source='Battery storage') ->
  inv_battery


#put investment together
bind_rows(cap_yearly %>% filter(!is.na(investment_cost)),
          inv_hydro_nuclear, inv_pumped, inv_battery) ->
  investment_all


#value of power generation
#extrapolate full year

provdata_filled %>% filter(var == 'Generation, calculated') %>% rename(energy_source=source) %>%
  mutate(year=year(date)) -> gen

gen %>% group_by(variant, prov, energy_source) %>%
  filter(Value1m>0) %>% filter(month(date)<=month(max(date))) %>%
  group_by(variant, prov, energy_source, year) %>% summarise(across(Value1m, sum)) %>%
  mutate(yoy=Value1m/lag(Value1m)) -> gen_ytd

gen %>%
  filter(year>=2015, var == 'Generation, calculated') %>%
  group_by(variant, prov, energy_source, year) %>%
  summarise(across(c(generation=Value1m), sum)) %>%
  left_join(gen_ytd %>% select(-Value1m)) %>%
  mutate(generation=na.cover(generation, yoy*lag(generation))) ->
  gen_yearly

gen_yearly %>%
  filter(variant=='Actual utilization') %>%
  #filter(energy_source!='Thermal') %>%
  #group_by(prov, energy_source) %>% mutate(generation=generation-generation[year==2020]) %>%
  ungroup %>%
  mutate(prov=add_region(prov)) %>%
  mutate(energy_source=factor(energy_source, levels=sort(unique(energy_source))[c(3,5,2,1,4)])) %>%
  group_by(prov, energy_source, year) %>% summarise(across(generation, ~sum(.x, na.rm=T))) %>%
  #ggplot(aes(year, generation, col=prov)) + geom_line() + facet_wrap(~energy_source)
  ggplot(aes(year, generation, fill=energy_source)) +
  #geom_col(position='fill') +
  geom_col() +
  facet_wrap(~prov)

gen_yearly %>%
  filter(variant=='Actual utilization', energy_source!='Thermal') %>%
  mutate(value_of_output=0.449*generation/1e4) ->
  production


bind_rows(production %>% mutate(category='production') %>% rename(value=value_of_output),
          investment_all %>% mutate(category='investment') %>% rename(value=investment)) %>%
  mutate(north_south=add_region(prov)) %>%
  group_by(prov, category, energy_source) %>%
  arrange(year) %>%
  mutate(growth_sector=value-lag(value),
         label=paste0(category, ': ', energy_source)) %>%
  rename(value_sector=value) %>% ungroup ->
  cleantech_gdp

cleantech_gdp %>%
  filter(year>=2020) %>%
  ggplot(aes(year, growth_sector, fill=label)) +
  geom_col() + facet_wrap(~north_south)



#related to GDP growth


prov_gdp_growth %>%
  group_by(north_south, year) %>%
  summarise(across(contains('value'), sum)) ->
  northsouth_gdp_growth

prov_gdp_growth %>%
  inner_join(cleantech_gdp %>% select(-Unit)) %>%
  mutate(share_of_value=value_sector/value_real, share_of_growth=growth_sector/value_real_growth) %>%
  filter(year==2024) %>%
  ggplot(aes(prov, share_of_value, fill=label)) + geom_col() + coord_flip() + facet_wrap(~north_south, scales='free_y')


prov_gdp_growth %>%
  inner_join(cleantech_gdp %>% select(-Unit)) %>%
  group_by(north_south, prov, label) %>%
  summarise(across(contains('value'), ~.x[year==2024]-.x[year==2021])) %>%
  mutate(share_of_growth=value_sector/value_real) %>%
  ggplot(aes(prov, share_of_growth, fill=label)) + geom_col() + coord_flip() + facet_wrap(~north_south, scales='free_y')


cleantech_gdp %>% select(-Unit) %>%
  group_by(north_south, category, energy_source, label, year) %>%
  summarise(across(matches('value|growth'), ~sum(.x, na.rm=T))) %>%
  left_join(northsouth_gdp_growth) %>%
  mutate(share_of_value=value_sector/value_real, share_of_growth=growth_sector/value_real_growth) %>%
  filter(year>=2015) %>%
  ggplot(aes(year, share_of_value, fill=label)) + geom_col() + facet_wrap(~category+north_south)


cleantech_gdp %>% select(-Unit) %>%
  group_by(north_south, label, year) %>%
  summarise(across(matches('value|growth'), ~sum(.x, na.rm=T))) %>%
  left_join(northsouth_gdp_growth) %>%
  group_by(north_south, label) %>%
  summarise(across(contains('value'), ~.x[year==2024]-if_null(.x[year==2023], 0))) %>%
  mutate(share_of_growth=value_sector/value_real) %>%
  ggplot(aes(north_south, share_of_growth, fill=label)) + geom_col()




#export capacity addition data
cap_yearly %>% filter(year %in% 2021:2024) %>%
  group_by(prov, energy_source) %>%
  summarise(across(MW, sum)) %>%
  spread(energy_source, MW) %>%
  select(-Thermal) %>%
  copy.xl()

cap_yearly %>% filter(year %in% 2021:2024, prov %in% c('Heilongjiang', 'Jilin', 'Liaoning')) %>%
  group_by(energy_source) %>%
  summarise(across(MW, ~sum(.x, na.rm=T)))

gem_nuclear %>% filter(grepl('constr|announced', status), country_area=='China') %>%
  group_by(state_province, status) %>% summarise(across(capacity__mw_, sum)) %>%
  spread(status, capacity__mw_) %>%
  (function(df) {df$total <- rowSums(df[,-1]); df}) %>% arrange(desc(total)) %>% copy.xl()

gem_hydro
