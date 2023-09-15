source('R/wind mapping functions.R')

require(creahelpers)
require(rcrea)


in_file = "data/Peak Load by Power Grid.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, c('var', 'grid'), columnFilter = 'Peak Load.*Consumption', read_vardata = T) ->
  peakloads

readwindEN(in_file, c('var', 'grid'), columnFilter = 'Transmission', read_vardata = T) %>%
  separate(grid, c('grid_from', 'grid_to'), sep=' [^(a-zA-Z )] ') %>%
  mutate(across(starts_with('grid'), function(x) x %>% gsub(' China', '', .) %>% recode('Huazhong'='Central'))) ->
  transmission

transmission %>% #filter(date>='2019-01-01') %>%
  mutate(Value = Value / 100 / 24 / days_in_month(date), Unit='GW') %>%
  group_by(Unit, grid_to, grid_from) %>%
  summarise(across(Value, max)) %>%
  summarise(across(Value, max))


#calculate transmission from province import/export
read_xlsx('data/grids.xlsx', sheet='grid_definitions') %>% rename(prov=provEN) -> grids
read_xlsx('data/grids.xlsx', sheet='province_splits') %>% rename(prov=provEN)  %>%
  filter(year==max(year, na.rm = T)) %>% select(-year) %>%
  pivot_longer(is.numeric, names_to='prod', values_to='capacity_platts') -> splits

splits %>% filter(prod=='Thermal') %>%
  group_by(prov) %>%
  mutate(share = capacity_platts / sum(capacity_platts)) -> splits_total


in_file="data/power generation by province and source.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T, columnExclude = 'Consumption', zero_as_NA = T) -> provgen
readwindEN(in_file, c('var', 'prov'), read_vardata = T, columnFilter = 'Consumption', zero_as_NA = T) -> provcons

provgen %>%
  mutate(var='generation') %>%
  bind_rows(provcons) %>%
  group_by(prov, var, source) %>%
  unYTD %>%
  group_by(prov, var, date) %>%
  summarise(across(Value1m, sum, na.rm=T)) ->
  provgen_totals

provgen_totals %<>% mutate(year=year(date), month=month(date), Value1m=Value1m %>% (function(x) {x[x==0]<-NA;x}))
provgen_totals %<>% group_by(prov, var, year) %>%
  summarise(year_median=median(Value1m, na.rm=T)) %>%
  left_join(provgen_totals, .)

provgen_totals %<>% mutate(month_ratio = Value1m/year_median) %>%
  group_by(prov, var, month) %>%
  summarise(across(month_ratio, median, na.rm=T)) %>%
  left_join(provgen_totals, .)

provgen_totals %<>% group_by(prov, var) %>%
  mutate(Value1m_norm = year_median*month_ratio,
         is_mad = is.outlier(Value1m/Value1m_norm, SD=5, na.rm=T))

provgen_totals %>% filter(!is_mad) %>% ggplot(aes(date, Value1m, col=var)) + geom_line() + facet_wrap(~prov, scales='free_y')

provgen_totals %>% filter(year(date)>=2010) %>%
  group_by(prov, date) %>%
  summarise(exports=Value1m[var=='generation']-Value1m[var=='Power Consumption']) ->
  provexp

provexp %>%
  left_join(splits_total) %>%
  mutate(exports = exports * ifelse(is.na(subgrid), 1, share),
         prov = ifelse(is.na(subgrid), prov, subgrid)) %>%
  left_join(grids) %>%
  group_by(grid, date) %>%
  summarise(across(exports, sum)) %>%
  mutate(transmission_capacity=cummax(abs(exports)/100/24/days_in_month(date))/(5500/8760)) ->
  provexp_cap

provexp_cap %<>% group_by(grid) %>% fill(transmission_capacity)

in_file = "data/power capacity by province&type.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)
readwindEN(in_file, c('var', 'prod', 'prov'), read_vardata = T) %>%
  mutate(prod=gsub(' ?power.*', '', prod, ignore.case=T)) ->
  cap_prov

cap_prov %<>% filter(prov != 'National') %>% group_by(date, prod) %>%
  summarise(across(Value, sum)) %>% mutate(prov='National') %>%
  bind_rows(cap_prov %>% filter(prov != 'National'))


splits %>% inner_join(cap_prov, .) %>%
  group_by(date, var, prod, prov) %>%
  mutate(Value = Value * capacity_platts / sum(capacity_platts),
         prov=subgrid) %>%
  bind_rows(cap_prov %>% anti_join(splits)) ->
  cap

cap %>% left_join(grids %>% select(prov, grid)) %>%
  mutate(grid=ifelse(prov=='National', 'Total', grid)) %>%
  group_by(grid, prod, date) %>%
  summarise(across(Value, sum)) %>%
  mutate(var='Electric capacity') ->
  cap_bygrid

#minimum hydropower utilization
provgen %>% group_by(var, prod=source, prov) %>% unYTD %>%
  mutate(prod=disambiguate(prod, unique(cap_prov$prod))) ->
  prov_cf

prov_cf %<>% group_by(var, prod) %>% summarise(across(Value1m, sum)) %>%
  mutate(prov='National') %>% bind_rows(prov_cf)

cap_prov %>% select(date, prod, prov, capacity_10kW=Value) %>%
  inner_join(prov_cf %>% select(date, prod, prov, output_10kWh=Value1m)) ->
  cap_gen

cap_gen %>%
  mutate(utilization = output_10kWh / (capacity_10kW * 24 * days_in_month(date))) %>%
  filter(utilization>0, utilization<1) -> cf

cf %>% filter(prod=='Hydro') %>%
  ggplot(aes(as.factor(month(date)), utilization)) + geom_boxplot() + facet_wrap(~prov)

prod_to_adjust=c('Hydro', 'Solar', 'Wind')
cap_gen %>% filter(prod %in% prod_to_adjust) %>%
  inner_join(splits %>% select(subgrid, prov, prod, capacity_platts)) %>%
  group_by(date, prov, prod) %>%
  mutate(across(c(capacity_10kW, output_10kWh), ~.x*capacity_platts/sum(capacity_platts)),
         prov=subgrid) %>%
  bind_rows(cap_gen %>% anti_join(splits) %>% filter(prod %in% prod_to_adjust)) %>%
  left_join(grids) -> cap_gen_grid

cap_gen_grid %<>%
  group_by(date, prod) %>%
  summarise(across(c(capacity_10kW, output_10kWh), sum)) %>%
  mutate(grid='Total') %>%
  bind_rows(cap_gen_grid)

cap_gen_grid %>%
  group_by(date, prod, grid) %>%
  summarise(across(c(capacity_10kW, output_10kWh), sum)) %>%
  mutate(utilization = output_10kWh / (capacity_10kW * 24 * days_in_month(date))) %>%
  filter(utilization>0, utilization<1) -> cf_grid

cf_grid %>% filter(year(date)>=2010) %>%
  ggplot(aes(as.factor(month(date)), utilization)) + geom_violin() + facet_grid(prod~grid)

cf_grid %>% filter(year(date)>=2010) %>%
  group_by(grid, month=month(date), prod) %>%
  summarise(across(utilization, min)) ->
  min_cf

cummax <- function(x) {
  for(i in seq_along(x)[-1]) x[i] <- max(x[i-1], x[i], na.rm=T)
  return(x)
}


peakloads %>%
  mutate(grid=recode(grid, WALTON='East'), var='Peak load', prod='Peak load') %>%
  bind_rows(cap_bygrid) %>%
  mutate(Value=Value/100, Unit='GW') %>%
  bind_rows(provexp_cap %>% select(grid, date, Value=transmission_capacity) %>%
              mutate(prod='Transmission', var='Electric capacity')) %>%
  group_by(prod,grid) %>%
  mutate(grid=gsub(' China', '', grid),
         type=ifelse(grepl('Wind|Solar', prod), 'Variable', 'Dispatchable'),
         max_to_date=cummax(Value)) ->
  peak_balances


peak_balances %<>% mutate(month=month(date)) %>%
  left_join(min_cf %>% mutate(grid=gsub(' China', '', grid))) %>%
  mutate(across(c(max_to_date_adj = max_to_date, Value_adj=Value), multiply_by, pmin(utilization, 1, na.rm=T)),
         Value_adj = ifelse(prod %in% c('Nuclear', 'Thermal'), max_to_date, Value_adj),
         prod=factor(prod, levels=c('Peak load', 'Thermal', 'Nuclear', 'Hydro', 'Transmission', 'Wind', 'Solar') %>% rev))

peak_balances %>%
  filter(date==max(date)) %>%
  ggplot(aes(var, max_to_date_adj, fill=prod, alpha=type)) +
  geom_col() + facet_wrap(~grid, scales='free_y') +
  theme_crea() + scale_fill_manual(values=c(unname(crea_palettes$change[1:2]), 'gray', unname(crea_palettes$dramatic))) +
  labs(title='Available capacity and peak loads in China', y='GW', x='', fill='source') +
  scale_alpha_manual(values=c(1, .45)) +
  x_at_zero()

peak_balances %>% filter(date==max(date)) %>% group_by(grid, var) %>% summarise(across(max_to_date_adj, sum)) %>%
  summarise(reserve_ratio = max_to_date_adj[1]/max_to_date_adj[2]-1)

peak_balances %>% filter(prod=='Peak load') %>%
  group_by(grid, year=year(date)) %>%
  summarise(across(Value, max)) %>%
  mutate(rel=Value/max(Value)) %>%
  ggplot(aes(year, Value, fill=rel)) + geom_col() +
  facet_wrap(~grid, scales='free_y') +
  x_at_zero() + labs(title='Annual peak loads by grid', y='GW') +
  theme_crea() +
  scale_fill_crea_c('change', guide='none')


peak_balances %>% filter(prod=='Peak load', Value>0) %>%
  ggplot(aes(date, Value_adj)) +
  geom_area(data=peak_balances %>% filter(prod!='Peak load'),
            mapping=aes(fill=prod)) +
  geom_line(mapping=aes(col=prod), linewidth=1) +
  facet_wrap(~grid, scales='free_y') +
  scale_x_date(limits = c(ymd('2010-01-01', NA)), expand=expansion()) +
  x_at_zero() +
  scale_color_manual(values='gray', name='') +
  theme_crea() +
  scale_fill_crea_d('change', col.index=c(2:3, 1, 5,6,7), name='') +
  labs(title='Monthly peak loads and available capacity by grid', y='GW', x='') -> plt
quicksave(file.path(output_dir, 'Peak loads and available capacity by grid.png'), plot=plt, width=8*1.2, height=6*1.2, dpi=300/1.2)

peakloads %>% mutate(plotdate = date %>% 'year<-'(2022),
                     year=as.factor(year(date)),
                     grid=recode(grid, WALTON='East')) %>%
  filter(Value>0) %>%
  ggplot(aes(plotdate, Value/100, col=year)) + facet_wrap(~grid, scales='free_y') + geom_line() +
  theme_crea() + scale_color_manual(values=colorRampPalette(crea_palettes$change)(17)) +
  labs(title='Peak loads by grid', x='', y='GW')





read_csv("https://api.energyandcleanair.org/v1/weather?variable=CDD,HDD&region_type=gadm1&format=csv&region_iso2=CN") ->
  dd

require(raster)
getadm(1, 'coarse') %>% subset(NAME_0=='China') -> adm
getpop("count") -> pop

pop %<>% crop(adm)
pop %>% aggregate(10, sum, na.rm=T) -> pop10km
pop %>% extract(adm, sum, na.rm=T) -> admpop

adm$pop <- admpop %>% as.vector()

adm@data %>% rename(region_id=GID_1) %>% left_join(dd, .) -> dd2

dd2 %>% group_by(date, variable) %>% summarise(across(value, ~weighted.mean(.x, w=pop))) %>%
  mutate(NAME_1='National') %>%
  bind_rows(dd2) ->
  dd2

dd2 %>% filter(year(date)<=2022, NAME_1 %in% c('National', 'Sichuan', 'Chongqing', 'Anhui', 'Jiangsu', 'Zhejiang', 'Shanghai'), variable=='CDD') %>%
  group_by(NAME_1, variable) %>%
  mutate(across(value, rollmeanr, fill=NA, k=7)) %>%
  group_by(NAME_1, year=year(date), variable) %>%
  summarise(across(value, max, na.rm=T)) %>%
  ggplot(aes(year, value)) +
  geom_smooth(method='lm', color=crea_palettes$dramatic[1]) +
  geom_line(linewidth=1, color=crea_palettes$dramatic["Orange"]) +
  facet_wrap(~NAME_1) +
  labs(title='Average cooling degrees, hottest week of the year', subtitle='population-weighed', x='', y='degrees Celcius') +
  theme_crea(plot.margin=unit(c(1, 1, .25, .5), 'line')) +
  scale_x_continuous(expand = expansion(), breaks = function(x) seq(x[1]+2, x[2], 2)) -> plt
quicksave(file.path(output_dir, 'Average cooling degrees.png'), plot=plt, width=8*1.2, height=6*1.2, dpi=300/1.2)



#output data by grid region
peak_balances %>% filter(date==max(date)) %>%
  mutate(max_to_date=ifelse(!is.finite(max_to_date), 0, Value)) %>%
  group_by(grid, var, prod) %>%
  summarise(across(max_to_date, sum)) %>% ungroup %>% select(-var) %>%
  spread(prod, max_to_date, fill=0) %>% set_names(make.names(names(.))) %>%
  mutate(reserve_ratio=(Hydro+Nuclear+Thermal)/Peak.load,
         reserve_ratio_no_hydro=(Nuclear+Thermal)/Peak.load)
