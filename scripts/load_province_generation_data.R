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
           read_vardata = T) -> cap_util

readwindEN(get_data_file('power capacity by province&type.xlsx'),
           c('prov', 'var', 'source'),
           columnExclude = 'Wind|Solar',
           read_vardata = T) -> cap_all

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
  mutate(basis_for_data='reported') %>%
  complete(prov, nesting(source, var), date) %>%
  group_by(prov, source, var) %>%
  mutate(Value1m=ifelse(Value1m<=0, NA, Value1m) %>% na.approx(na.rm=F)) ->
  provdata_filled

if(exists('extrapolate_last_month'))
  provdata_filled %<>% extrapolate_last_month

#add utilization when missing
provdata_filled %<>% group_by(prov, source, date) %>%
  filter('Utilization' %notin% var) %>%
  summarise(Value1m=Value1m[var=='Generation']/Value1m[var=='Capacity'],
            basis_for_data=ifelse(all(basis_for_data=='reported'), 'reported', NA)) %>%
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
  summarise(Value1m=Value1m[var=='Capacity']*Value1m[var=='Utilization'],
            basis_for_data=ifelse(all(basis_for_data=='reported'), 'reported', NA)) %>%
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


# subtract biomass from thermal
#https://globalenergymonitor.org/wp-content/uploads/2024/09/Global-Bioenergy-Power-Tracker-GBPT-September-2024.xlsx
#read GEM data
gem_dir = "G:/Shared drives/CREA-data/Global Coal Plant Tracker/non_coal"

read_gem <- function(file, sheets=2) {
  lapply(sheets, function(s) read_xlsx(file.path(gem_dir, file), sheet=s)) %>%
    bind_rows() %>% set_names(make_names(names(.))) %>%
    mutate(across(starts_with('state_province'), ~disambiguate(.x, c('Shandong'='Shangdong'))))
}

read_gem('Global-Bioenergy-Power-Tracker-GBPT-September-2024.xlsx', 2:3) %>%
  mutate(energy_source='Bioenergy') %>% filter(country_area=='China', status == 'construction' | start_year<=2024) -> gem_bio

#biomass capacity scaled up to national totals
readwindEN(get_data_file('generation-consumption-utilization-capacity.xlsx'),
           c('var', 'source', 'subtype'), read_vardata = T, skip=3, zero_as_NA = T) %>%
  filter(grepl('Capacity', var), grepl('Bio', subtype), month(date)==12) %>%
  mutate(year=year(date), Value=Value*10) %>% rename(MW_CEC = Value) -> bio_cap

gem_bio %>% group_by(start_year) %>% summarise(across(capacity__mw_, sum)) %>%
  mutate(MW=cumsum(capacity__mw_)) %>% left_join(bio_cap %>% select(start_year=year, MW_CEC)) %>%
  mutate(scaling=MW_CEC/MW) %>%
  fill(scaling, .direction = 'downup') ->
  bio_scaling

gem_bio %>% full_join(bio_scaling %>% select(start_year, scaling)) %>%
  mutate(capacity__mw_=capacity__mw_*scaling) ->
  gem_bio_scaled


gem_bio_scaled %>%
  group_by(prov=state_province, year=start_year) %>% summarise(across(c(Value1m=capacity__mw_), sum)) %>%
  mutate(Value1m=cumsum(Value1m)/10) %>% complete(year=2010:2024) %>% fill(Value1m, .direction='downup') %>%
  mutate(date=ymd(paste(year, 12, 31))) %>%
  complete(date=unique(sort(provdata_filled$date))) %>%
  mutate(Value1m=na.approx(Value1m, date, date, na.rm=F),
         var='Capacity', source='Bioenergy', basis_for_data='GEM unit level data scaled to national total') ->
  gen_bio

gen_bio %<>% mutate(Value1m=Value1m*4515*days_in_month(date)/365,
                    basis_for_data='National-level generation apportioned to provinces using capacity from GEM') %>%
  select(-var) %>%
  cross_join(provdata_filled %>% ungroup %>% distinct(var) %>% filter(grepl('Generation', var))) %>%
  bind_rows(gen_bio) %>%
  cross_join(provdata_filled %>% ungroup %>% distinct(variant))

provdata_filled %<>% bind_rows(gen_bio)

provdata_filled %<>%
  group_by(var, variant, prov, date) %>%
  filter(grepl('Capacity|Generation', var), 'Thermal' %in% source) %>%
  summarise(Value1m = pmax(0, Value1m[source=='Thermal']-if_null(Value1m[source=='Bioenergy'], 0))) %>%
  mutate(source='Fossil', basis='reported values for thermal with bioenergy subtracted') %>%
  bind_rows(provdata_filled)

provdata_filled %<>% group_by(var, prov, source, variant) %>%
  mutate(Value_rollmean_12m=zoo::rollapplyr(Value1m, 12, mean, fill=NA))

provdata_filled %<>% replace_na(list(basis_for_data='interpolated'))

provdata_filled %>%
  filter(var=='Generation, calculated', year(date)>=2013) %>%
  mutate(source=factor(source, levels=c('Fossil', 'Hydro', 'Bioenergy', 'Nuclear', 'Wind', 'Solar'))) %>%
  filter(!is.na(source)) ->
  plotdata



plotdata %>%
  filter(variant=='Utilization at trend') %>% #Utilization at trend
  replace_na(list(Value_rollmean_12m=0)) %>%
  group_by(prov, date) %>%
  mutate(share=Value_rollmean_12m/sum(Value_rollmean_12m)) ->
  shares

shares %>%
  group_by(variant, prov, source) %>%
  summarise(across(c(Value_rollmean_12m, share),
                   ~.x[date==last_month]-.x[date=='2020-12-31'])) ->
  changes

changes %>% filter(source != 'Fossil') %>% group_by(prov) %>%
  summarise(across(is.numeric, sum)) %>%
  mutate(source='Non-fossil total') -> changes_total

#paste.xl() -> region_dict
#region_dict %>% write_csv(file.path(output_dir, 'region_dictionary.csv'))
region_dict <- read_csv(file.path(output_dir, 'region_dictionary.csv'))

southern_provinces <-
  "Shanghai, Jiangsu, Zhejiang, Anhui, Fujian, Jiangxi, Hubei, Hunan, Chongqing, Sichuan, Yunnan, Guizhou, Tibet, Guangxi, Guangdong, Hainan" %>%
  strsplit(', ') %>% unlist

add_region <- function(prov, split='North-South') {
  regs <- NULL
  if(split=='North-South') regs <- ifelse(prov %in% southern_provinces, 'South', 'North')
  if(split=='East-West') regs <- region_dict$region[match(prov, region_dict$prov)]
  if(is.null(regs)) stop('Invalid region split')
  return(regs)
}


#shapefiles
get_adm(1, 'low') %>% st_as_sf() %>% filter(NAME_0=='China') -> adm1
get_adm(0, 'low') %>% st_as_sf() -> adm0



#Analysis of factors
readwindEN(get_data_file("Regional GDP w YoY.xlsx"), c('prov', 'var'), read_vardata = T) %>%
         mutate(type=ifelse(grepl('YTD', Name), 'YTD', 'single month'),
                prov=disambiguate(prov, c('Beijing', 'Chongqing', 'Tianjin', 'Shanghai')),
                YoY=YoY=='YoY') ->
  gdp_prov_total

gdp_prov_total %<>% group_by(prov, month(date)) %>% unYoY() %>%
  filter(is.na(YoY))



gdp_prov_total %>% ungroup %>%
  filter(month(date)==12) %>% mutate(year=year(date)) %>%
  rename(value_nominal=Value) %>%
  #inner_join(defl) %>%
  mutate(value_real=value_nominal, #*deflator,
         north_south=add_region(prov)) %>%
  group_by(prov) %>% mutate(across(starts_with('value_'), list(growth=~.x-lag(.x)))) -> prov_gdp_growth


readwindEN(get_data_file('Electricity Consumption by province_2023.xlsx'), c('prov', 'var'),
           read_vardata = T) -> power_demand




readwindEN(get_data_file('Regional GDP complete.xlsx'), c('var', 'price_basis', 'sector', 'subsector'),
           read_vardata = T, columnFilter = 'Constant|Current') %>%
  replace_na(list(sector='Total')) -> gdp_natl

readwindEN(get_data_file('Regional GDP complete.xlsx'), c('prov', 'var', 'sector', 'subsector'),
           read_vardata = T, columnExclude = 'Constant|Current') %>%
  mutate(subsector=disambiguate(subsector,
                                c('Hotels and Catering Services'='Hotel',
                                  Finance='Financ',
                                  'Wholesale and Retail Trade'='Trade',
                                  'Transport, Storage and Post'='Transport')),
         across(contains('sector'), ~ifelse(.x=='YTD', NA, .x)),
         type=ifelse(grepl('YTD', Name), 'YTD', 'single month'),
         prov=disambiguate(prov, c('Beijing', 'Chongqing', 'Tianjin', 'Shanghai'))) %>%
  replace_na(list(sector='Total')) -> gdp_prov

readwindEN(get_data_file('iron&steel output by province.xlsx'),
           c('prov', 'var', 'prod'),
           read_vardata = T) %>% filter(!grepl('Taiwan', Name)) -> i_s

readwindEN(get_data_file('Output of electricity intensive products by province.xlsx'),
           c('prov', 'var', 'prod'),
           read_vardata = T) -> ind

ind %<>% bind_rows(i_s)

read_xlsx('inst/extdata/population by province.xlsx') %>% distinct %>%
  pivot_longer(-Region, names_to='year', values_to='pop') %>%
  mutate(across(year, as.numeric)) %>%
  rename(prov=Region) -> pop

#gdp per capita
gdp_prov %<>%
  group_by(prov, sector, subsector) %>%
  mutate(gdp_12m=zoo::rollapplyr(Value, 4, sum, fill=NA),
         year=year(date)) %>%
  left_join(pop) %>%
  group_by(prov) %>% fill(pop, .direction = 'updown')


gdp_prov_total %<>% mutate(year=year(date)) %>% left_join(pop) %>%
  group_by(prov) %>% fill(pop, .direction = 'updown')

