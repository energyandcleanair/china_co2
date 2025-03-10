###read data

#read GEM datasets
gem_dir = "G:/Shared drives/CREA-data/Global Coal Plant Tracker/non_coal"

#https://globalenergymonitor.org/wp-content/uploads/2024/10/Global-Solar-Power-Tracker-June-2024-v2.xlsx
bind_rows(read_xlsx(file.path(gem_dir, "Global-Solar-Power-Tracker-June-2024-v2.xlsx"), sheet=2),
          read_xlsx(file.path(gem_dir, "Global-Solar-Power-Tracker-June-2024-v2.xlsx"), sheet=3)) %>%
  set_names(make_names(names(.))) %>%
  mutate(energy_source='Solar') -> gem_solar

#https://globalenergymonitor.org/wp-content/uploads/2024/05/Global-Wind-Power-Tracker-June-2024.xlsx

bind_rows(read_xlsx(file.path(gem_dir, "Global-Wind-Power-Tracker-June-2024.xlsx"), sheet=2),
          read_xlsx(file.path(gem_dir, "Global-Wind-Power-Tracker-June-2024.xlsx"), sheet=3)) %>%
  set_names(make_names(names(.))) %>%
  mutate(energy_source='Wind') -> gem_wind



#read Global Wind and Solar Atlases
require(sf)
require(terra)

atlas_dir = "~/../Downloads"

#https://api.globalsolaratlas.info/download/World/World_PVOUT_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF.zip
unzip(file.path(atlas_dir, "World_PVOUT_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF.zip"),
      exdir=file.path(atlas_dir, 'GlobalSolarAtlas-v2')) ->
  solar_atlas_files

solar_atlas_files %>% grep('\\.tif$', ., value=T) %>% rast() -> pvout

#https://globalwindatlas.info/api/gis/country/CHN/capacity-factor_IEC3/
#rast(file.path(atlas_dir, "CHN_capacity-factor_IEC3.tif")) -> wind_cf

#https://globalwindatlas.info/api/gis/global/capacity-factor_iec3/
rast(file.path(atlas_dir, "gwa3_250_capacityfactor_IEC3.tif")) -> wind_cf


gem_solar %>% to_sf_points() %>% terra::extract(pvout, .) %>% mutate(atlas_CF=PVOUT/24) -> gem_solar_cf
gem_wind %>% to_sf_points() %>% terra::extract(wind_cf, .) %>% rename(atlas_CF=matches('capacity.?factor')) -> gem_wind_cf


bind_rows(gem_solar %>% mutate(atlas_CF=gem_solar_cf$atlas_CF),
          gem_wind %>% mutate(atlas_CF=gem_wind_cf$atlas_CF) %>% rename(country=contains('country'))) %>%
  rename(province=state_province, MW=contains('mw')) ->
  gem

gem %<>% filter(energy_source=='Wind') %>%
  mutate(energy_source=paste(energy_source, disambiguate(installation_type, c('Onshore', 'Offshore')))) %>%
  bind_rows(gem)

summarise_gem <- function(df) {
  df %>% summarise(atlas_CF=weighted.mean(atlas_CF, MW, na.rm=T),
                   age=2023-weighted.mean(start_year, MW),
                   MW=sum(MW))
}

gem %>% filter(status=='operating', start_year<=2023) %>%
  group_by(country, energy_source) %>%
  summarise_gem ->
  gem_country

gem %>% filter(country=='China', status=='operating', start_year<=2023) %>%
  group_by(province, energy_source) %>%
  summarise_gem ->
  gem_province

gem_province %>% write_csv('outputs/solar_wind_predicted_utilization_by_province_from_Global_Atlas.csv')


#read Ember power data and calculate utilization and share of distributed
ember_yearly <- read_csv("https://storage.googleapis.com/emb-prod-bkt-publicdata/public-downloads/yearly_full_release_long_format.csv")
ember_us <- read_csv('https://storage.googleapis.com/emb-prod-bkt-publicdata/public-downloads/us_yearly_full_release_long_format.csv')


# API URL
library(httr)
library(jsonlite)
get_url <- function(url) {
  # Make the GET request
  response <- GET(url)

  # Parse the JSON response
  data_json <- content(response, "text")
  data_parsed <- fromJSON(data_json, flatten = TRUE)
  data_parsed$response$data
}


eia_cap <- get_url("https://api.eia.gov/v2/electricity/state-electricity-profiles/capability/data/?frequency=annual&data[0]=capability&start=2000&end=2023&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=saCHQrMiZMdRfq9clMJVD4pYF2PKiEgBX0Q314QK")


ember_yearly %>%
  set_names(make_names(names(.))) %>%
  filter(variable %in% c('Wind', 'Solar'), unit %in% c('GW', 'TWh'), area_type=='Country') %>%
  select(area, country_code, year, energy_source=variable, unit, value) %>%
  spread(unit, value) %>%
  group_by(area, energy_source) %>%
  mutate(GW_mean=(GW+lag(GW))/2,
         actual_CF=TWh/(GW*8.76),
         GW_yoy=GW/lag(GW)-1) ->
  ember_cf

#read China province power data
province_ws <- read_province_solar_wind_data()
add_energy_source = function(df) df %>% mutate(energy_source=disambiguate(var, c(Wind='Wind', Solar='Solar|Photovoltaic')))

infile  <- 'curtailment.xlsx' %>% get_data_file()

readwindEN(infile, c('prov', 'var'), columnExclude = 'Mengxi|Mengdong', read_vardata = T, skip=2) %>% filter(!is.na(var)) -> curtailment2
readwindEN(infile, c('prov', 'subgrid', 'var'), columnFilter = 'Mengxi|Mengdong', read_vardata = T, skip=2) -> curtailment3

curtailment2 %<>% add_energy_source
curtailment3 %<>% add_energy_source

#Mengdong = Hulunbuir City, Xing'an League, Tongliao City, Chifeng City, Xilin Gol League
gem %>% filter(country=='China', status=='operating', province=='Inner Mongolia', start_year<=2023) %>%
  mutate(subgrid=ifelse(grepl('Xilingol|Chifeng|Tongliao|Hinggan|Hulunbuir', major_area__prefecture__district_), 'Mengdong', 'Mengxi')) %>%
  group_by(energy_source, subgrid) %>% summarise(across(MW, sum)) %>% mutate(weight=MW/sum(MW)) ->
  neimeng_split

curtailment3 %<>% left_join(neimeng_split %>% select(-MW)) %>%
  group_by(energy_source, prov, date) %>% summarise(Value=weighted.mean(Value, weight))

curtailment2 %>% select(all_of(names(curtailment3))) -> curtailment

curtailment %>% filter(year(date) %in% 2021:2024) %>%
  group_by(prov, energy_source, month(date)) %>%
  summarise(across(Value, mean)) %>%
  summarise(across(Value, mean)) %>%
  mutate(curtailment=1-Value/100) ->
  curtailment_mean

province_ws %>% filter(date=='2023-12-31', var=='Capacity') %>%
  mutate(GW=Value/100) %>%
  select(prov, energy_source=source, GW) %>%
  left_join(curtailment_mean, .) ->
  curtailment_mean



#China province analysis
province_ws %>% filter(year(date) %in% 2021:2024, var=='Utilization') %>% rename(energy_source=source) %>%
  group_by(prov, energy_source) %>% mutate(Value1m=ifelse(Value==0, NA, Value1m) %>% na.approx() %>% divide_by(days_in_month(date)*24)) %>%
  group_by(prov, energy_source, month(date)) %>% summarise(across(Value1m, mean)) %>% summarise(across(c(actual_CF=Value1m), mean)) ->
  province_cf

#todo: add share of distributed

gem_province %>% rename(prov=province, MW_GEM=MW) %>% full_join(province_cf) %>%
  full_join(curtailment_mean %>% select(-Value)) %>%
  mutate(actual_CF_ex_curtailment=actual_CF/(1-curtailment)) ->
  province_alldata

province_alldata %>%
  ggplot(aes(atlas_CF, actual_CF_ex_curtailment)) + geom_point() + facet_wrap(~energy_source, scales='free') + geom_abline() + geom_text_repel(aes(label=prov), size=1)


province_alldata %>% lm(actual_CF_ex_curtailment ~ (atlas_CF+age+curtailment):energy_source+energy_source, data=.) %>% summary

#todo: prediction model based on China data



#global analysis
require(ggflags)

gem_country %<>% ungroup %>% mutate(country_code=countrycode(country, 'country.name.en', 'iso3c'))

ember_cf %>% filter(year %in% 2021:2023, GW_yoy<1) %>%
  group_by(country_code, energy_source) %>%
  summarise(across(actual_CF, mean), across(matches('GW|TWh'), ~.x[year==max(year)])) %>%
  inner_join(gem_country) %>%
  filter(actual_CF<.6,
         actual_CF<.4 | energy_source!='Solar',
         actual_CF>.1 | energy_source!='Wind',
         actual_CF>.05) ->
  country_alldata

country_alldata %>%
  ggplot(aes(atlas_CF, actual_CF, col=country=='China')) +
  facet_wrap(~energy_source, scales='free') +
  geom_flag(aes(country=country_code %>% countrycode('iso3c', 'iso2c') %>% tolower)) +
  geom_abline() +
  theme(aspect.ratio = 1)

country_alldata %>%
  ggplot(aes(atlas_CF, actual_CF, col=country=='China')) +
  facet_wrap(~energy_source, scales='free') +
  geom_point(aes(size=GW)) +
  geom_abline() +
  geom_smooth(method='lm', se=F) +
  geom_text_repel(aes(label=country), size=1) +
  theme(aspect.ratio = 1, legend.position = 'bottom')


country_alldata %>% mutate(category='country') %>%
  bind_rows(province_alldata %>% rename(country=prov) %>% mutate(category='province')) %>%
  ggplot(aes(atlas_CF, actual_CF, col=category)) +
  facet_wrap(~energy_source, scales='free') +
  geom_point(aes(size=GW)) +
  geom_abline() +
  geom_smooth(method='lm', se=F) +
  geom_text_repel(aes(label=country), size=1) +
  scale_x_continuous(labels=scales::percent, #breaks = function(x) seq(0,1,.1) %>% subset(.>x[1] & .<x[2]),
                     expand=expansion(mult=c(0,.05))) +
  scale_y_continuous(labels=scales::percent, #breaks = function(x) seq(0,1,.1) %>% subset(.>x[1] & .<x[2]),
                     expand=expansion(mult=c(0,.05))) +
  scale_color_crea_d('dramatic') +
  expand_limits(x=0, y=0) +
  labs(x='expected capacity factor', y='actual capacity factor',
       size='installed capacity, GW',
       title='Solar and wind capacity factors',
       subtitle='in different countries and Chinese provinces') +
  theme_crea() +
  theme(aspect.ratio = 1, legend.position = 'bottom')




country_alldata %>% mutate(share_of_distributed=(1-MW/1000/GW) %>% pmax(0) %>% pmin(1)) %>%
  lm(actual_CF ~ (atlas_CF+age+share_of_distributed):energy_source+energy_source, data=.) %>% summary


country_alldata %>% filter(energy_source=='Wind') %>%
  lm(actual_CF ~ (atlas_CF+age), data=.) %>% summary
