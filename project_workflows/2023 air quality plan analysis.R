source('load_package.R')

read_csv('~/RPackages/china_transition_outlook/emissions/CO2 emissions by province.csv') %>%
  mutate(productEN=productEN %>% gsub('B riquettes', 'Briquettes', .) %>% gsub('Tota1', 'Total', .)) -> co2

co2$provEN %>% unique

station_key <- read_csv(get_data_file('air_quality_station_codes.csv'))
key_region_citylist <- read_csv(get_data_file('air_pollution_key_regions_2023.csv')) %>%
  filter(change_in_2023 %notin% c('removed', 'not a city')) %>% select(-change_in_2023)

city_key <- station_key %>%
  distinct(city_name_EN=CityEN, NAME_1_EN=ProvinceEN, city_name_ZH=CityZH, NAME_1_ZH = ProvinceZH) %>%
  filter(!is.na(city_name_EN)) %>%
  rename(GADM_NAME_2_EN=city_name_EN) %>%
  left_join(key_region_citylist) %>%
  select(-GADM_NAME_2_EN)

city_key %>% filter(!is.na(keyregion), keyregion!='PRD') %>% distinct(keyregion, NAME_1_EN) ->
  prov_key

co2 %<>% group_by(provEN, productEN) %>% summarise(across(c(Mtce, MtCO2), ~sum(.x, na.rm=T))) %>%
  mutate(sectorEN='Total') %>% bind_rows(co2)

co2 %<>%
  left_join(prov_key %>% rename(provEN=NAME_1_EN) %>% distinct(provEN) %>%
              mutate(is_keyregion=T))

co2 %>%
  group_by(productEN, sectorEN, year, is_keyregion) %>%
  summarise(across(Mtce, ~sum(.x, na.rm=T))) %>%
  summarise(keyregion_share=Mtce[!is.na(is_keyregion)]/sum(Mtce)) %>%
  filter(productEN=='Coal Total')

co2 %>% ungroup %>%
  filter(productEN=='Coal Total', sectorEN=='Total') %>%
  arrange(-Mtce) %>% mutate(share=Mtce/sum(Mtce)) %>%
  ggplot(aes(provEN, Mtce, fill=is_keyregion)) +
  geom_col() + coord_flip()

gis_dir='~/GIS'
require(sf); require(ggspatial)
get_adm(1, 'coarse') %>% subset(NAME_0=='China') %>% st_as_sf -> adm1
get_adm(2, 'coarse') %>% subset(NAME_0=='China') %>% st_as_sf -> adm2

key_region_citylist %>% filter(keyregion!='PRD') %>%
  full_join(adm2 %>% rename(NAME_1_EN=NAME_1, city_name_EN=NAME_2), .) -> adm2


ggplot() + annotation_spatial(adm1) +
  layer_spatial(adm2 %>% filter(!is.na(keyregion)), aes(fill=keyregion)) +
  geom_spatial_text(data=adm2 %>% st_centroid(), mapping=aes(label=city_name_EN))

read_xlsx("~/RPackages/gcpt-analysis/data/Global-Coal-Plant-Tracker-July-2023_ChinaBriefRev.xlsx",
          sheet='Units', .name_repair = make.names) %>%
  filter(Country=='China') -> coalplants

read_xlsx('~/../Downloads/Global-Steel-Plant-Tracker-2023-03-2.xlsx',
          sheet='Steel Plants', .name_repair = make.names) %>%
  filter(Country=='China') -> steelplants

add_keyregion_info <- function(df) {
  df %>% to_sf_points() %>% st_nearest_feature(adm2) %>%
    sapply(function(x) adm2$GID_2[x]) ->
    df$GID_2

  df %>% left_join(adm2 %>% select(GID_2, NAME_1_EN, keyregion))
}

coalplants %<>% add_keyregion_info

coalplants %>% group_by(Status, !is.na(keyregion)) %>%
  summarise(across(c(MW=contains('MW')), sum)) %>%
  mutate(share=MW/sum(MW))

coalplants %>% group_by(Status, keyregion) %>%
  filter(Status=='operating') %>%
  summarise(across(c(MW=contains('MW')), sum)) %>%
  mutate(share=MW/sum(MW))


steelplants %<>% separate(Coordinates, c('lat', 'lon'), ', ', convert=T) %>%
  add_keyregion_info()

steelplants %>% group_by(Status, !is.na(keyregion)) %>%
  summarise(across(c(crude_steel_capacity_tpa=contains('crude.steel.capacity')),
                   function(x) x %>% as.numeric %>% sum(na.rm=T))) %>%
  mutate(share=crude_steel_capacity_tpa/sum(crude_steel_capacity_tpa))

require(terra)
get_population_path('gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif') %>%
  rast -> pop

terra::extract(pop*cellSize(pop, unit='km'), adm2, sum, na.rm=T) -> pop_adm2

adm2$pop <- pop_adm2[,2]

adm2 %>% st_drop_geometry() %>%
  group_by(keyregion) %>% summarise(across(pop, ~sum(.x, na.rm=T))) %>%
  mutate(share=pop/sum(pop), cumsum=cumsum(pop))
