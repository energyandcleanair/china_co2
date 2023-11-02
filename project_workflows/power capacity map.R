in_file = get_data_file("power capacity by province&type.xlsx")
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T, zero_as_NA = T) -> provcap

pwr_sources <- c('Thermal', 'Wind', 'Solar', 'Nuclear', 'Hydro')
provs <- read_xlsx(get_data_file('provincesZH.xlsx'))
provcap$prov %>% unique %>% subset(!grepl(paste(pwr_sources, collapse='|'), .))
provcap %<>% mutate(prov = disambiguate(Name, c(provs$Province, 'National')), source=disambiguate(Name, pwr_sources))


provcap %>% group_by(source) %>%
  filter(!is.na(Value)) %>% filter(date==max(date)) %>% ggplot(aes(prov, Value)) +
  facet_wrap(~source, scales='free_x') + geom_col() + coord_flip()


in_file = get_data_file("Power Consumption by Province M.xlsx")
getwindvars(in_file)
readwindEN(in_file, c('source', 'prov'), read_vardata = T, zero_as_NA = T) -> provcons

provcons %<>% group_by(source, prov) %>%
  unYTD() %>% roll12m() %>%
  filter(date==max(date)) %>%
  mutate(Value = Value12m * 12 / 8760)

require(sf)
require(ggspatial)
sf_use_s2(F)
gis_dir='~/GIS'
adm0 <- get_adm(0, 'coarse') %>% st_as_sf()
adm1 <- get_adm(1, 'coarse') %>% st_as_sf() %>% filter(NAME_0=='China')

adm1 %<>% st_centroid() %>% st_coordinates() %>% bind_cols(adm1, .)
adm1 %<>% mutate(prov=fix_province_names(NAME_1))

provcap %>% group_by(source) %>%
  filter(!is.na(Value)) %>% filter(date==max(date)) %>%
  bind_rows(provcons) %>%
  full_join(adm1, .) %>%
  ggplot(aes(X, Y, size=Value/100, col=source)) + geom_point() + facet_wrap(~source) +
  annotation_spatial(adm0, fill='white') + layer_spatial(adm1, fill='white') + geom_point() +
  scale_size_area(max_size = 15) +
  theme_crea() + theme(panel.grid = element_blank(), panel.background = element_rect(fill='lightblue')) +
  scale_color_crea_d(col.index=c(1,9,3,10,6,4),guide='none') + coord_sf(expand=F) +
  labs(title='Power generation capacity in China by province and type',
       x='', y='', size='GW') -> p
quicksave('outputs/Power generation capacity in China by province and type.png', plot=p)

#newly added
provcap %>% group_by(prov, source) %>%
  filter(!is.na(Value)) %>%
  summarise(Value=Value[date==max(date)]-Value[date=='2021-12-31']) %>%
  full_join(adm1, .) %>%
  ggplot(aes(X, Y, size=Value/100, col=source)) + geom_point() + facet_wrap(~source) +
  annotation_spatial(adm0, fill='white') + layer_spatial(adm1, fill='white') + geom_point() +
  scale_size_area(max_size = 15) +
  theme_crea() + theme(panel.grid = element_blank(), panel.background = element_rect(fill='lightblue')) +
  scale_color_crea_d(col.index=c(1,9,10,6,4),guide='none') + coord_sf(expand=F) +
  labs(title='Newly added power capacity in China by province and type',
       subtitle='January 2022 to August 2023',
       x='', y='', size='GW') -> p
quicksave('outputs/Newly added power capacity in China by province and type.png', plot=p)
