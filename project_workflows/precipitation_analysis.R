require(terra)
require(sf)


read_csv("https://api.energyandcleanair.org/v1/weather?region_type=gadm1&region_iso2=CN&variable=precipitation&format=csv") ->
  prec

readwindEN(get_data_file('power capacity by province&type.xlsx'),
           c('prov', 'var', 'source'),
           columnExclude = 'Wind|Solar',
           skip=2, read_vardata = T) -> cap_all

prec %>% mutate(date = date %>% 'day<-'(days_in_month(.)),
                region_name=fix_province_names(region_name)) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  prov_monthly

cap_all %>% filter(source=='Hydropower') %>%
  rename(electricity_source=source, region_name=prov, hydro_capacity=Value) %>%
  inner_join(prec %>% rename(precipitation=value)) %>%
  group_by(date) %>%
  summarise(precipitation=weighted.mean(precipitation, hydro_capacity)) %>%
  mutate(plotdate=date %>% 'year<-'(2022),
         year=as.factor(year(date))) %>%
  ggplot(aes(plotdate, precipitation*3600*24, col=year)) + geom_line()



Sys.setenv(GIS_DIR='~/GIS')
get_adm(1, 'low') %>% st_as_sf() %>% filter(NAME_0=='China') -> adm1
list.files('~/../Downloads/precipitation', full.names = T) -> f
f %>% terra::rast() -> prec
f %>% basename() %>% gsub('\\.nc$', '', .) %>% gsub('.*\\.', '', .) %>% substr(1,8) %>% ymd() -> dates
names(prec) <- dates

terra::extract(prec, adm1, fun=mean, na.rm=T) -> prec_adm1

prec_adm1 %>% mutate(NAME_1=adm1$NAME_1) %>% pivot_longer(c(-ID, -NAME_1), names_to='date') %>%
  mutate(across(date, ymd),
         year=as.factor(year(date)),
         plotdate=date %>% 'year<-'(2022),
         mm=value*1000*3600*24*days_in_month(date)) %>%
  ggplot(aes(plotdate, mm, col=year, alpha=year(date)>=2023, linewidth=year(date)==2024)) + geom_line() + facet_wrap(~NAME_1) +
  scale_alpha_manual(values=c(.2, 1)) + scale_linewidth_manual(values=c(.5,1.2))



read_csv('~/../Downloads/g4.areaAvgTimeSeries.GPM_3IMERGDL_07_precipitation.20150101-20241031.98E_25N_118E_38N.csv', skip=7) -> prec_area

prec_area %>% group_by(year=year(time), month=month(time)) %>% #filter(year(time)>=2023) %>%
  rename(value=contains('precipitation')) %>%
  summarise(across(value, mean)) %>%
  mutate(year=as.factor(year),
         plotdate=ymd(paste(2022, month, 15))) %>%
  ggplot(aes(plotdate, value, col=year, linewidth=year, alpha=year)) + geom_line() +
  scale_linewidth_manual(values=c(rep(.5, 8), .8, 1.2)) + scale_alpha_manual(values=c(rep(.2,8), 1, 1)) +
  theme_crea() +
  labs(title='Southwest-Central China monthly precipitation',
       y='mm/day average', x='') +
  scale_color_manual(values=colorRampPalette(crea_palettes$change[c(7:5,2:1)])(10)) +
  scale_x_date(date_labels = '%b')
quicksave('outputs/latest/Southwest-Central China monthly precipitation.png')

