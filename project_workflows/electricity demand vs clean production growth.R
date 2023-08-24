new_cap <- readwindEN('data/Power Capacity.xlsx', c('var', 'source'), columnExclude = 'Conventional|Coal|Gas|Thermal',
                  columnFilter='New',
                  zero_as_NA = T, read_vardata = T) %>% filter(source!='YTD') %>%
  mutate(GW=Value/100)


cap <- readwindEN('data/Power Capacity.xlsx', c('var', 'source'), columnExclude = 'New|Conventional|Coal|Gas',
           zero_as_NA = T, read_vardata = T) %>% filter(source!='YTD') %>%
  mutate(GW=Value/100)
cap$GW[cap$date=='2023-06-30' & grepl('Solar', cap$source)] <-
  cap$GW[cap$date=='2022-06-30' & grepl('Solar', cap$source)] + 78


gen_cons <- read_csv('data/monthly_full_release_long_format-4.csv') %>%
  set_names(tolower(make.names(names(.)))) %>% rename(source=variable, TWh=value) %>%
  filter(country.code=='CHN', unit=='TWh')


cap %>% mutate(source = disambiguate(source, unique(gen_cons$source)),
               date = date %>% 'day<-'(1)) %>%
  inner_join(gen_cons) %>%
  complete(date=unique(gen_cons$date), source) %>%
  group_by(source) %>% arrange(date) %>%
  mutate(GW = na.approx(GW, date, na.rm=F)) %>%
  bind_rows(gen_cons %>% filter(source=='Demand')) -> gen_cons

gen_cons %<>% mutate(utilization = TWh/(GW*days_in_month(date)*24/1000), month=month(date))

gen_cons %<>% filter(year(date) %in% 2018:2022) %>% group_by(source, month) %>% summarise(across(c(mean_utilization=utilization), ~mean(.x, na.rm=T))) %>%
  left_join(gen_cons) %>%
  mutate(TWh_norm = case_when(source=='Demand'~TWh,T~GW*mean_utilization*days_in_month(date)*24/1000))

gen_cons %<>% group_by(source) %>% mutate(change_TWh = get.yoy(TWh_norm, date, type='absolute'),
                                          change_GW = get.yoy(GW, date, type='absolute'))

gen_cons %>% filter(source != 'Demand') %>%
  ggplot(aes(date, change_TWh, fill=source)) + geom_col() +
  geom_point(data=gen_cons %>% filter(source == 'Demand'))

gen_cons %>% group_by(source) %>% arrange(date) %>%
  mutate(change_TWh = TWh_norm - lag(TWh_norm, 6),
         change_GW = GW - lag(GW, 6)) %>%
  mutate(YH=date %>% 'month<-'(ifelse(month(date)<7, 6, 12))) %>%
  group_by(source, YH) %>% summarise(across(change_TWh, ~sum(.x, na.rm=T))) ->
  gen_cons_yh

gen_cons_yh %>% filter(source != 'Demand') %>%
  ggplot(aes(YH, change_TWh, fill=source)) + geom_col() +
  geom_point(data=gen_cons_yh %>% filter(source == 'Demand'))

gen_cons %>% mutate(year=year(date)) %>%
  group_by(source, year) %>% summarise(across(change_TWh, ~sum(.x, na.rm=T))) ->
  gen_cons_yr

gen_cons_yr %>% filter(source != 'Demand') %>%
  mutate(change_TWh=change_TWh*ifelse(year==2023, 2, 1)) %>%
  ggplot(aes(year, change_TWh, fill=source)) + geom_col() +
  geom_point(data=gen_cons_yr %>% filter(source == 'Demand'))


#added generation from 2023 installations
gen_cons %>% filter(year(date) %in% 2018:2022) %>% group_by(source) %>% summarise(across(c(mean_utilization=utilization), ~mean(.x, na.rm=T))) ->
  yearly_cf

gen_cons %>% filter(year(date)==2022, source=='Demand') %>% summarise(across(TWh, sum)) -> total_demand

new_cap %>% filter(date=='2023-06-30') %>%
  select(source, GW) %>% mutate(source=disambiguate(source, yearly_cf$source)) %>%
  left_join(yearly_cf) %>%
  mutate(TWh=GW*8.76*mean_utilization,
         total_TWh=sum(TWh),
         growth_covered=total_TWh*2/total_demand$TWh)


