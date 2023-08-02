require(tidyverse)
require(lubridate)
require(magrittr)
require(sf)

seq.Date(ymd('2023-01-01'), today(), by='day') %>% 
  pbapply::pblapply(function(start_date) {
    message(start_date)
    end_date=start_date
    read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                "pollutant=pm25&",
                "date_from=",start_date,"&date_to=", end_date,
                "&level=city&do_average=true&averaging_period=1d&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv"))
  }) %>% bind_rows() -> daily_global

daily_global %>% saveRDS('~/global_daily.RDS')
daily_global <- readRDS('~/global_daily.RDS')

cities <- read_csv('https://api.energyandcleanair.org/cities?format=csv')
cities$geometry %>% strsplit(',') %>% lapply(function(x) x %>% force_numeric %>% matrix(nrow=1) %>% 'colnames<-'(c('lon', 'lat')) %>% as_tibble) %>% 
  bind_rows() %>% bind_cols(cities, .) -> cities

daily_global %<>% left_join(cities %>% select(location_id=id, city_name=name, country_name, lat, lon))

daily_global %>% group_by(city_id) %>% filter(!creahelpers::is.outlier(value, SDs=5) | city_name=='New York') %>% 
  arrange(desc(value)) %>% filter(value>117) %>% 
  group_by(city_name, country_name) %>% summarise(n=n()) %>% arrange(desc(n)) %>% 
  head(30) %>% ungroup %>% 
  mutate(city = paste0(city_name, ', ', country_name),
         city=factor(city, levels=city)) %>% 
  ggplot(aes(city, n)) + geom_col(aes(fill=country_name)) + coord_flip() + theme_crea() +
  scale_x_discrete(limits=rev) +
  ggsci::scale_fill_aaas(guide='none') +
  x_at_zero() +
  labs(title='Days of pollution exceeding the worst day in New York',
       subtitle='In 2023 to date (out of 159 days)', x='', y='') +
  geom_label(aes(label=n))

adm <- get_adm(0, 'coarse')
require(ggspatial)
daily_global %>% group_by(city_id) %>% filter(!creahelpers::is.outlier(value, SDs=10) | city_name=='New York') %>% 
  arrange(desc(value)) %>% 
  distinct(location_id, .keep_all = T) %>% 
  mutate(has_worse=ifelse(value>117, 'yes', 'no')) %>% 
  ggplot(aes(lon, lat)) + annotation_spatial(adm, fill=NA) + geom_point(aes(col=has_worse, alpha=has_worse)) +
  theme_crea() + scale_color_crea_d('change', col.index = c(1,7)) + scale_alpha_discrete(range=c(.3,.8)) +
  theme(panel.grid=element_blank()) +
  labs(title='Cities with worse daily pollution than the worst day in New York in 2023', x='', y='', col='', alpha='')

daily_global %>% arrange(desc(value)) %>% distinct(city_name, .keep_all=T) %>% 
daily_global %>% filter(city_name=='New York') %>% arrange(desc(value))
