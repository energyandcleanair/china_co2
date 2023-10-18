output_dir='outputs'

cap <- readwindEN('data/Power Capacity.xlsx', c('var', 'source'), columnExclude = 'New|Conventional|Coal|Gas',
           zero_as_NA = T, read_vardata = T) %>% filter(source!='YTD') %>%
  mutate(GW=Value/100)

gen_cons <- read_csv('data/monthly_full_release_long_format-4.csv') %>%
  set_names(tolower(make.names(names(.)))) %>% rename(source=variable, TWh=value) %>%
  filter(country.code=='CHN', unit=='TWh')

#projected 2023 capacity: Wind, Solar, Hydro, Nuclear 43000, 53000, 42000, 5846
#https://www.cec.org.cn/detail/index.html?3-323217
#projected solar additions: 210 GW
#https://www.bloomberg.com/news/articles/2023-09-06/china-s-breakneck-solar-expansion-starts-to-meet-power-grid-resistance
tibble(source=c('Wind', 'Solar', 'Hydro', 'Nuclear'),
       date=ymd('2023-12-01'),
       GW=c(43000, 53000, 42000, 5846)/100) -> cap_fut

cap_fut$GW[cap_fut$source=='Solar'] <- cap$GW[cap$date=='2022-12-31' & grepl('Solar', cap$source)] + 210

dates <- seq.Date(min(gen_cons$date), ymd('2025-12-01'), by='month')

compound_growth = function(x, g, type='percent') {
  for(i in which(is.na(x) & seq_along(x)>1)) {
    if(type=='percent') x[i] <- x[i-1] * (1+g[i])
    if(type=='absolute') x[i] <- x[i-1] + g[i]
  }
}

cap %>% mutate(source = disambiguate(source, unique(gen_cons$source)),
               date = date %>% 'day<-'(1)) %>%
  inner_join(gen_cons) %>%
  bind_rows(cap_fut) %>%
  complete(date=dates, source) %>%
  group_by(source) %>%
  mutate(annual_add_GW=GW[date=='2023-12-01']-GW[date=='2022-12-01']) %>%
  group_by(source, month(date)) %>%
  mutate(GW = case_when(date>='2024-01-01'~GW[year(date)==2023]+annual_add_GW*(year(date)-2023), T~GW)) %>%
  group_by(source) %>% arrange(date) %>%
  mutate(GW = na.approx(GW, date, na.rm=F)) %>%
  bind_rows(gen_cons %>% filter(source=='Demand') %>% complete(date=dates, source)) -> gen_cons

gen_cons %>% ggplot(aes(date, GW, col=source)) + geom_line()

gen_cons %>% filter(source=='Demand') %>% mutate(YoY=get_yoy(TWh, date)) %>% filter(year(date)==2023) %>%
  summarise(across(YoY, ~mean(.x, na.rm=T))) %>% use_series(YoY) -> demand_growth
demand_growth <- 5.2e-2

gen_cons %<>% mutate(utilization = TWh/(GW*days_in_month(date)*24/1000), month=month(date)) %>%
  group_by(month, source) %>% mutate(TWh = case_when(year(date)==2023 & source=='Demand' & is.na(TWh)~TWh[year(date)==2022]*(1+demand_growth), T~TWh))

gen_cons %<>% group_by(source) %>% mutate(change_TWh = get.yoy(TWh, date, type='absolute'),
                                          change_GW = get.yoy(GW, date, type='absolute'))

gen_cons %>% mutate(year=year(date)) %>%
  group_by(source, year) %>% summarise(across(change_TWh, ~sum(.x, na.rm=T)),
                                       across(change_GW, ~.x[month==12])) ->
  gen_cons_yr


gen_cons %>% filter(year(date) %in% 2018:2022) %>% group_by(source) %>% summarise(across(c(mean_utilization=utilization), ~mean(.x, na.rm=T))) ->
  yearly_cf



#added capacity vs demand
infile <- "data/Electricity Consumption by sector.xlsx"
readwindEN(infile, c('var', 'sector', 'subsector'), read_vardata = T, zero_as_NA = T, columnFilter = 'Whole Society') -> elec_cons_sector

elec_cons_sector %>% group_by(year=year(date)) %>%
  summarise(TWh=sum(Value)/100e3) %>%
  filter(year >=2007, year<2023) %>%
  mutate(source='Demand', TWh=TWh-lag(TWh)) ->
  cons_yr

new_cap <- readwindEN('data/Power Capacity.xlsx', c('var', 'source'), columnExclude = 'Conventional|Coal|Gas|Thermal',
                      columnFilter='New',
                      zero_as_NA = T, read_vardata = T) %>% filter(source!='YTD') %>%
  mutate(GW=Value/100)

new_cap %>% filter(month(date)==12) %>%
  mutate(source=disambiguate(source, yearly_cf$source), year=year(date)) %>%
  select(source, year, GW) -> new_cap_yr

gen_cons_yr %>% filter(year==2023) %>% select(source, year, GW=change_GW, TWh=change_TWh) %>%
  bind_rows(new_cap_yr, cons_yr, .) %>%
  left_join(yearly_cf) %>%
  mutate(TWh=case_when(source=='Demand'~TWh,T~GW*8.76*mean_utilization)) ->
  gen_cons_yr_norm

start_yr=2010

cons_yr %>% filter(year>=start_yr) %>% ungroup %>%
  summarise(across(TWh, mean)) -> average_demand_growth

gen_cons_yr_norm %>% filter(source != 'Demand', year>=start_yr) %>%
  ggplot(aes(year, TWh, alpha=ifelse(year==2023, 'Forecast', 'Actual'))) +
  geom_col(aes(fill=source)) +
  geom_point(data=gen_cons_yr_norm %>% filter(source == 'Demand', year>=start_yr), aes(shape='Demand growth')) +
  geom_hline(aes(yintercept=TWh, linetype='Average demand growth'), data=average_demand_growth) +
  theme_crea() +
  scale_shape(name='', guide=guide_legend(order=1)) +
  scale_linetype_manual(values='dashed', name='', guide=guide_legend(order=2)) +
  scale_fill_crea_d(guide=guide_legend(order=3)) +
  scale_alpha_manual(values=c(1,.6), name='',
                     guide=guide_legend(override.aes = list(shape=NA, fill=crea_palettes$CREA[1]), order=4)) +
  x_at_zero() +
  scale_x_continuous(expand=expansion(mult=c(.01,.01)), breaks=function(x)round(x[1]:x[2],0)) +
  labs(x='', fill='Added generation', y='Annual increase, TWh/year',
       title='Electricity demand growth vs. clean power additions in China',
       caption='Annual capacity additions converted to generation using average capacity factors') -> p
quicksave(file.path(output_dir, 'Electricity demand growth vs clean power additions in China.png'),
          plot=p, scale=1)


gen_cons_yr_norm %>% filter(year>=start_yr) %>% select(source, year, TWh) %>%
  bind_rows(average_demand_growth %>% mutate(source='Average demand growth')) %>%
  write_csv(file.path(output_dir, 'Electricity demand growth vs clean power additions in China.csv'))


#added generation from 2023 installations
gen_cons %>% filter(year(date)==2022, source=='Demand') %>% summarise(across(TWh, sum)) -> total_demand

new_cap %>% filter(date=='2023-06-30') %>%
  select(source, GW) %>% mutate(source=disambiguate(source, yearly_cf$source)) %>%
  left_join(yearly_cf) %>%
  mutate(TWh=GW*8.76*mean_utilization,
         total_TWh=sum(TWh),
         growth_covered=total_TWh*2/total_demand$TWh)
