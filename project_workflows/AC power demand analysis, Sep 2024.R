source('scripts/load_package.R')

last_month=ymd('2024-09-30')

output_dir='outputs/AC_power_demand_Sep2024'
dir.create(output_dir)

logo=F

make_filename=function(title) file.path(output_dir,
                                        paste0(title,
                                               ifelse(logo, '', ', no logo'),
                                               '.png'))

paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       '&region_type=gadm1',
       #'&region_id=CN',
       '&region_iso2=CN') %>% read_csv ->
  met_prov

paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD,temperature',
       '&format=csv',
       #'&region_type=country',
       '&region_id=CN') %>% read_csv ->
  met_natl


paste0('https://api.energyandcleanair.org/v1/',
       'weather?',
       'variable=HDD,CDD',
       '&format=csv',
       #'&aggregate_by=country',
       '&region_type=country',
       '&date_from=2023-01-01',
       '&date_to=2023-01-01') %>% read_csv ->
  met_global

pop <- read_csv(get_data_file('population_by_province_2022.csv'))

power_demand_prov <- readwindEN(get_data_file('Electricity Consumption by province_2023.xlsx'),
                                c('prov', 'var'), skip=3,
                                read_vardata = T) %>%
  rename(region_name=prov)

power_demand_natl <- readwindEN(get_data_file('Electricity Consumption by sector_2023.xlsx'),
                                c('var', 'sector'), skip=3,
                                read_vardata = T) %>%
  mutate(sector=na.cover(sector, gsub('.* by ', '', var)))

power_demand_natl_broad <- readwindEN(get_data_file('Electricity Consumption by sector YTD.xlsx'),
           c('var', 'sector'), skip=3,
           read_vardata = T) %>%
  mutate(sector=ifelse(sector=='YTD', gsub('.* by ', '', var), sector)) %>%
  group_by(sector) %>% unYTD



#data prep
met_prov %>% mutate(date = date %>% 'day<-'(days_in_month(.)),
                    region_name=fix_province_names(region_name)) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  met_prov_monthly

met_natl %>% mutate(date = date %>% 'day<-'(days_in_month(.))) %>%
  group_by(region_name, variable, date) %>% summarise(across(value, mean)) ->
  met_natl_monthly

met_prov_monthly %>% filter(date<=last_month) %>%
  spread(variable, value) %>%
  left_join(power_demand_prov %>% rename(power_demand=Value)) ->
  power_demand_prov

power_demand_prov %<>%
  group_by(region_name) %>%
  mutate(across(c(power_demand, HDD, CDD),
                list(yoy_perc=~get.yoy(.x, date),
                     yoy_abs=~get.yoy(.x, date, 'absolute'))))

power_demand_natl_broad %>% rename(power_demand_ytd=Value, power_demand=Value1m) %>%
  mutate(sector_category='broad') %>%
  bind_rows(power_demand_natl %>% mutate(sector_category='granular')) %>%
  left_join(met_natl_monthly %>% spread(variable, value)) %>%
  group_by(sector) %>%
  mutate(across(c(power_demand, HDD, CDD),
                list(yoy_perc=~get.yoy(.x, date),
                     yoy_abs=~get.yoy(.x, date, 'absolute')))) ->
  power_demand_all




#model building
#IEA: cooling used almost 400 TWh in 2017 https://www.iea.org/reports/the-future-of-cooling-in-china
power_demand_prov %>% filter(year(date)>2017) %>%
  lm(power_demand~(CDD+HDD)*date:region_name, data=.) -> m

m %>% summary

power_demand_prov %<>% ungroup %>% mutate(power_demand_predicted=predict(m, .))

power_demand_all %>% ungroup %>%
  filter(sector_category=='broad',
         grepl('Resid|Tertiary', sector),
         !is.na(power_demand+CDD+HDD)) -> attrib_data

attrib_data %>% lm(power_demand~sector+date:sector+(CDD+HDD):as.factor(year(date)):sector, data=.) -> m

m %>% summary

attrib_data %>%
  mutate(power_demand_predicted=predict(m, .)) %>%
  group_by(sector, month(date)) %>%
  mutate(across(c(CDD, HDD), ~.x[year(date)==2015])) %>% ungroup %>%
  mutate(power_demand_with_2015_weather=predict(m, .)) %>%
  mutate(CDD=0, HDD=0) %>%
  mutate(power_demand_no_heating_cooling=predict(m, .),
         power_demand_heating_cooling=pmax(0, power_demand_predicted-power_demand_no_heating_cooling),
         power_demand_heating_cooling_with_2015_weather=pmax(0, power_demand_with_2015_weather-power_demand_no_heating_cooling),
         power_demand_other=power_demand-power_demand_heating_cooling) %>%
  pivot_longer(starts_with('power_demand')) %>%
  filter(name %in% c('power_demand_heating_cooling', 'power_demand_heating_cooling_with_2015_weather')) %>%
  mutate(label=case_when(grepl('2015', name)~'with constant 2015 weather',
                         T~'actual')) ->
  cooling_demand

cooling_demand %>%
  group_by(label, date) %>% summarise(across(value, sum)) %>%
  mutate(value=(value/10+65/12)*1.5, value_12m=rollapplyr(value, 12, mean, fill=NA)) %>%
  ggplot(aes(date, value, linetype=label)) +
  geom_line(aes(linewidth='monthly')) + #col=sector,
  geom_smooth(aes(linewidth='12-month mean'), col=crea_palettes$CREA['Dark.red'], se=F) +
  #geom_line(aes(y=value_12m), col=crea_palettes$CREA['Dark.red'], linewidth=1) +
  #facet_wrap(~sector, ncol=1) +
  scale_linetype_discrete(name='', guide=guide_legend(nrow=2)) +
  scale_color_crea_d(col.index=c(13:14,11), guide='none') +
  theme_crea(legend.position = 'top') +
  x_at_zero() +
  scale_x_date(limits = ymd(c('2016-01-01', NA)), expand=expansion(mult=c(.02,.02))) +
  scale_linewidth_manual(values=c(1,.75), name='') +
  labs(x='', y='TWh/month', title='Electricity demand for heating and cooling',
       caption='CREA estimates based on regression model of monthly power demand data\nand heating and cooling degree days derived from ERA5 data') -> p

quicksave(make_filename('Electricity demand for heating and cooling'), plot=p,scale=.8, logo = logo)


cooling_demand %>% filter(name=='power_demand_heating_cooling', !grepl('Whole', sector)) %>% group_by(date) %>%
  summarise(across(c(power_demand=value), ~sum(.x)+2000/12)) %>%
  mutate(sector='Heating and cooling') -> cooling_demand_total


power_demand_all %>% filter(sector_category=='broad', !grepl('Whole', sector)) %>%
  mutate(sector=case_when(grepl('Resid|Tertiary', sector)~'Other residential and service use',
                          T~'Primary&secondary industry')) %>%
  group_by(date, sector) %>%
  summarise(across(power_demand, sum)) %>%
  bind_rows(cooling_demand_total) %>%
  mutate(power_demand=case_when(grepl('resid', sector)~if_null(power_demand-power_demand[grepl('cooling', sector)]),
                                T~power_demand)) %>% filter(year(date)>=2015) %>%
  group_by(sector) %>% mutate(power_demand_12m=rollapplyr(power_demand, 12, sum, fill=NA)) %>%
  ungroup %>% mutate(sector=factor(sector, levels=sector %>% unique %>% sort %>% rev)) %>%
  na.omit ->
  sector_plotdata

sector_plotdata %>%
  ggplot(aes(date, power_demand_12m/10, col=sector)) +
  geom_line(linewidth=1.5) +
  geom_smooth(data=sector_plotdata %>% filter(date<='2020-01-01'), method='lm', se=F,
              aes(linetype='pre-Covid trend'), fullrange=T) +
  x_at_zero() +
  labs(subtitle='12-month moving sum', title='Electricity consumption by end use', x='', y='TWh/year') +
  theme_crea() +
  scale_color_crea_d(col.index = c(1,4,11), name='') +
  scale_linetype_manual(values='dotted', guide=guide_legend(override.aes = list(col='black')),
                        name='')-> p

quicksave(make_filename('Electricity consumption by end use'), plot=p,scale=.8, logo = logo)





#plots
power_demand_prov %>% filter(month(date) %in% 5:9, year(date)==2024, CDD>0) %>%
  ggplot(aes(CDD_yoy_abs, power_demand_yoy_perc, col=region_name)) + geom_line() +
  scale_color_crea_d(guide='none', col.index = rep(1:14, 100)) +
  geom_text_repel(aes(label=region_name), size=1.5,
                  data=power_demand_prov %>% filter(month(date) == 9, year(date)==2024, CDD>0)) +
  theme_crea(axis.title=element_text(size=rel(.8)), axis.text=element_text(size=rel(.7))) +
  labs(x='increase in average cooling loads, degrees, year-on-year', y='increase in power demand, %, year-on-year',
       title='Increase in average cooling load and power demand',
       subtitle='May-September, month-by-month data by province')

power_demand_prov %>% filter(month(date) %in% 8:9, year(date)==2024, CDD>0) %>%
  group_by(region_name) %>% summarise(across(is.numeric, mean)) %>%
  ggplot(aes(CDD_yoy_abs, power_demand_yoy_perc, label=region_name)) +
  geom_smooth(se=F, method = 'lm', color=crea_palettes$dramatic[1], linetype='dashed') +
  geom_point() + geom_text_repel(size=3) +
  theme_crea(axis.title=element_text(size=rel(.8)), axis.text=element_text(size=rel(.7))) +
  labs(x='increase in average cooling loads, degrees, year-on-year', y='increase in power demand, %, year-on-year',
       title='Increase in average cooling load and power demand',
       subtitle='August-September average by province') +
  scale_y_continuous(labels=scales::percent) -> p
quicksave(make_filename('CDD vs power demand by province'), plot=p,
          scale=.7, logo = logo)

coeff=5
power_demand_all %>% ungroup %>%
  filter(month(date) %in% 5:9, year(date)==2024, sector_category=='broad') %>%
  mutate(sector=ifelse(grepl('Resid', sector), 'Residential', sector)) -> plotdata

ggplot(mapping=aes(date, y=power_demand_yoy_perc*coeff, col=sector)) +
  geom_col(aes(y=CDD_yoy_abs, fill='cooling loads'), data=plotdata %>% distinct(date, .keep_all = T), col=NA) +
  geom_line(aes(linetype='power demand growth'), linewidth=2, data=plotdata) +
  scale_y_continuous(sec.axis = sec_axis( trans=~./coeff, name="power demand growth, year-on-year", labels=scales::percent),
                     expand=expansion(mult=c(0,.05))) +
  scale_color_crea_d(guide='none', col.index = c(1,2,4,5,6)) +
  scale_fill_crea_d('dramatic', name='') +
  geom_label_repel(aes(label=sector), xlim=ymd(c('2024-10-15', NA)), label.padding=.35,
                   data=plotdata %>% filter(month(date) == 9)) +
  theme_crea() +
  theme(plot.title=element_text(size=rel(1.2)),
        axis.title=element_text(size=rel(.8)), axis.text=element_text(size=rel(.7))) +
  labs(x='', y='increase in cooling loads, degrees, year-on-year', linetype='') +
  scale_x_date(expand = expansion(mult=c(.02, .7)), breaks=plotdata$date %>% unique,
               date_labels = '%b') +
  labs(title='Increases in power demand by sector and cooling loads, by month') ->
  p

quicksave(make_filename('CDD vs power demand by sector'), plot=p,
          scale=.7, logo = logo)

power_demand %>%
  pivot_longer(c(Value, Value_predicted)) %>%
  group_by(name, date) %>%
  summarise(across(value, sum)) %>%
  filter(year(date)>=2015) %>%
  #filter(month(date)==9) %>%
  ggplot(aes(date, value, col=name)) + geom_line()

power_demand %>%
  pivot_longer(c(Value, Value_predicted)) %>%
  group_by(name, date) %>%
  summarise(across(value, sum)) %>%
  group_by(name, month(date)) %>%
  mutate(yoy=value-lag(value)) %>%
  filter(year(date)>=2015) %>%
  #filter(month(date)==9) %>%
  ggplot(aes(date, yoy, col=name)) + geom_line()


met_prov_monthly %>% filter(year(date)>=2019, variable=='CDD') %>%
  mutate(year=as.factor(year(date)),
         plotdate=date %>% 'year<-'(2022)) %>%
  ggplot(aes(plotdate, value, col=year)) + geom_line() + facet_wrap(~region_name)


met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  group_by(region_name) %>% mutate(value_rel=value/mean(value)) %>%
  ggplot(aes(date, value-273.15, fill=value_rel)) +
  geom_col() + facet_wrap(~region_name) +
  scale_fill_crea_c('change', guide='none') + theme_crea()


met_prov_monthly %>% filter(month(date)==9, variable=='CDD') %>%
  group_by(region_name) %>% mutate(value_rel=value-mean(value)) %>%
  ggplot(aes(date - 9*30, value, fill=value_rel)) +
  geom_col() + facet_wrap(~region_name) +
  x_at_zero() +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) + theme_crea() +
  labs(title='Population-weighted average cooling degree days in China',
       subtitle='in September of each year, by province',
       y='degrees Celcius', x='')


met_natl_monthly %>% select(-value, value=value_urban) %>%
  filter(month(date) %in% 6:9, variable=='CDD') %>%
  mutate(value_rel=value-mean(value),
         month=factor(month.name[month(date)], levels=month.name)) %>%
  ggplot(aes(year(date), value, fill=value_rel)) +
  geom_col() +
  x_at_zero() +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dashed', color='black', alpha=.7,
              linewidth=1) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024), expand = expansion(mult=c(.02, .02))) +
  facet_wrap(~month) +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Cooling loads in China by year and month',
       subtitle='population-weighted average cooling degree days',
       y='degrees Celcius', x='')



met_prov_monthly %>%
  filter(month(date) %in% 6:9, variable=='temperature') %>%
  left_join(pop) %>%
  group_by(date) %>%
  summarise(across(value, ~weighted.mean(.x, population_total))) %>%
  mutate(value_rel=value-mean(value),
         month=factor(month.name[month(date)], levels=month.name)) %>%
  ggplot(aes(year(date), value-273.15, col=value_rel)) +
  geom_line(linewidth=2) + geom_point(size=1.5) +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dashed', color='black', alpha=.7,
              linewidth=1) +
  scale_x_continuous(breaks = c(2010,2015,2020,2024), expand = expansion(mult=c(.02, .02))) +
  facet_wrap(~month) +
  scale_color_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(panel.spacing = unit(1.5, "lines"),
             plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Temperatures in China by year and month',
       subtitle='population-weighted average',
       y='degrees Celcius', x='') -> p
quicksave(make_filename('national average temperature by year and month'), plot=p,
          scale=.7, logo = logo)




met_prov_monthly %>% filter(month(date)==9, variable=='CDD') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  summarise(value=value[year(date)==2024]-value[year(date)==2023]) %>%
  arrange(value) %>%
  mutate(region_name=factor(region_name, levels=region_name)) %>%
  ggplot(aes(region_name, value, fill=value)) +
  geom_col() +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Year-on-year change in population-weighted average cooling degree days',
       subtitle='September 2024, by province',
       y='degrees Celcius', x='') +
  coord_flip()



met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  ggplot(aes(year(date), value, col=value)) +
  geom_line(linewidth=1) +
  stat_smooth(geom='line', method='lm', se=F,
              linetype='dotted', color='black', alpha=.3,
              linewidth=.7) +
  facet_wrap(~region_name, scales='free_y') +
  scale_color_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Year-on-year change in population-weighted average cooling degree days',
       subtitle='September 2024, by province',
       y='degrees Celcius', x='')


met_prov_monthly %>% filter(month(date)==9, variable=='temperature') %>%
  mutate(region_name = fix_province_names(region_name)) %>%
  group_by(region_name) %>%
  summarise(anomaly=value[year(date)==2024]-mean(value[year(date)<=2023]),
            difference_with_previous_record=value[year(date)==2024]-max(value[year(date)<=2023])) %>%
  arrange(anomaly) %>%
  mutate(region_name=factor(region_name, levels=region_name)) %>%
  pivot_longer(-region_name) %>%
  ggplot(aes(region_name, value, fill=value)) +
  geom_col() +
  facet_wrap(~name) +
  scale_fill_crea_c('change', guide='none', col.index = c(1:3,6:7)) +
  theme_crea(plot.margin = unit(c(1.5,1,1,1), "lines")) +
  labs(title='Temperature anomalies by province',
       subtitle='September 2024',
       y='degrees Celcius', x='') +
  coord_flip()


#international comparison

require(wbstats)
require(countrycode)
require(ggrepel)

read_csv('https://api.energyandcleanair.org/energy/iea_balance?year=2023&format=csv&api_key=dc946f77140ef428d2a060537a84858cfbb13dd915a551ede6506f10f48457cc') -> codes

full_join(read_csv('data/IEA_CMCC_HDD16monthlyworldbypopallmonths.csv', skip=9),
          read_csv('data/IEA_CMCC_CDDhum21monthlyworldbypopallmonths.csv', skip=9)) %>%
  rename(CDD=contains('CDD'), HDD=contains('HDD')) ->
  dd_global


readRDS('G:/Shared drives/CREA-data/IEA paid datasets/World Energy Balances 2024/WBAL/WEB_2024.RDS') -> iea

iea %<>% select(-country) %>% left_join(codes %>% distinct(iso2c=iso2, countrycode=region_raw, country))

iea %<>% mutate(country=case_when(countrycode=='JAPAN'~'Japan', T~country),
                iso2c=case_when(countrycode=='JAPAN'~'JP', T~iso2c))

ind <- wb_indicators()
wbd <- ind %>% filter(grepl('^Population, total|GDP .constant 2015 US', indicator)) %>%
  use_series(indicator_id) %>%
  wb_data("all")

wbd %>% select(iso2c, year=date, gdp=contains('GDP'), pop=contains('POP')) %>%
  left_join(iea, .) -> iea

iea %>% filter(flow=='Residential', product=='Electricity', unit=='TJ') %>%
  filter(!is.na(value), !is.na(country)) %>%
  group_by(year) %>%
  mutate(gdp_percap=gdp/pop, gdp_rank=rank(-gdp), gdp_per_pop_rank=rank(-gdp_percap),
         mwh_percap=value/3.6*1000/pop) ->
  percap

focus_countries <- c('China', 'Singapore', 'Japan', 'South Korea',
  'Spain', 'Italy', 'Germany', 'Malaysia',
  'Argentina', 'Turkey', 'Portugal',
  'Mexico')

percap %>%
  group_by(iso2c) %>%
  #filter((gdp_rank[year==2022]<=40 & gdp/pop[year==2022]>12000) | iso2c=='CN') %>%
  filter(country %in% focus_countries) %>%
  ungroup() %>% filter(gdp_percap<50000) ->
  percap_plotdata

percap_plotdata %>% #filter(country=='Japan') %>%
  ggplot(aes(gdp_percap, mwh_percap, col=country)) +
  geom_smooth(se=F, span=.4) +
  #geom_line() +
  geom_label_repel(aes(label=country), data=percap_plotdata %>% group_by(country) %>% filter(year==max(year)),
                   size=2) +
  scale_color_crea_d(guide='none', col.index = rep(c(1,2,4:6,9:14), 100)) +
  theme_crea() + theme(plot.title=element_text(size=rel(1.2))) +
  x_at_zero() +
  scale_x_continuous(labels=scales::comma) +
  labs(title='Residential electricity consumption and GDP in selected countries',
       x='GDP per person, 2015$', y='MWh/person') ->
  p

quicksave(make_filename('residential power demand vs gdp by country'), plot=p,
          scale=.7, logo = logo)

dd_global %>%
  mutate(iso2c=countrycode(ISO3, 'iso3c', 'iso2c')) %>%
  group_by(iso2c, year=year(Date)) %>%
  summarise(across(c(HDD, CDD), sum)) %>%
  left_join(percap, .) ->
  percap

percap %>% filter(CDD>=800, CDD<2500, gdp_percap>8e3, gdp_percap<=40e3, year==2022) %>%
  ggplot(aes(country, mwh_percap)) + geom_col() + coord_flip()

percap %>% filter(!is.na(HDD+CDD+gdp_percap+mwh_percap)) %>%
  lm(mwh_percap~(HDD+CDD):poly(gdp_percap,2)+poly(gdp_percap,2), data=.) -> m_global

m_global %>% summary

percap %>%
  ungroup %>% mutate(mwh_percap_predicted=predict(m_global, .)) %>%
  filter(mwh_percap_predicted>0, mwh_percap_predicted<4) %>%
  ggplot(aes(mwh_percap_predicted, mwh_percap, col=country=='China', groups=country)) +
  geom_smooth(method='gam', se=F) + geom_abline()


