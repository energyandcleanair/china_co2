require(chinatracker)
require(tidyverse)
require(magrittr)
require(rcrea)
require(creahelpers)
library(directlabels)
require(ggrepel)
require(lubridate)
require(grid)
require(gridExtra)
require(cowplot)

output_dir <- "H:/My Drive/ASPI"

infile <- "inst/extdata/Electricity Consumption by Province_2023.xlsx"
infile %>% getwindvars()
readwindEN(infile, c('prov', 'var'), read_vardata = T, skip=3) -> elec_cons_prov


infile <- "inst/extdata/Electricity Consumption by sector_2023.xlsx"
infile %>% getwindvars(skip=3)
readwindEN(infile, c('var', 'sector', 'subsector'), read_vardata = T, zero_as_NA = T, skip=3) -> elec_cons_sector

elec_cons_sector %<>% mutate(sector=case_when(grepl('Households', Name)~'Households', T~sector),
                             subsector=case_when(var=='Electricity Consumption by Urban Households'~'Urban',
                                                 var=='Electricity Consumption by Rural Households'~'Rural',
                                                 T~subsector))


elec_cons_sector %<>%
  #fill missing data with previous year values
  group_by(month(date), sector, subsector) %>% fill(Value, .direction='down') %>%
  group_by(sector, subsector) %>%
  roll12m(months=12, incol='Value')

elec_cons_sector %>%
  roll12m(months=3, incol='Value') %>%
  mutate(YoY_3m = get.yoy(Value3m, date),
         change_3m = get.yoy(Value3m, date, type='absolute'))


elec_cons_sector %<>% filter(!grepl('^Animal|Agriculture$|^Industry$|^Manufacturing$|^Fishery|^Forestry|^Post|Whole Society|Conservancy$|Education$|Heat$', sector),
                            !is.na(sector)) %>%
  mutate(broad_sector = case_when(grepl('Manufac|Construction|Processing|Extraction|Mining|Production|Repair|Smelting|Geologic', sector)~'Industry',
                                  grepl('Agriculture|Households', sector)~'Residential&Rural',
                                  grepl('Printing|Wholesale|Transport|Services|Real Estate|Education|Finance|Social|Storage|Public|Telecomm|Management', sector)~'Services'))


focus_period <- seq.Date(ymd('2024-03-01'),ymd('2024-09-30'), by='day')
base_period <- focus_period %>% 'year<-'(year(.)-1)
period_name <- 'from March-September 2023 to 2024'
fn <- file.path(output_dir, paste('Drivers of electricity consumption growth,', period_name))

elec_cons_sector %>%
  #fill missing data with previous year values
  group_by(month(date), sector, subsector) %>% fill(Value, .direction='down') %>%
  filter(!is.na(Value), is.na(subsector)) %>%
  group_by(broad_sector, sector, subsector) %>%
  #summarise(change=Value12m[date=='2023-12-31']-Value12m[date=='2019-12-31']) %>%
  summarise(change=sum(Value[date %in% focus_period])-sum(Value[date %in% base_period])) %>%
  ungroup %>% arrange(-change) -> elec_plot

elec_plot %>% mutate(sector=case_when(sector %in% elec_plot$sector[1:20]~sector, T~'Others')) %>%
  group_by(broad_sector, sector) %>%
  summarise(across(change, sum)) %>%
  mutate(share_within_broad_sector=change/sum(change),
         change=change/1e5,
         Unit='TWh') %>%
  ungroup %>%
  mutate(share_overall=change/sum(change)) %>%
  write_csv(paste0(fn, '.csv')) %>%
  #arrange(change_3m) %>% mutate(sector=factor(sector, levels=sector)) %>%
  ggplot(aes(broad_sector, change, fill=sector)) + geom_col() +
  geom_text(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, color='white') + #, fill='white', check_overlap=F
  #geom_label(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, fill='white', label.padding=unit(0, 'cm')) +
  scale_fill_manual(values=rep(unname(crea_palettes$CREA[c(1:7,14)]),100), guide='none') +
  x_at_zero() + theme_crea() +
  labs(title='Drivers of electricity consumption growth',
       subtitle = paste('Change', period_name),
       y='TWh', x='') -> p
quicksave(paste0(fn, '.png'), plot=p, scale=1.33, logo=F)


elec_cons_sector %>%
  filter(!is.na(Value), is.na(subsector)) %>%
  group_by(broad_sector, sector, subsector) %>%
  summarise(change_2016_2019=(Value12m[date=='2019-12-31']-Value12m[date=='2016-12-31'])/3,
            change_2019_2023=(Value12m[date=='2023-12-31']-Value12m[date=='2019-12-31'])/4) %>%
  mutate(acceleration=change_2019_2023-change_2016_2019) %>%
  ungroup %>% arrange(-acceleration) -> elec_change

elec_change %>%
  ggplot(aes(broad_sector, acceleration*12/1e5, fill=sector)) + geom_col() +
  geom_text(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, color='white') + #, fill='white', check_overlap=F
  #geom_label(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, fill='white', label.padding=unit(0, 'cm')) +
  scale_fill_manual(values=rep(unname(crea_palettes$CREA[c(1:7,14)]),100), guide='none') +
  x_at_zero() + theme_crea() +
  labs(title='Drivers of electricity consumption acceleration',
       subtitle = 'Change from 2019 to 2023, compared with 2016 to 2019',
       y='TWh', x='')


#province breakdown
pop <- read_csv(get_data_file('population_by_province_2022.csv'))

elec_cons_prov %>%
  group_by(prov) %>%
  summarise(cons = sum(Value[date %in% focus_period]),
            change_abs  = sum(Value[date %in% focus_period])-sum(Value[date %in% base_period]),
            change_perc = sum(Value[date %in% focus_period])/sum(Value[date %in% base_period])-1) ->
  prov_changes

prov_changes %>%
  arrange(change_perc) %>% mutate(prov=factor(prov, levels=prov)) %>%
  ggplot(aes(prov, change_perc)) + geom_col() + coord_flip()

prov_changes %>%
  left_join(pop %>% mutate(prov=chinatracker::fix_province_names(region_name))) %>%
  mutate(across(c(cons, change_abs), ~.x/population_total)) %>%
  ggplot(aes(cons, change_perc, label=prov)) + geom_point() + geom_text_repel()

#output annual data
elec_cons_sector %>%
  filter(!is.na(Value), month(date)==12) %>%
  mutate(year=year(date)) %>%
  select(sector, subsector, year, Value12m) %>%
  spread(year, Value12m) %>% write_csv('H:/My Drive/ASPI/electricity consumption by sector and year.csv')

elec_cons_sector %>% distinct(sector, subsector) %>% View

elec_cons_prov %<>% group_by(prov) %>%
  roll12m(months=12, incol='Value')


elec_cons_prov %>%
  filter(!is.na(Value), month(date)==12) %>%
  mutate(year=year(date)) %>%
  select(prov, year, Value12m) %>%
  spread(year, Value12m) %>% write_csv('H:/My Drive/ASPI/electricity consumption by province and year.csv')


#effect of heating and cooling
read_csv('https://api.energyandcleanair.org/v1/weather?variable=HDD,CDD&format=csv&region_id=CN') -> dd

dd %>% group_by(region_name, variable, date=date %>% 'day<-'(days_in_month(date))) %>%
  summarise(across(value, mean)) -> dd_monthly

dd_monthly %>% ggplot(aes(date, value, col=variable)) + geom_line()
dd_monthly %>% ggplot(aes(month(date), value, col=as.factor(year(date)))) + geom_line() + facet_wrap(~variable)

dd_monthly %>% spread(variable, value) %>%
  left_join(elec_cons_sector, .) -> elec_dd

dd_monthly %>% group_by(year=year(date), variable) %>%
  filter(n()==12) %>%
  summarise(across(value, mean)) %>%
  ggplot(aes(year, value)) + geom_col() + geom_smooth(method='lm', se=F, fullrange=T) + facet_wrap(~variable)

elec_dd %>%
  lm(Value~(CDD+HDD):sector+sector:date, data=.) %>%
  summary


buildings='Wholesale|Households|Hotels|Public Service|Agriculture'

elec_plot %>% filter(is.na(subsector)) %>% group_by(buildings=grepl(buildings, sector)) %>%
  summarise(across(change, sum)) %>% summarise(share=change[buildings]/sum(change))

elec_dd %<>% filter(grepl(buildings, sector),
                    is.na(subsector)) %>%
  mutate(sector=ifelse(is.na(subsector), sector, paste(sector, '-', subsector))) %>%
  group_by(date) %>% summarise(across(Value, sum, na.rm=T), across(c(CDD, HDD), unique)) %>%
  mutate(sector='Buildings', month=as.factor(month(date)))

#elec_dd %>% lm(Value~(CDD+HDD):sector + sector:date, data=.) -> m
#elec_dd %>% lm(Value~CDD+HDD+date+month, data=.) -> m
elec_dd %>% filter(month %in% c(4,5,10)) %>% lm(Value~date, data=.) -> m
elec_dd %>% lm(Value~(CDD+HDD)*date, data=.) -> m
summary(m)

elec_dd %>% #filter(grepl('Wholesale|Households|Hotels', sector)) %>%
  mutate(CDD=0) %>%
  predict(m, .) -> elec_dd$value_no_AC

elec_dd %>% #filter(grepl('Wholesale|Households|Hotels', sector)) %>%
  mutate(CDD=0, HDD=0) %>%
  predict(m, .) -> elec_dd$value_no_heat_cool

elec_dd %>% #filter(grepl('Wholesale|Households|Hotels', sector)) %>%
  mutate(HDD=0) %>%
  predict(m, .) -> elec_dd$value_no_heating

elec_dd %>% predict(m, .) -> elec_dd$value_pred

elec_dd %>% group_by(month(date)) %>%
  mutate(HDD=HDD[year(date)==2019]) %>%
  predict(m, .) -> elec_dd$value_constant_HDD

elec_dd %>% group_by(month(date)) %>%
  mutate(CDD=CDD[year(date)==2019]) %>%
  predict(m, .) -> elec_dd$value_constant_CDD


elec_dd %>% filter(!is.na(HDD)) %>% pivot_longer(starts_with('value'), names_to='variant') %>%
  ggplot(aes(date, value, col=variant)) + geom_line() + facet_wrap(~sector, scales='free_y')

elec_dd %>% filter(!is.na(HDD)) %>% pivot_longer(starts_with('value'), names_to='variant') %>%
  filter(grepl('pred|constant', variant)) %>%
  group_by(year=year(date), variant, sector) %>% summarise(across(value, sum)) %>%
  ggplot(aes(year, value, col=variant)) + geom_line() + facet_wrap(~sector, scales='free_y') +
  x_at_zero()


elec_dd %>% filter(!is.na(HDD)) %>%
  ggplot(aes(year(date), Value, col=as.factor(month(date)))) +
  geom_line() + facet_wrap(~sector, scales='free_y')


elec_dd %>%
  mutate(value_heat_cool=pmax(0, Value-value_no_heat_cool)) %>%
  group_by(year=year(date)) %>%
  summarise(across(starts_with('value'), sum)) %>%
  summarise(AC_demand_share=value_heat_cool[year==2023]/Value[year==2023],
            across(starts_with('value'), ~.x[year==2023]-.x[year==2019])) %>%
  mutate(heat_cool_growth_contribution=value_heat_cool/Value)






infile <- get_data_file("Electricity Consumption by key sector.xlsx")
infile %>% getwindvars(skip=1)
readwindEN(infile, c('var', 'sector'), read_vardata = T, zero_as_NA = T, columnFilter = 'Whole Society|Industry') -> elec_cons_total


elec_cons_total %>% group_by(year=year(date)) %>%
  summarise(across(Value, sum)) %>%
  mutate(Value=Value/lag(Value)-1) %>%
  filter(year %in% 2015:2022) %>% spread(year, Value) %>% copy.xl()

read_industrial_output() -> ind

ind %>% filter(grepl('Solar Cell|Battery', prod), is.na(battery_type) | battery_type=='Total') %>%
  mutate(Value=case_when(grepl('Solar', prod)~Value1m/100*0.35/.8, #350 kg/kw, 800 gCO2/kWhe
                         T~Value1m/1000*.108/.8)*100e3, #108 kg/kWh
         sector=case_when(prod=='Battery'~'Battery manufacturing',
                          T~'Solar PV manufacturing')) ->
           pv_and_battery

read_csv(file.path('outputs/monthly_snapshot_2024_11', 'vehicle production.csv')) -> evs

evs$variable %>% unique
evs %>% filter(grepl('cumulative sales', variable)) %>%
  mutate(sector='EV charging',
         Value=6.5*100e3*value/value[date=='2024-10-31']) ->
  ev_charging

elec_cons_sector$sector %>% unique %>% sort
elec_cons_sector %>%
  filter(grepl('Information Transmission|Whole Soc|Charging', sector)) %>%
  bind_rows(pwr_data$monthly %>% filter(var=='Consumption') %>% mutate(Value=Value1m*1e4) %>% rename(sector=source)) %>%
  bind_rows(pv_and_battery) %>%
  bind_rows(ev_charging) %>%
  filter(year(date)>2019) %>%
  ggplot(aes(date, Value/100e3, col=sector)) + geom_line(linewidth=1) + labs(title='Monthly electricity consumption in China', x='', y='TWh') +
  theme_crea_new() + theme(legend.text = element_text(size=rel(.8))) +
  x_at_zero() + scale_color_crea_d(col.index = c('Dark.red','Orange','Blue', 'Green', 'Dark.violet', 'Red'), guide=guide_legend(ncol=1))
