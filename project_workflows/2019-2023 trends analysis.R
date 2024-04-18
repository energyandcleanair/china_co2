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


elec_cons_sector %>%
  #fill missing data with previous year values
  group_by(month(date), sector, subsector) %>% fill(Value, .direction='down') %>%
  filter(!is.na(Value), is.na(subsector)) %>%
  group_by(broad_sector, sector, subsector) %>%
  summarise(change=Value12m[date=='2023-12-31']-Value12m[date=='2019-12-31']) %>%
  ungroup %>% arrange(-change) -> elec_plot

elec_plot %>% mutate(sector=case_when(sector %in% elec_plot$sector[1:20]~sector, T~'Others')) %>%
  group_by(broad_sector, sector) %>%
  summarise(across(change, sum)) %>%
  mutate(share_within_broad_sector=change/sum(change)) %>%
  ungroup %>%
  mutate(share_overall=change/sum(change)) %>%
  write_csv(file.path(output_dir, 'Drivers of electricity consumption growth.csv')) %>%
  #arrange(change_3m) %>% mutate(sector=factor(sector, levels=sector)) %>%
  ggplot(aes(broad_sector, change*12/1e5, fill=sector)) + geom_col() +
  geom_text(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, color='white') + #, fill='white', check_overlap=F
  #geom_label(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, fill='white', label.padding=unit(0, 'cm')) +
  scale_fill_manual(values=rep(unname(crea_palettes$CREA[c(1:7,14)]),100), guide='none') +
  x_at_zero() + theme_crea() +
  labs(title='Drivers of electricity consumption growth',
       subtitle = 'Change from 2019 to 2023',
       y='TWh', x='') -> p
quicksave(file.path(output_dir, 'Drivers of electricity consumption growth.png'), plot=p, scale=1.33, footer_height=.03, logo=F)


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






infile <- "inst/extdata/Electricity Consumption by key sector.xlsx"
infile %>% getwindvars(skip=1)
readwindEN(infile, c('var', 'sector'), read_vardata = T, zero_as_NA = T, columnFilter = 'Whole Society') -> elec_cons_total


elec_cons_total %>% group_by(year=year(date)) %>%
  summarise(across(Value, sum)) %>%
  mutate(Value=Value/lag(Value)-1) %>%
  filter(year %in% 2015:2022) %>% spread(year, Value) %>% copy.xl()
