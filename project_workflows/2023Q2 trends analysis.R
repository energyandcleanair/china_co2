require(chinatracker)
require(tidyverse)
require(magrittr)
require(rcrea)
require(creahelpers)
require(directlabels)
require(ggrepel)
require(lubridate)
require(grid)
require(gridExtra)

"data/power generation by province and source.xlsx"

infile <- "data/Power Consumption by Province M.xlsx"
infile %>% getwindvars()
readwindEN(infile, c('var', 'prov'), read_vardata = T) -> elec_cons_prov


infile <- "data/Electricity Consumption by sector.xlsx"
infile %>% getwindvars()
readwindEN(infile, c('var', 'sector', 'subsector'), read_vardata = T, zero_as_NA = T) -> elec_cons_sector

infile <- "data/Electricity Consumption by key sector.xlsx"
infile %>% getwindvars()
readwindEN(infile, c('var'), columnFilter = 'Urban and Rural', read_vardata = T, zero_as_NA = T) %>%
  mutate(var='Electricity Consumption', sector='Households') %>% bind_rows(elec_cons_sector) -> elec_cons_sector


focus_month <- today() %>% subtract(30) %>% "day<-"(1)


#existing analysis: capacity additions, dams water level, transport volumes
#Kpler data
#China monthly coal imports Kpler.xlsx




elec_cons_prov %<>% group_by(prov) %>% unYTD %>% roll12m() %>% roll12m(months=3) %>%
  mutate(YoY_3m = get.yoy(Value3m, date),
         change_3m = get.yoy(Value3m, date, type='absolute'))

elec_cons_prov %>% ungroup %>% filter(date==max(date)) %>%
  arrange(change_3m) %>% mutate(prov=factor(prov, levels=prov)) %>%
  ggplot(aes(prov, change_3m, fill=YoY_3m)) + geom_col() + coord_flip()


elec_cons_sector %<>% group_by(sector, subsector) %>%
  roll12m(months=3, incol='Value') %>%
  roll12m(months=12, incol='Value') %>%
  mutate(YoY_3m = get.yoy(Value3m, date),
         change_3m = get.yoy(Value3m, date, type='absolute'))

elec_cons_sector %>% filter(!is.na(subsector)) %>% distinct(sector, subsector) %>% View
elec_cons_sector %>% filter(!is.na(subsector)) %>% distinct(sector) -> t1
elec_cons_sector %>% filter(is.na(subsector)) %>% distinct(sector) -> t2
t1$sector %whichnotin% t2$sector

elec_cons_sector %>% filter(sector=='Construction', is.na(subsector)) %>%
  replace_na(list(subsector='Total')) %>%
  ggplot(aes(date, Value12m*12/100e3)) + geom_line(linewidth=1.5, color=crea_palettes$dramatic[1]) + facet_wrap(~subsector) +
  x_at_zero() + theme_crea() + scale_x_date(date_breaks = '2 years', date_labels = '%Y') +
  labs(x='', y='TWh/year, 12-month moving sum') -> p1

elec_cons_sector %>% filter(sector=='Construction', !is.na(subsector), !is.na(Value12m)) %>%
  ggplot(aes(date, Value12m*12/100e3, color=subsector)) + geom_line(linewidth=1.5) + facet_wrap(~subsector) +
  x_at_zero() + theme_crea() +
  labs(x='', y='') + scale_color_crea_d('dramatic', guide='none') -> p2

plot_grid(p1, p2) -> p
title <- ggdraw() + draw_label("Power demand trends in China's construction sector", fontface='bold', color=crea_palettes$CREA[1], size=25)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) -> p
quicksave(file.path(output_dir, 'Power demand trends in Chinas construction sector.png'), plot=p, scale=1.33, footer_height=.03)

elec_cons_sector %<>% filter(!grepl('^Animal|Agriculture$|^Industry$|^Manufacturing$|^Fishery|^Forestry|^Post|Whole Society|Conservancy$|Education$|Heat$', sector),
                            !is.na(sector)) %>%
  mutate(broad_sector = case_when(grepl('Manufac|Construction|Processing|Extraction|Mining|Production|Repair|Smelting|Geologic', sector)~'Industry',
                                  grepl('Agriculture|Households', sector)~'Residential&Rural',
                                  grepl('Printing|Wholesale|Transport|Services|Real Estate|Education|Finance|Social|Storage|Public|Telecomm|Management', sector)~'Services'))

elec_cons_sector %>%
  filter(!is.na(Value)) %>%
  filter(date==max(date), is.na(subsector)) %>%
  ungroup %>% arrange(-change_3m) -> elec_plot

elec_plot %>% mutate(sector=case_when(sector %in% elec_plot$sector[1:20]~sector, T~'Others')) %>%
  group_by(broad_sector, sector) %>%
  summarise(across(change_3m, sum)) %>%
  #arrange(change_3m) %>% mutate(sector=factor(sector, levels=sector)) %>%
  ggplot(aes(broad_sector, change_3m*3/1e5, fill=sector)) + geom_col() +
  geom_text(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, color='white') + #, fill='white', check_overlap=F
  #geom_label(aes(label=str_wrap(sector, width=40)), position=position_stack(vjust=.5), size=3, fill='white', label.padding=unit(0, 'cm')) +
  scale_fill_manual(values=rep(unname(crea_palettes$CREA[c(1:7,14)]),100), guide='none') +
  x_at_zero() + theme_crea() +
  labs(title='Drivers of electricity consumption growth',
       subtitle = 'Change in the second quarter of 2023, compared with previous year',
       y='TWh', x='') -> p
quicksave(file.path(output_dir, 'Drivers of electricity consumption growth.png'), plot=p, scale=1.33, footer_height=.03)







in_file = "real estate indicators.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'type'), columnExclude = "Funds", read_vardata = T) -> re
readwindEN(in_file, c('var', 'source', 'type'), columnFilter = "Funds", read_vardata = T) -> re_funds
re_funds %<>% mutate(source = ifelse(type=='YTD', source, type), type='YTD')

re %<>% bind_rows(re_funds) %>%
  group_by(var, source, type) %>%
  group_modify(function(df, ...) { df %>% unYTD() %>% roll12m})

require(directlabels)
re %>% filter(year(date)>=2015, grepl('Funds', var), !grepl('Foreign', source)) %>%
  ggplot(aes(date, Value12m*12/10, col=source)) + geom_line(size=1) + geom_dl(aes(label=source), method=list('last.bumpup', cex=.8)) +
  theme_crea() + scale_color_crea_d(guide=F) +
  scale_x_date(expand=expansion(mult=c(0, .5))) +
  labs(title='Real estate financing in China',
       subtitle='12-month moving sum',
       y='CNY bln', x='')
ggsave('Real estate financing in China.png')

re %>% filter(year(date)>=2015, !grepl('Fund|Price', var)) %>%
  mutate(Value12m = Value12m / case_when(Unit=='CNY 100 mn'~ 10000,
                                         Unit=='10000 sq.m'~100),
         Unit = recode(Unit, 'CNY 100 mn'='CNY trn', '10000 sq.m'='mln m2')) %>%
  ggplot(aes(date, Value12m*12)) + geom_line(size=1) + facet_wrap(~var+Unit, scales='free_y') +
  theme_crea() +
  labs(title='Real estate volumes in China',
       subtitle='12-month moving sum',
       y='', x='') +
  expand_limits(y=0) +
  theme(strip.text = element_text(size=rel(.7), lineheight = .2))
ggsave('Real estate volumes in China.png', width=10, height=8)
re %>% write_csv('real estate.csv')

in_file = "real estate by province.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('prov', 'var', 'type')) -> re_provs

re_provs %<>%
  group_by(var, prov, type) %>%
  group_modify(function(df, k) { df %>% unYTD() %>% roll12m })

re_provs %>% filter(year(date) %in% 2020:2021, month(date) %in% 7:10, !grepl('Land', var)) %>%
  group_by(year=year(date), var, prov) %>%
  summarise(across(Value1m, mean)) %>%
  group_by(var, prov) %>% summarise(change_perc=Value1m[year==2021]/Value1m[year==2020]-1) -> re_changes

re_changes %>% filter(grepl('Newly Started|Total Sale', var)) %>%
  ggplot(aes(prov, change_perc, fill=change_perc>0)) + facet_wrap(~var) + geom_col() +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(limits=rev) +
  coord_flip(ylim=c(-.5, .5)) +
  theme_crea() +
  scale_fill_crea_d('change', col.index=c(1,7), guide='none') +
  labs(title='Real estate starts and sales by province',
       subtitle='July-October, change from 2020 to 2021',
       x='', y='')
ggsave('Real estate starts and sales by province.png')

re_changes %>% write_csv('Real estate starts and sales by province.csv')
