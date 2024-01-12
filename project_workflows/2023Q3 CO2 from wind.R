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
require(readxl)
require(zoo)

output_dir='outputs/2023Q3'
dir.create(output_dir)

last_month = ymd('2023-09-30')

in_file = get_data_file("apparent consumption of fossils.xlsx")
getwindvars(in_file) #%>% grep("YTD", ., value=T)

rw <- function(...) readwindEN(..., read_vardata = T, zero_as_NA = T)

rw(in_file, c('var', 'prod'), columnFilter = "YTD") -> d1
rw(in_file, c('var', 'prod', 'area', 'sector'), columnFilter = "Consump.*Steam") %>%
  mutate(type='M') -> d2
#rw(in_file, c('var', 'prod'), columnFilter = "(Imports|Exports).*(Oil Products)") -> d3
rw(in_file, c('var', 'prod'), columnFilter = "Consumption: Coking Coal$") %>%
  mutate(type='M', sector='Metallurgy Industry') -> d4

d = bind_rows(d1, d2, d4) %>% arrange(date) %>%
  mutate(YoY=!is.na(YoY), Value=Value*ifelse(Unit=="ton", 1e-4,1),
         Unit=recode(Unit, ton="10000 tons"),
         Value=ifelse(Value==0, NA, Value))

d %<>%
  group_by(var, prod, sector, type) %>%
  unYoY %>% filter(!YoY) %>%
  unYTD

d %<>% mutate(var = ifelse(var=='Apparent Consumption' & prod=='Natural Gas',
                           'Apparent Consumption WIND', var),
              Value1m=Value1m*ifelse(Unit %in% c('10k ton', '10000 tons') & prod=='Natural Gas', 1e4/0.657e-3/1e8, 1),
              Unit=ifelse(prod=='Natural Gas', '100M cu.m', Unit))


d %<>% filter(prod=='Crude Oil') %>% group_by(prod, date) %>% summarise_at('Value1m', sum) %>%
  mutate(var='Apparent Consumption') %>% bind_rows(d, .)

d %<>% filter(grepl('Processing of Petroleum|Finished Oil Products', prod)) %>%
  mutate(prod='Oil Products', Value1m = Value1m * ifelse(grepl('Exports', var), -1, 1)) %>%
  group_by(prod, date) %>% summarise(across(Value1m, sum)) %>%
  mutate(var='Apparent Consumption') %>% bind_rows(d, .)

#1 m3 = 0.76e-3 t

d %<>% filter(prod=='Natural Gas', grepl('Output|Imports', var)) %>%
  group_by(prod, date) %>% summarise(across(Value1m, sum)) %>%
  mutate(var='Apparent Consumption') %>%
  bind_rows(d, .)

d %>% filter(prod=='Natural Gas') %>% ggplot(aes(date, Value1m, col=var)) + geom_line() + facet_wrap(~var)
d %>% filter(grepl('Cons', var), !grepl('WIND', var), year(date)>=2021) %>%
  ggplot(aes(date, Value1m)) +
  geom_col() + facet_wrap(~prod+sector, scales='free_y')

d$Value1m[d$prod=='Steam Coal' & d$var=='Consumption' & d$date=='2014-01-31' & d$sector=='Total'] <-
  d$Value1m[d$prod=='Steam Coal' & d$var=='Consumption' & d$date=='2013-01-31' & d$sector=='Total'] * .9



d$Value1m[year(d$date)==2021 & month(d$date) %in% 7:9] %<>% multiply_by(.98)
d$Value1m[year(d$date)==2022 & month(d$date) %in% 1:3 & d$prod=='Oil Products'] %<>% multiply_by(.96)

#get bottom up estimates for last month when it is missing from data
source('project_workflows/coal use bottom-up, 2023Q2.R')

coaluse_fill %>% mutate(sector=case_when(sector_coal=='Heating'~'Heating System Industry',
                                         sector_coal=='Other'~'Other Sectors',
                                         sector_coal=='Coking'~'Metallurgy Industry',
                                         grepl('Power|Metal|Chemical', sector_coal)~paste(sector_coal, 'Industry'),
                                         T~sector_coal),
                        prod=case_when(sector_coal=='Coking'~'Coking Coal',T~'Steam Coal')) %>%
  ungroup %>% select(-name) %>%
  full_join(d) %>%
  group_by(sector, prod) %>%
  fill(var) %>%
  group_by(sector, prod, var) %>%
  group_modify(function(df, ...) {
    last_year_date <- df$date %>% 'year<-'(year(df$date)-1)
    last_year_value <- df$Value1m[match(last_year_date, df$date)]
    ind <- is.na(df$Value1m) & !is.na(df$YoY_pred)
    df$Value1m[ind] <- df$YoY_pred[ind] * last_year_value[ind]
    return(df)
  }) -> d




CO2.factor <- c(Cement = 858.200/233035.7, #Cement = 769.300/231624.90 #2020 data https://zenodo.org/record/5126601
                'Steam Coal' = .7*29.3*96.1/1e3/100,
                'Coking Coal' = 24*94.6/1e3/100,
                'Crude Oil' = 42.3*20.0*11/3/1e3/100,
                Gasoline = 45.8*19.88*11/3/1e3/100,
                Kerosene = 46.3*19.5*11/3/1e3/100,
                'Diesel Oil' = 45.3*20.0727*11/3/1e3/100,
                'Fuel Oil' = 40.5*21.4909*11/3/1e3/100,
                'Natural Gas' = 36.0*55.04/100/100,
                'Oil Products' = NA) %>%
  tibble(cat=names(.), ef=.)

d %>% filter(grepl('Diesel|Fuel Oil|Gasoline|Kerosene', prod), grepl('Consumption', var),
             year(date)==2021, month(date)==11) %>% ungroup %>%
  left_join(CO2.factor %>% rename(prod=cat)) %>% summarise(ef=weighted.mean(ef, Value)) %>% unlist ->
  CO2.factor$ef[CO2.factor$cat=='Oil Products']

d$cat = disambiguate(d$prod, CO2.factor$cat)
d$cat[d$prod == 'Kerosene'] <- 'Oil Products'
d$cat[d$prod == 'Petroleum Coke'] <- 'Oil Products'

oilprod = 'Oil Products' #c('Gasoline', 'Diesel Oil', 'Fuel Oil')
d %<>% group_by(var, sector, date) %>%
  group_modify(function(df, ...) {
    df$Value1m[df$prod=='Crude Oil'] %<>% subtract(sum(df$Value1m[df$prod %in% oilprod]))
    df
  })

d %<>% left_join(CO2.factor) %>% mutate(CO2 = Value1m * ef)


#adjustment to BP numbers
read_bp('G:/Shared drives/CREA-data/BP Statistical Review of World Energy/Statistical Review of World Energy Data.xlsx',
        'Coal Cons.*EJ|Oil Cons.*EJ|Gas Cons.*EJ', read_multiple = T, year=2022) %>%
  bind_rows() %>% filter(country=='China') %>% mutate(prod_bp=gsub(' .*', '', variable)) -> bp

d %<>% mutate(prod_bp=disambiguate(prod, c(Coal='Coal', Oil='Oil', Gas=' Gas')), year=year(date))

d %>% filter(grepl('Cons', var), is.na(sector) | sector=='Total' | prod=='Coking Coal', grepl('Oil Prod|Coal| Gas', prod)) %>%
  group_by(prod_bp, year) %>%
  summarise(across(CO2, sum)) ->
  d_yr

d_yr %>% inner_join(bp %>% rename(EJ_BP=value)) %>% na.omit %>%
  group_by(prod_bp) %>%
  mutate(adjustment=EJ_BP * CO2[year==2019] / EJ_BP[year==2019] / CO2) %>%
  select(prod_bp, year, adjustment) ->
  adj

d %>% left_join(adj) %>% group_by(prod_bp) %>%
  fill(adjustment, .direction='downup') %>%
  mutate(adjustment=na.cover(adjustment, 1),
         CO2=CO2*adjustment) ->
  d.adj

d.adj %<>% group_by(var, prod, sector, type) %>%
  roll12m(months=12, outcol='CO2_12m', incol='CO2') %>%
  roll12m(months=3, outcol='CO2_3m', incol='CO2')


bp %>% filter(year<=2011) %>% group_by(prod_bp) %>%
  mutate(bp_adj_hist=value/value[year==2011]) %>%
  select(year, prod_bp, bp_adj_hist) ->
  bp_hist

d.adj %>% filter(date=='2011-12-31',
                 var != 'Apparent Consumption WIND',
                 grepl('Consumpt', var), is.na(sector) | sector == 'Total' | prod=='Coking Coal',
                 !grepl('Cement', prod)) %>%
  select(-date, -year) %>%
  right_join(bp_hist) %>%
  mutate(CO2_12m = CO2_12m * bp_adj_hist,
         date=ymd(paste0(year, '-12-31'))) %>%
  bind_rows(d.adj %>% filter(date>'2012-01-01' | (grepl('Cement', prod) & month(date)==12))) ->
  d.adj2


#quarterly
d.adj2 %>% filter(date>'2000-12-01',date<=last_month,
                  #month(date) %in% c(3,6,9,11),
                  prod %in% c('Cement', 'Natural Gas', 'Steam Coal', 'Coking Coal', oilprod), #, 'Crude Oil'
                  var != 'Apparent Consumption WIND',
                  grepl('Consumpt', var) | prod=='Cement') %>%
  mutate(include_in_totals = is.na(sector) | sector=='Total' | prod=='Coking Coal') -> d.quarter


#adjust for bottom-up estimates
coaluse_plot %>% filter(sector_coal=='Power') %>% group_by(date) %>%
  mutate(adj = value - value[name=='reported']) %>%
  select(name, date, adj) %>%
  mutate(prod='Steam Coal') %>%
  cross_join(tibble(sector=c('Total', 'Power Industry'))) -> bottomup_adj

d.quarter %<>% cross_join(bottomup_adj %>% ungroup %>% distinct(name)) %>%
  left_join(bottomup_adj) %>%
  replace_na(list(adj=0)) %>%
  group_by(name, var, prod, sector) %>%
  roll12m(months=12, outcol='adj_12m', incol='adj') %>%
  roll12m(months=3, outcol='adj_3m', incol='adj') %>%
  mutate(CO2 = CO2 + adj*ef,
         CO2_3m = CO2_3m + adj_3m*ef,
         CO2_12m = CO2_12m + adj_12m*ef) %>%
  filter(!is.na(CO2_12m))

d.quarter %<>% filter(include_in_totals) %>%
  group_by(name, date) %>% summarise(across(c(starts_with('CO2')), sum)) %>%
  mutate(include_in_totals=F, prod='Total') %>%
  bind_rows(d.quarter)

d.quarter %>% filter(prod=='Total') %>% ggplot(aes(date, CO2_12m, col=name)) + geom_line()

d.quarter %<>% group_by(name, prod, sector) %>%
  mutate(YoY_change_3m = get.yoy(CO2_3m, date, 'absolute'),
         YoY_3m = get.yoy(CO2_3m, date, 'relative'),
         YoY_change_1m = get.yoy(CO2, date, 'absolute'),
         YoY_1m = get.yoy(CO2, date, 'relative'))


d.quarter %>% filter(date==last_month, prod=='Total') %>%
  select(name, prod, sector, CO2_3m, YoY_3m)


d.quarter %>% ungroup %>%
  dplyr::select(name, date, prod, sector, CO2, CO2_3m, CO2_12m) %>%
  write_csv(file.path(output_dir, 'CO2.csv'))

d.quarter %>% filter(include_in_totals | prod=='Total', name != 'reported') %>% ungroup %>%
  dplyr::select(name, date, prod, CO2_12m) %>% spread(prod, CO2_12m) %>%
  mutate(across(where(is.numeric), multiply_by, 12)) %>%
  write_csv(file.path(output_dir, '12-month CO2.csv'))


prodcols = c('black', crea_palettes$dramatic[c(6,5,3,2)], 'gray30') #
names(prodcols) = c('Crude Oil','Cement','Natural Gas','Oil Products','Coking Coal','Steam Coal')


d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  group_by(name, date) %>% summarise(across(CO2_12m, sum)) %>%
  filter(year(date) %in% 2013:2019) %>%
  lm(CO2_12m~date, data=.) ->
  m

d.quarter %>% filter(grepl('predicted', name)) %>% ungroup %>% distinct(date) %>% mutate(CO2_12m=predict(m, .)) ->
  trend

d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>% ungroup %>%
  bind_rows(trend %>% filter(year(date)>=2012) %>% mutate(prod='pre-COVID trend')) %>%
  mutate(CO2_12m=CO2_12m*12,
         variable="CO2 emissions, 12-month moving sum",
         unit='Mt CO2 / year') %>%
  select(variable, date, product=prod, value=CO2_12m, unit) %>%
  write_csv(file.path(output_dir, "CO2 12m sum.csv"))

d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  mutate(prod=factor(prod, levels=names(prodcols))) %>%
  ggplot(aes(date, CO2_12m*12)) +
  geom_area(aes(fill=prod)) +
  geom_line(aes(col='pre-COVID trend'),
            data=trend %>% filter(year(date)>=2012),
            linetype='dashed', size=1) +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(c(0,.05)), labels = scales::comma) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  scale_color_crea_d(name='') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="12-month moving sum", y='Mt CO2 / year', x='') +
  theme(plot.title = element_text(size=rel(1.8))) -> plt
quicksave(file.path(output_dir, 'CO2 12m sum.png'), plot=plt)



d.quarter %>% filter(prod=='Total', grepl('predicted', name),
                     month(date) %in% (3*1:4),
                     year(date)>=2015) %>%
  write_csv(file.path(output_dir, 'CO2 quarterly.csv')) %>%
  ggplot(aes(date-45, CO2_3m*3)) +
  geom_col(aes(fill=month(date) == month(last_month))) +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  theme_crea() +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[2], crea_palettes$dramatic[1])), guide='none') +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y', expand=expansion(mult=c(.01,.01))) +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="Quarterly", y='Mt CO2 / quarter', x='') -> plt
quicksave(file.path(output_dir, 'CO2 quarterly.png'), plot=plt, footer_height=.025)




d.quarter %>% group_by(name, sector, prod) %>%
  mutate(YoY = get_yoy(CO2_3m, date)) %>% filter(date==last_month) %>% select(YoY) %>% data.frame




prodcolsZH <- prodcols
names(prodcolsZH) %<>% translateFuels

d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  mutate(prod=prod %>% translateFuels %>% factor(levels=names(prodcolsZH))) %>%
  ggplot(aes(date, CO2_12m*12/100, fill=prod)) +
  geom_area() +
  scale_x_date(expand=c(0,0), labels=function(x) paste0(year(x), '年')) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  theme_crea() +
  scale_fill_manual(values=prodcolsZH, name='产品') +
  labs(title="中国能源和水泥行业的二氧化碳排放量",
       subtitle="12月移动总计", y='亿吨/年', x='') +
  theme(plot.title = element_text(size=rel(1.8))) -> plt
quicksave(file.path(output_dir, 'CO2 12m sum ZH.png'), plot=plt)


d.quarter %>% filter(include_in_totals | prod=='Total', grepl('predicted', name)) %>%
  mutate(YoY_change_3m=YoY_change_3m*3, CO2_12m=CO2_12m*12) %>% ungroup %>%
  select(date, prod, YoY_change_3m, CO2_12m) %>%
  write_csv(file.path(output_dir, 'CO2 quarterly.csv'))

d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  ggplot(aes(date, YoY_change_1m)) + geom_col(aes(fill=prod)) +
  geom_point(data=d.quarter %>% filter(prod=='Total', grepl('predicted', name)), aes(col=prod)) +
  scale_x_date(expand=c(0,0), limits = c(ymd('2018-01-31'), NA)) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  scale_color_manual(values='steelblue', name='') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="monthly change, year-to-year", y='Mt CO2 / month', x='') +
  theme(plot.title = element_text(size=rel(1.8))) -> plt
quicksave(file.path(output_dir, 'CO2 monthly change by fuel.png'), plot=plt, scale=1.33)

d.quarter %>% filter(prod=='Total', grepl('predicted', name)) %>%
  write_csv(file.path(output_dir, 'CO2 monthly change.csv')) %>%
  ggplot(aes(date, YoY_change_1m, fill=YoY_change_1m<0)) + geom_col() +
  scale_x_date(expand=c(0,0), limits = c(ymd('2018-01-01'), NA)) +
  theme_crea() +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="monthly change, year-to-year", y='Mt CO2 / month', x='') +
  theme(plot.title = element_text(size=rel(1.8))) +
  scale_fill_crea_d('change', col.index = c(7, 1), guide='none') -> plt
quicksave(file.path(output_dir, 'CO2 monthly change.png'), plot=plt, scale=1.33)


d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  mutate(prodZH = translateFuels(prod)) %>%
  ggplot(aes(date, YoY_change_3m*3/100)) +
  geom_col(aes(fill=prodZH)) +
  geom_point(data=d.quarter %>% filter(prod=='Total', grepl('predicted', name)) %>% mutate(prod='总计'), aes(col=prod)) +
  scale_x_date(expand=c(0,0), limits = c(ymd('2018-01-31'), NA)) +
  theme_crea() +
  scale_fill_manual(values=prodcolsZH, name='燃料或产品') +
  scale_color_manual(values='steelblue', name='') +
  labs(title="中国能源和水泥行业的二氧化碳排放量",
       subtitle="季节同比变化", y='亿吨二氧化碳 / 季度', x='') +
  theme(plot.title = element_text(size=rel(1.8))) -> p
quicksave(file.path(output_dir, 'CO2 quarterly change ZH.png'), plot=p)


d.quarter %>% filter(!(sector == 'Total' & prod == 'Steam Coal'),
                     grepl('predicted', name)) %>%
  mutate(sector = case_when(prod=='Cement'~'Building Materials',
                            prod %in% c('Crude Oil', oilprod)~'Oil Consumption',
                            is.na(sector)&prod!='Total'~paste(prod,'Consumption'),
                            prod=='Total'~'Total',
                            T~sector)) %>%
  group_by(prod, sector, date) %>% mutate(across(YoY_change_3m, mean)) %>%
  mutate(sector=recode(sector, Total='All Sectors', 'Oil Consumption'='All Sectors', 'Natural Gas Consumption'='All Sectors')) ->
  d.changes

d.changes %>% filter(prod != 'Total', date==last_month) %>%
  ggplot(aes(prod, YoY_change_3m, fill=sector)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_crea_d(name='Sector') +
  labs(title="Contributions to changes in emissions",
       subtitle="in the third quarter of 2023, compared with 2022", y='Mt CO2 / year', x='') +
  coord_flip() +
  #x_at_zero() +
  scale_x_discrete(limits=rev) -> plt
quicksave(file.path(output_dir, 'Contributions to emissions growth.png'), plot=plt, scale=1.33)

d.changes %>% filter(date==last_month) %>%
  select(sector, prod, YoY_change_3m, YoY_3m, CO2_3m) %T>% copy.xl %>%
  write_csv(file.path(output_dir, 'Contributions to emissions growth.csv'))



d.changes %>%
  ggplot(aes(date, YoY_change_3m, fill=sector)) +
  facet_wrap(~prod) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_crea_d(name='Sector') +
  labs(title="Contributions to changes in emissions",
       subtitle="in the third quarter of 2023, compared with 2022", y='Mt CO2 / year', x='') -> plt
quicksave(file.path(output_dir, 'Contributions to emissions growth time series.png'), plot=plt, scale=1.33)


