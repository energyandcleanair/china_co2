source('R/wind mapping functions.R')

require(creahelpers)
require(rcrea)

output_dir='outputs/2023Q1'
dir.create(output_dir)

load(file.path(output_dir, 'analysis.RData'))

in_file = "data/apparent consumption of fossils.xlsx"
getwindvars(in_file) #%>% grep("YTD", ., value=T)

rw <- function(...) readwindEN(..., , read_vardata = T, zero_as_NA = T)

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
read_bp('Coal Cons.*EJ|Oil Cons.*EJ|Gas Cons.*EJ', read_multiple = T, year=2021) %>% 
  bind_rows() %>% filter(country=='China') %>% mutate(prod_bp=gsub(' .*', '', variable)) -> bp

d %<>% mutate(prod_bp=disambiguate(prod, c(Coal='Coal', Oil='Oil', Gas=' Gas')), year=year(date))

d %>% filter(grepl('Cons', var), sector=='Total' | prod=='Coking Coal', grepl('Oil Prod|Coal| Gas', prod)) %>% 
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
d.adj2 %>% filter(date>'2000-12-01',date<='2023-03-31',
                  #month(date) %in% c(3,6,9,11),
                  prod %in% c('Cement', 'Natural Gas', 'Steam Coal', 'Coking Coal', oilprod), #, 'Crude Oil' 
                  var != 'Apparent Consumption WIND',
                  grepl('Consumpt', var) | prod=='Cement') %>%
  mutate(include_in_totals = is.na(sector) | sector=='Total' | prod=='Coking Coal') -> d.quarter


#adjust for bottom-up estimates
source('project_workflows/coal use bottom-up, 2023Q1.R')

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


d.quarter %>% filter(date=='2023-03-31', prod=='Total') %>% 
  select(name, prod, sector, CO2_3m, CO2_3m, YoY_3m)


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
  scale_y_continuous(expand=expansion(c(0,.05))) +
  theme_crea() +
  scale_fill_manual(values=prodcols, name='Product') +
  scale_color_crea_d(name='') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="12-month moving sum", y='Mt CO2 / year', x='') +
  theme(plot.title = element_text(size=rel(1.8))) -> plt
quicksave(file.path(output_dir, 'CO2 12m sum.png'), plot=plt)


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


d.quarter %>% filter(include_in_totals) %>%
  mutate(prodZH = translateFuels(prod)) %>%
  ggplot(aes(date, YoY_change_3m*3/100)) + 
  geom_col(aes(fill=prodZH)) +
  geom_point(data=d.quarter %>% filter(prod=='Total') %>% mutate(prod='总计'), aes(col=prod)) +
  scale_x_date(expand=c(0,0)) +
  theme_crea() +
  scale_fill_manual(values=prodcolsZH, name='燃料或产品') +
  scale_color_manual(values='steelblue', name='') +
  labs(title="中国能源和水泥行业的二氧化碳排放量",
       subtitle="季节同比变化", y='亿吨二氧化碳 / 季度', x='') +
  theme(plot.title = element_text(size=rel(1.8)))
quicksave(file.path(output_dir, 'CO2 quarterly change ZH.png'))


d.quarter %>% filter(date=='2023-03-31', 
                     !(sector == 'Total' & prod == 'Steam Coal'),
                     grepl('predicted', name)) %>%
  mutate(sector = case_when(prod=='Cement'~'Building Materials',
                            prod %in% c('Crude Oil', oilprod)~'Oil Consumption',
                            is.na(sector)&prod!='Total'~paste(prod,'Consumption'),
                            prod=='Total'~'Total',
                            T~sector)) %>%
  group_by(prod, sector) %>% mutate(across(YoY_change_3m, mean)) -> d.changes

d.changes %>% filter(prod != 'Total') %>% 
  mutate(sector=recode(sector, Total='All Sectors', 'Oil Consumption'='All Sectors')) %>% 
  write_csv(file.path(output_dir, 'Contributions to emissions growth.csv')) %>% 
  ggplot(aes(prod, YoY_change_3m, fill=sector)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() +
  scale_fill_crea_d(name='Sector') +
  labs(title="Contributions to changes in emissions",
       subtitle="in the first quarter of 2023, compared with 2022", y='Mt CO2 / year', x='') +
  coord_flip() + 
  scale_x_discrete(limits=rev) +
  x_at_zero() -> plt
quicksave(file.path(output_dir, 'Contributions to emissions growth.png'), plot=plt, scale=1.33)

d.changes %>% select(sector, prod, YoY_change_3m) %>%
  write_csv(file.path(output_dir, 'Contributions to emissions growth.csv'))


in_file='data/Electricity Consumption by sector.xlsx'
getwindvars(in_file)
readwindEN(in_file,
           c('country', 'var', 'sector', 'subsector'),
           columnExclude = 'Urban and Rural') %>%
  mutate(type='M') -> elec

readwindEN(in_file,
           c('country', 'sector'),
           columnFilter='Urban and Rural') %>%
  mutate(type='M') %>% bind_rows(elec, .) -> elec


elec %>% group_by(var, sector, subsector) %>% 
  roll12m(months=3, incol='Value') %>% 
  mutate(YoY = get.yoy, col='Value3m') %>% 
  arrange(desc(Value3m)) %>% 
  filter(date==max(date), is.na(subsector) | subsector=="") %>% select(sector, YoY)

elec %>%
  mutate(sector = ifelse(grepl('Electricity, Gas and Water|Electricity and Heat', sector), subsector, sector)) %>%
  filter(sector %notin% c('Mining', 'Manufacturing', 'Industry', ''),
         is.na(subsector) | subsector=="", month(date) %in% 9) %>%
  group_by(sector, year=paste0('Y',year(date))) %>%
  summarise(across(Value, sum)) %>%
  spread(year, Value) %>% select(Y2020, Y2021) %>%
  ungroup %>%
  mutate(increase = Y2021-Y2020,
         growth=Y2021/Y2020-1,
         increase_share = increase / increase[sector=='Whole Society'][1],
         sector = orderfactor(sector, -increase)) %>%
  filter(sector!='Whole Society') %>%
  arrange(desc(abs(increase))) %T>%
  write_csv(file.path(output_dir, 'Contributions to electricity demand growth.csv')) %>%
  head(20) %>%
  ggplot(aes(sector, increase_share)) + geom_col() + coord_flip() +
  labs(title='Contributions to electricity demand growth',
       subtitle='Jan-May 2021 on year') +
  theme_crea() +
  theme(plot.title.position = 'plot')
ggsave(file.path(output_dir, 'Contributions to electricity demand growth.png'))


in_file='apparent consumption of fossils YTD M new.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod', 'country', 'sector'), read_vardata = T) %>% 
  mutate(sector=na.cover(sector, 'Total')) -> coaluse

coaluse %<>% filter(var=='Consumption', sector=='Total') %>% 
  group_by(var, date) %>% summarise(across(Value, sum)) %>% 
  rename(Value1m=Value)

in_file='Implied coal demand.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), read_vardata = T) %>% mutate(type=na.cover(type, '1m')) -> coal
coal %>% filter(prod %in% c('Raw Coal', 'Coal and Lignite'), !is.na(Value), is.na(YoY)) %>% 
  ddply(.(var, prod, type), unYTD) %>% 
  group_by(date) %>% group_modify(function(df, ...) {
    outdf=df[1,]
    outdf$var='Supply (output+net imports)'
    outdf$prod='Coal'
    
    outdf$Value1m = sum(c(df$Value1m[df$var=='Output' & is.na(df$YoY) & df$prod=='Raw Coal'],
                          df$Value1m[df$var=='Imports'],
                          -df$Value1m[df$var=='Exports']),
                        na.rm=T)
    return(outdf)
  }) -> coalsupply

adj = mean(coalsupply$Value1m[year(coalsupply$date)>=2014], na.rm=T) / mean(coaluse$Value1m[year(coaluse$date)>=2014], na.rm=T)
coaluse$Value1m %<>% multiply_by(adj)
bind_rows(coalsupply, coaluse) %>% 
  ddply(.(var), roll12m) %>% 
  filter(!is.na(Value1m), year(date)>=2010) %>% 
  ggplot(aes(date, Value12m*12/100e3, col=var)) + geom_line(size=1) +
  theme_crea() + scale_color_crea_d('dramatic', guide=guide_legend(nrow=1)) +
  labs(y='Gt/year', x='', title='Coal demand and supply in China', subtitle='12-month moving sum', col='',
       caption='CREA analysis of data from China National Bureau of Statistics, Customs and Wind Information') +
  theme(legend.position = 'top')
ggsave('coal demand vs supply.png')

in_file='Output of Major Industrial Products YTD M.xlsx'
#in_file='industrial output.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), read_vardata = T) %>% mutate(type=na.cover(type, '1m')) -> ind
readwindEN('power generation by source.xlsx', c('var', 'prod'), columnFilter = 'Nuclear', read_vardata = T) %>% 
  bind_rows(ind) -> ind


ind %<>% ddply(.(prod), unYTD) %>% ddply(.(prod), roll12m) %>% 
  ddply(.(prod), roll12m, months=3) %>% ddply(.(prod), get.yoy, col='Value1m')

ind %<>% ddply(.(var, prod), seasonal)

ind %<>% filter(!grepl('Smart Watch', prod))
plots = list('Industrial output'='',
             'Metals&cement output'='Steel Material|Crude Steel|Cement$|Coke|Pig Iron|Non-ferrous|Copper',
             'Heavy industry output'='Steel Material|Crude Steel|Cement$|Chemical|Plastic|Coke|Copper|Metals|Aluminous|Glass|Pig Iron',
             'Housing sales indicators'='Household|TV',
             'Transport fuel production'='Diese|Gasoline|Kerosene',
             'Solar cell output'='Solar Cells',
             'Power generation'='Solar$|Power$|Generating')

for(i in 1:length(plots)) {
  ind %>% filter(year(date)>=2015, grepl(plots[[i]], prod)) -> plotdata
  plotdata %>%
    mutate(YoY = (Value1m/lag(Value1m, 12)-1)  %>% pmax(-.2) %>% pmin(.2),
           prod = case_when(prod=='Generating Capacity'~'Electricity', 
                            prod=='Solar'~'Solar power',
                            T~gsub(' Generating Capacity', '', prod))) %>%
    ggplot(aes(date, Value.seasonadj, col=YoY))+
    geom_line(size=1.2)+geom_point(size=.8)+
    facet_wrap(~paste0(prod, ', ', Unit), scales='free_y') +
    scale_color_gradientn(colors=colorspace::darken(crea_palettes$change), labels=scales::percent) +
    labs(title=names(plots)[i], 
         subtitle='seasonally adjusted monthly data', #'12-month moving sum', 
         x='', y=unique(plotdata$Unit)) +
    theme_crea() + 
    theme(strip.text = element_text(size=rel(.4)),
          axis.text.y = element_text(size=rel(.5))) +
    geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
    scale_linetype_manual(values='dashed', name='') +
    expand_limits(y=0)
  ggsave(paste0(names(plots)[i], 'seasonal.png'), height=8, width=15)
  write_csv(plotdata, paste0(names(plots)[i], '.csv'))
}

ind %>% filter(year(date)>=2015, grepl('Solar Cells', prod)) %>%
  mutate(YoY = (Value1m/lag(Value1m, 12)-1)  %>% pmax(-.5) %>% pmin(.5),
         Value12m=Value12m*12/100) -> solar_plotdata

solar_plotdata %>% 
  ggplot(aes(date, Value12m, col=YoY, label=round(Value12m,0)))+
  geom_line(size=1.2)+geom_point(size=.8)+
  geom_text(data=solar_plotdata %>% filter(date==max(date)), vjust=-.4, fontface='bold')+
  facet_wrap(~prod, scales='free_y', nrow=4) +
  scale_color_crea_c('change', labels=scales::percent) +
  labs(title=names(plots)[i], subtitle='12-month moving sum', x='', y='GW') +
  theme_crea() + 
  scale_linetype_manual(values='dashed', name='')
ggsave(paste0(names(plots)[4], '.png'))

ind %>% filter(year(date)>=2017, grepl('Automob|Vehicle', prod)) %>% mutate(Unit='million') -> plotdata1

plotdata1 %>% ggplot(aes(date, Value12m*12/100, col=prod))+
  geom_line(size=1.2)+geom_point(size=.8)+
  facet_wrap(~prod, scales='free_y',ncol=1) +
  scale_color_crea_d('dramatic', guide=F) +
  labs(title='Vehicle production', subtitle='12-month moving sum', x='', y='million units') +
  theme_crea() + theme(strip.text = element_text(size=rel(.6)), legend.position = 'top') +
  geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='') +
  expand_limits(y=0) -> p1

plotdata1 %>% 
  group_by(date) %>% summarise(Value12m = Value12m[grepl('New Energy', prod)]/Value12m[grepl('Auto', prod)]) %>% 
  mutate(prod='Share of New Energy Vehicles', Unit='percent') -> plotdata2

plotdata2 %>% 
  ggplot(aes(date, Value12m, col=prod))+
  geom_line(size=1.2)+geom_point(size=.8)+
  scale_color_crea_d('dramatic', guide=F, col.index = 3) +
  labs(y='new energy vehicle share', title=' ', subtitle=' ') +
  theme_crea() + 
  geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='', guide=F) +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y=0) -> p2
require(gridExtra)
grid.arrange(p1,p2, nrow=1)
arrangeGrob(p1,p2, nrow=1) -> g
ggsave('Vehicle production.png', g)

bind_rows(plotdata1, plotdata2) %>% 
  select(date, prod, Unit, Value12m) %>% 
  write_csv('vehicle production.csv')

ind %>% filter(date==max(date)) %>% 
  select(prod, YoY) %>% mutate(across(YoY, scales::percent))



  

in_file='new power capacity.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'YTD')) %>% filter(!grepl('Coal|Gas', YTD)) -> cap
readwindEN(in_file, c('var', 'planttype', 'source', 'YTD')) %>% filter(grepl('Coal|Gas', source)) -> cap2

cap %<>% bind_rows(cap2)

cap %<>% select(-type) %>% ddply(.(source), unYTD) %>% ddply(.(source), roll12m) %>% 
  ddply(.(source), roll12m, months=3) %>% ddply(.(source), get.yoy, col='Value3m')


cap %>% filter(source %notin% c("Coal", "Gas"), month(date)==10) %>% 
  mutate(date='yday<-'(date, 1)) %>% 
  ggplot(aes(date, Value/100, fill=source)) + geom_col() + facet_wrap(~source) + 
  theme_crea() + scale_fill_crea_d('dramatic', col.index = c(2,6,1,4,3)) +
  labs(title="Power generating capacity additions", subtitle = "Jan-Oct by year", y='GW', x='')
ggsave('Power generating capacity additions.png')

cap %>% filter(month(date)==9, source %notin% c("Coal", "Gas")) %>% 
  mutate(Value=Value/100, Unit = 'GW') %>% 
  select(date, value=Value, variable=var, source, Unit) %>% 
  write_csv('Power generating capacity additions.csv')




in_file='M1, M2, industry.xlsx'
getwindvars(in_file)

readwindEN(in_file, c('var1', 'var2', 'var3')) %>% mutate(Value=Value/100) -> stimu
stimu %>% distinct(var1, var2, var3) %T>% write_excel_csv('M1, M2, industry, dict.csv') ->
  dict

"M1
M2
Electricity
Pig iron
Cement
Cars
Coal
Steel products
6 plants' coal burn
Crude steel
Electricity demand
Heavy industry electricity demand
Coke
Industrial electricity demand
Total social finance value
Total social finance" %>% textConnection %>% readLines ->
  dict$var

stimu %<>% left_join(dict) %>% ddply(.(var), roll12m, incol='Value')

ind %>% ddply(.(var, prod, type), function(df) df %>% unYTD %>% get.yoy) %>% 
  filter(grepl('Coal|Cement$|Steel|Iron|Non-ferr|Gasoli|Diesel|Boilers', prod)) %>% 
  select(-var, -Value) %>% rename(var=prod, Value=YoY) -> ind2

ind2 %>%
  ddply(.(var), roll12m, incol='Value') %>% 
  filter(grepl('Pig Iron|Cement|Steels|^Steel|Diesel|Non-ferr', var)) %>% 
  mutate(name='variable', var=gsub('.*Kinds of ', '', var)) %>% 
  ggplot(aes(date, Value12m, col=name)) + geom_line() +
  facet_wrap(~var) +
  geom_line(data=stimu %>% filter(grepl('M2|social finance$', var)) %>% rename(name=var)) +
  scale_x_date(limits=ymd(c('2005-01-01', NA))) +
  scale_y_continuous(limits=c(-.1,.3), labels=scales::percent) +
  labs(title='Growth rates of industrial output\nvs finance and money supply',
       subtitle='12-month average', x='', y='year-on-year') +
  theme_crea() +
  scale_color_crea_d('dramatic')
  
readwindEN(in_file, c('var', 'planttype', 'source', 'YTD')) %>% filter(grepl('Coal|Gas', source)) -> cap2

cap %<>% bind_rows(cap2)

cap %<>% select(-type) %>% ddply(.(source), unYTD) %>% ddply(.(source), roll12m) %>% 
  ddply(.(source), roll12m, months=3) %>% ddply(.(source), get.yoy, col='Value3m')


cap %>% filter(source %notin% c("Coal", "Gas"), month(date)==10) %>% 
  mutate(date='yday<-'(date, 1)) %>% 
  ggplot(aes(date, Value/100, fill=source)) + geom_col() + facet_wrap(~source) + 
  theme_crea() + scale_fill_crea_d('dramatic', col.index = c(2,6,1,4,3)) +
  labs(title="Power generating capacity additions", subtitle = "Jan-Oct by year", y='GW', x='')
ggsave('Power generating capacity additions.png')







in_file = "monthly industry stats.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption') -> prod
readwindEN(in_file, c('var', 'sector'), columnFilter = 'Power Consumption.*Industry') -> cons
readwindEN(in_file, c('sector'), columnFilter = 'Residents|Social') -> hhcons

hhcons %>% 
  mutate(sector = ifelse(grepl('Residents', sector), 'Residential', 'Total')) %>% 
  bind_rows(cons) %>% 
  mutate(prod = paste0('Power Demand: ', sector), var='Power Demand') %>% 
  bind_rows(prod) ->
  prod

prod %>% filter(year(date)==2021) %>% use_series(date) %>%  unique
prod %<>% fixdates()

prod %<>% 
  group_by(var, prod, type) %>%
  group_modify(function(df, k) { df %>% unYTD() %>% roll12m %>% seasonal(year_range = 2012:2019) })

prod %>% filter(year(date)>=2018) %>% 
  ggplot(aes(date, Value.seasonadj)) + geom_line() + facet_wrap(~prod, scales='free_y')


prod %>% filter(prod=='Crude Steels') %>% mutate(type='historical') -> steel

steel %<>%
  (function(df) {
    df %>% filter(date=='2020-12-31') %>% mutate(date=ymd('2021-12-31')) -> target
    df %>% tail(1) -> base
    bind_rows(base, target) %>% mutate(type='target')
  }) %>% bind_rows(steel)

steel %>%
  ggplot(aes(date, Value12m/100*12, linetype=type)) +
  geom_line(size=1) +
  geom_hline(aes(yintercept = Value12m/100*12, col='2020 level'),
             data = prod %>% filter(date=='2020-12-31', prod=='Crude Steels'),
             linetype='dashed',
             size=1) +
  geom_vline(aes(xintercept = ymd('2020-02-01'), size='COVID-19 lockdown')) +
  theme_crea() +
  labs(title="China's crude steel output", x='', y='Mt', col='', subtitle='12-month moving sum',
       lty='') +
  scale_size_manual(values=.5, name='')
ggsave('China crude steel output.png')

steel %>% mutate(Value12m=Value12m/100*12) %>% select(var, prod, date, moving_12_sum_Mt=Value12m, type) %>% 
  write_csv('China crude steel output.png')


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
