source('R/wind mapping functions.R')

lang='EN'

in_file = "data/Volume of Imports.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), read_vardata = T) -> imp

imp %<>% 
  group_by(var, prod) %>% mutate(type='monthly') %>% 
  group_modify(function(df, k) { df %>% unYTD %>% roll12m %>% seasonal(year_range = 2012:2019) })

imp %>% filter(year(date)>=2018) %>% 
  mutate(prod=gsub(" \\(.*", "", prod)) %>% 
  ggplot(aes(date, Value12m/100*12)) + 
  geom_line(size=1) + facet_wrap(~prod, scales='free_y') +
  expand_limits(y=0) + x_at_zero() +
  theme_crea() +
  labs(title="China's fossil fuel imports", x="", subtitle="12-month sum", y="Mt/year")
quicksave("Chinas fossil fuel imports.png")

in_file = "data/monthly industry stats.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption', read_vardata = T) -> prod

in_file = 'data/power generation by type.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnFilter = "Thermal|Hydro|Nuclear|Solar|Wind", read_vardata = T) %>% 
  filter(!is.na(Value)) -> pwr
prod %<>% bind_rows(pwr)
prod$Value[prod$date=='2020-05-31' & grepl('Solar Cells', prod$prod)] <- NA

prod %<>% 
  group_by(var, prod, type) %>% 
  group_modify(function(df, k) { 
    message(k)
    df %>% unYTD() %>% 
      filter(Value1m>0) %>% roll12m %>% seasonal(year_range = 2012:2019) })
  
prod %>% group_by(prod) %>% 
  filter((today() %>% subtract(30) %>% 'day<-'(1) %in% (date %>% 'day<-'(1)))) ->
  prod_withlatest

prod_withlatest %>% filter(year(date)>=2018) %>% 
  ggplot(aes(date, Value.seasonadj)) + geom_line() + facet_wrap(~prod, scales='free_y')


prod_withlatest %<>% ddply(.(prod), roll12m, months=3, incol='Value.seasonadj', outcol='Value3m.seasonadj') %>% 
  ddply(.(prod), get.yoy, col='Value3m.seasonadj')

plots = list('Industrial output'='',
             'Metals&cement output'='Steel Material|Crude Steel|Cement$|Coke|Pig Iron|Non-ferrous|Copper',
             'Heavy industry output'='Steel Material|Crude Steel|Cement$|Chemical|Plastic|Coke|Copper|Metals|Aluminous|Glass|Pig Iron',
             'Housing indicators'='Household|TV|Escalator',
             'Transport fuel production'='Diese|Gasoline|Kerosene',
             'Solar cell output'='Solar Cells',
             'Power generation'='Solar$|Power$|Generating|Nuclear',
             'Power demand'='Power Demand',
             'Coal mine output'='Raw Coal')

for(i in 1:length(plots)) {
  prod_withlatest %>% filter(year(date)>=2017, grepl(plots[[i]], prod),
                  !is.na(Value1m)) %>% 
    mutate(Unit_multiplier=case_when(Unit=='10000 kw'~1/100,
                                     T~1),
           Unit=case_when(Unit=='10000 kw'~'GW',
                          T~Unit)) -> plotdata
  
  labscale=1.15
  if(length(unique(plotdata$prod))>3) labscale=.5
  
  plotdata %>%
    mutate(YoY = (Value3m.seasonadj/lag(Value3m.seasonadj, 12)-1)  %>% pmax(-.2) %>% pmin(.2),
           prod = case_when(prod=='Generating Capacity'~'Electricity', 
                            prod=='Solar'~'Solar power',
                            T~gsub(' Generating Capacity', '', prod))) %>%
    ggplot(aes(date, Value.seasonadj*Unit_multiplier, col=YoY))+
    geom_line(size=.8)+geom_point(size=.8)+
    facet_wrap(~paste0(prod, ', ', Unit), scales='free_y') +
    scale_color_gradientn(colors=colorspace::darken(crea_palettes$change), labels=scales::percent) +
    labs(title=names(plots)[i], 
         subtitle='seasonally adjusted monthly data', #'12-month moving sum', 
         x='', y=unique(plotdata$Unit)) +
    theme_crea() + 
    theme(strip.text = element_text(size=rel(labscale*.8)),
          axis.text.y = element_text(size=rel(labscale))) +
    geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
    scale_linetype_manual(values='dashed', name='') +
    expand_limits(y=0) +
    x_at_zero()
  ggsave(paste0('plots/', names(plots)[i], 'seasonal.png'), height=6, width=8)
  write_csv(plotdata, paste0(names(plots)[i], '.csv'))
}

prod_withlatest %>% filter(year(date)>=2015, grepl('Solar Cells', prod)) %>%
  mutate(YoY = (Value1m/lag(Value1m, 12)-1)  %>% pmax(-.5) %>% pmin(.5),
         Value12m=Value12m*12/100) -> solar_plotdata

solar_plotdata %>% 
  ggplot(aes(date, Value12m, col=YoY, label=round(Value12m,0)))+
  geom_line(size=1.2)+geom_point(size=.8)+
  geom_text(data=solar_plotdata %>% filter(date==max(date)), vjust=-.4, fontface='bold')+
  scale_color_crea_c('change', labels=scales::percent) +
  labs(title=ifelse(lang=='EN', 'Solar cell output', '光伏电池产量'), 
       subtitle=ifelse(lang=='EN', '12-month moving sum', '12个月总量'), 
       x='', y='GW',
       col=ifelse(lang=='EN', 'year-on-year', '同比增长')) +
  theme_crea() + 
  scale_linetype_manual(values='dashed', name='') + 
  expand_limits(y=0) + scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  scale_x_date(labels = yearlab) +
  x_at_zero()
ggsave(paste0('solar cell output, ',lang,'.png'), width=8, height=6)

prod_withlatest %>% filter(year(date)>=2017, grepl('Automob|Vehicle', prod)) %>% mutate(Unit='million') -> 
  plotdata1

plotdata1 %>% ggplot(aes(date, Value12m*12/100, col=prod))+
  geom_line(size=1.2)+geom_point(size=.8)+
  facet_wrap(~prod, scales='free_y',ncol=1) +
  scale_color_crea_d('dramatic', guide=F) +
  labs(title='Vehicle production', subtitle='', x='', y='million units, 12-month moving sum') +
  theme_crea() + theme(strip.text = element_text(size=rel(.6)), legend.position = 'top') +
  geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='') +
  expand_limits(y=0) + x_at_zero() -> p1

plotdata1 %>% 
  ddply(.(prod), roll12m, months=3, outcol='Value3m') %>% 
  group_by(date) %>% summarise(Value3m = Value3m[grepl('New Energy', prod)]/Value3m[grepl('Auto', prod)]) %>% 
  mutate(prod='Share of New Energy Vehicles', Unit='percent') -> plotdata2

plotdata2 %>% 
  ggplot(aes(date, Value3m, col=prod))+
  geom_line(size=1.2)+geom_point(size=.8)+
  scale_color_crea_d('dramatic', guide=F, col.index = 3) +
  labs(y='new energy vehicle share, 3-month moving sum', title=' ', subtitle=' ') +
  theme_crea() + 
  geom_vline(aes(linetype='COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='', guide=F) +
  scale_y_continuous(labels = scales::percent, breaks=function(x) seq(0,x[2],.05),
                     expand=expansion(mult=c(0,.05))) +
  expand_limits(y=0) -> p2

require(gridExtra)
grid.arrange(p1,p2, nrow=1)
arrangeGrob(p1,p2, nrow=1) -> g
quicksave('Vehicle production.png', plot=g)

bind_rows(plotdata1, plotdata2) %>% 
  select(date, prod, Unit, Value12m) %>% 
  write_csv('vehicle production.csv')

prod %>% filter(grepl('Nuclear|Hydro|Solar$|Wind|Thermal|Generating', prod), year(date)>=2015) %>% 
  mutate(plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1),
         year=as.factor(year(date)),
         prod=prod %>% gsub('^Gener.*', 'Total', .) %>% gsub('Gener.*', '', .)) ->
  hydroplot

hydroplot %>% 
  ggplot(aes(plotdate, Value1m/10, alpha=year, col=prod)) + 
  facet_wrap(~prod, scales='free_y') +
  geom_line(size=1) +
  expand_limits(y=0) + x_at_zero() +
  theme_crea() + 
  scale_alpha_discrete(guide='none') +
  ggrepel::geom_text_repel(aes(label=year), data=hydroplot %>% filter(month(date)==10),
                           min.segment.length = .25, force=.5) +
  scale_x_date(date_labels = '%b', expand=expansion(mult=.01)) +
  labs(title='Monthly power generation in China',
       y='TWh', x='') + x_at_zero() +
  scale_color_crea_d('dramatic',  guide='none', col.index = c(2,6,1,4,3,5))
quicksave('Monthly hydropower generation.png')


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
