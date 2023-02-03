loadlibs()

"CO2data/china" %>% boxpath %>% setwd
source('wind mapping functions.R')
source('wind mapping functions 2.R')

lang='EN'

in_file = "Fuels and metals - imports and exports.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), read_vardata = T, columnExclude = 'Value') -> imp

imp %<>% 
  group_by(var, prod) %>% mutate(type='monthly') %>% 
  group_modify(function(df, k) { df %>% unYTD %>% roll12m %>% seasonal(year_range = 2012:2019) })

imp %>% filter(year(date)>=2018) %>% 
  mutate(prod=gsub(" \\(.*", "", prod),
         varName=ifelse(grepl('Imports', var), 'Imports', 'Exports')) %>% 
  ggplot(aes(date, Value12m/100*12)) + 
  geom_line(size=1) + facet_wrap(~prod+varName, scales='free_y') +
  expand_limits(y=0) + x_at_zero() +
  theme_crea() +
  labs(title="China's fossil fuel imports", x="", subtitle="12-month sum", y="Mt/year")
quicksave("Chinas fossil fuel imports.png")

imp %<>% ddply(.(var, prod), roll12m, months=3, incol='Value.seasonadj', outcol='Value3m.seasonadj') %>% 
  ddply(.(var, prod), get.yoy, col='Value3m.seasonadj')

labscale=.5

imp %>% filter(year(date)>=2018) %>% 
  mutate(YoY = (Value3m.seasonadj/lag(Value3m.seasonadj, 12)-1)  %>% pmax(-.2) %>% pmin(.2),
         prod=gsub(" \\(.*", "", prod),
         var=ifelse(grepl('Imports', var), 'Imports', 'Exports'), 
         Unit_multiplier=1) %>%
  ggplot(aes(date, Value.seasonadj*Unit_multiplier, col=YoY))+
  geom_line(size=.8)+geom_point(size=.8)+
  facet_wrap(~paste0(prod, ' ', var, ', ', Unit), scales='free_y', ncol=3) +
  scale_color_gradientn(colors=colorspace::darken(crea_palettes$change), labels=scales::percent) +
  labs(title="China's monthly imports and exports", 
       subtitle='seasonally adjusted monthly data', #'12-month moving sum', 
       x='', y='') +
  theme_crea() + 
  theme(strip.text = element_text(size=rel(labscale*.8)),
        axis.text.y = element_text(size=rel(labscale))) +
  geom_vline(aes(linetype='First COVID-19 lockdown', xintercept=ymd('2020-02-01')), size=.5, alpha=.7) +
  scale_linetype_manual(values='dashed', name='', 
                        guide=guide_legend(override.aes = list(alpha=1))) +
  expand_limits(y=0) +
  x_at_zero()
quicksave('China imports exports.png')

