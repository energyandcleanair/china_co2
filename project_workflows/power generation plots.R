loadlibs()

"CO2data/china" %>% boxpath %>% setwd
source('wind mapping functions.R')
source('wind mapping functions 2.R')

sourcecols=list(Solar=crea_palettes$dramatic[1],
             Wind=crea_palettes$dramatic[2],
             Nuclear=crea_palettes$dramatic[6],
             Hydro=crea_palettes$CREA[2],
             Thermal=crea_palettes$CREA[6]) %>% lapply(unname) %>% unlist
sourcecolsZH=sourcecols
names(sourcecolsZH) %<>% translateSources()

in_file = 'power generation by type.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnFilter = "Thermal|Hydro|Nuclear|Solar|Wind", read_vardata = T) %>% 
  filter(!is.na(Value)) -> pwr

pwr %<>% 
  group_by(var, prod, type) %>%
  group_modify(function(df, k) { df %>% unYTD() %>% 
      filter(Value1m>0) %>% roll12m %>% seasonal(year_range = 2012:2019) })

pwr %>% 
  mutate(Value12m = ifelse(is.na(Value12m) & prod=='Solar', min(Value12m[prod=='Solar'], na.rm=T), Value12m)) %>% 
  filter(grepl('Solar$| Power', prod)) %>% 
  mutate(prod = gsub(' Power.*', '', prod),
         prod = factor(prod, levels=c('Thermal', 'Hydro', 'Nuclear', 'Wind', 'Solar') %>% rev)) ->
  pwr_plot

pwr_plot %>% filter(year(date)>=2017) %>% 
  ggplot(aes(date, Value12m*12/10, fill=prod)) + geom_area() +
  x_at_zero() + 
  scale_x_date(expand=expansion(mult=c(0,0))) +
  scale_fill_manual(values=sourcecols) + 
  theme_crea() +
  labs(title="China's power generation mix", subtitle='12-month sum', x='', y='TWh/year',
       fill='source')
quicksave('Chinas power generation mix.png')

pwr_plot %>% group_by(prod) %>% mutate(change = Value12m-Value12m[date=='2017-06-30']) %>% 
  filter(date>='2017-06-30') %>% 
  ggplot(aes(date, change*12/10, fill=prod)) + geom_area() +
  x_at_zero() + 
  scale_x_date(expand=expansion(mult=c(0,0))) +
  scale_fill_manual(values=sourcecols) + 
  theme_crea() +
  labs(title="Changes in China's power generation mix", 
       subtitle='in the past five years up to June 2022',
       x='', y='TWh/year, 12-month sum',
       fill='source')
ggsave('changes in Chinas power generation mix.png', width=8, height=6)

#ZH
lang='ZH'

pwr_plot %>% filter(year(date)>=2017) %>% 
  mutate(prodZH=factor(translateSources(prod), levels=names(sourcecolsZH))) %>% 
  ggplot(aes(date, Value12m*12/10/1000, fill=prodZH)) + geom_area() +
  x_at_zero() + 
  scale_x_date(expand=expansion(mult=c(0,0)), labels = function(x) paste0(year(x), '年')) +
  scale_fill_manual(values=sourcecolsZH) + 
  theme_crea() +
  labs(title="中国发电总量及机构", subtitle='12个月移动总计', x='', y='万亿千瓦时/年',
       fill='电源类别')
quicksave('Chinas power generation mix ZH.png')

pwr_plot %>% group_by(prod) %>% mutate(change = Value12m-Value12m[date=='2017-06-30']) %>% 
  filter(date>='2017-06-30') %>% 
  mutate(prodZH=factor(translateSources(prod), levels=names(sourcecolsZH))) %>% 
  ggplot(aes(date, change*12/10/1000, fill=prodZH)) + geom_area() +
  x_at_zero() + 
  scale_x_date(expand=expansion(mult=c(0,0)), labels = function(x) paste0(year(x), '年')) +
  scale_fill_manual(values=sourcecolsZH) + 
  theme_crea() +
  labs(title="中国发电机构变化", 
       subtitle='从2017年6月到2022年6月',
       x='', y='万亿千瓦时/年, 12个月移动总计',
       fill='电源类别')
ggsave('changes in Chinas power generation mix ZH.png', width=8, height=6)
