source('load_package.R')

read_csv('https://ember-climate.org/app/uploads/2022/07/yearly_full_release_long_format.csv') -> emb

emb %>% filter(Area=='World', Variable=='Renewables', Category=='Capacity') %>% print(n=Inf)
emb %>% filter(Area=='World', Category=='Capacity', Year %in% c(2009,2022),
               grepl('Hydro$|^Bio|^Solar|Wind$|^Other Renewables', Variable)) %>%
  group_by(Variable) %>% summarise(change_GW=Value[2]-Value[1]) %>%
  mutate(share_of_tripling=scales::percent(change_GW/sum(change_GW)))
emb$Variable %>% unique

emb %>% filter(Area=='China', Variable=='Renewables', Category=='Capacity') %>% print(n=Inf)


emb %>% filter(Area %in% c('China', 'World'), Variable=='Renewables', Category=='Capacity', Year %in% c(2013,2022)) %>%
  group_by(Area) %>% summarise(change_GW=Value[2]-Value[1]) %>% mutate(share=scales::percent(change_GW/change_GW[Area=='World']))


emb %>% filter(Area %in% c('China', 'World', 'EU'),
               Variable=='Renewables' & Category %in% c('Capacity', 'Electricity generation') |
                 Variable=='Demand', Year %in% c(2022))

emb %>% filter(Area %in% c('China', 'World'), Variable=='Demand', Year %in% c(2022))



emb %>% filter(Area %in% c('World'),
               Variable %in% c('Renewables', 'Solar', 'Wind', 'Hydro', 'Bioenergy') & Category == 'Capacity') %>%
  mutate(data='historical') %>%
  (function(df) {
    bind_rows(df %>% filter(Year==2022),
              df %>% filter(Year==2022)%>% mutate(Year=2030, Value=c(Value[Variable=='Renewables']*3, 296, 1765, 6101, 2742))) %>%
      mutate(data='target') %>%
      bind_rows(df,
                df %>% filter(Year==2022, Variable %in% c('Renewables', 'Solar'))%>% mutate(Year=2023, Value=c(Value[Variable=='Renewables']+500, Value[Variable=='Solar']+413)),
                .)
  }) %>% mutate(Variable=ifelse(Variable=='Renewables', 'Renewables Total', Variable)) -> emb_target

emb_target %>%
  ggplot(aes(Year, Value, linetype=data, col=Variable)) + geom_line(linewidth=2) + facet_wrap(~Variable) +
  theme_crea() +
  labs(x='', y='total installed capacity, GW', title='Global renewable power capacity growth',
       subtitle='Historical growth compared to the target of tripling capacity from 2022 to 2030',
       caption='Historical data from Ember; 2023 capacity additions from IEA&BNEF; 2030 capacity mix from IEA WEO 2023 (NZE scenario)') +
  x_at_zero(labels=scales::comma) + scale_color_crea_d(guide='none', col.index = c(5,1,11,9,2)) -> p
quicksave('~/Historical growth vs target.png', plot=p)


emb_target %>% filter(data=='target') %>% group_by(Variable) %>%
  mutate(Value=(Value[2]-Value[1])/(Year[2]-Year[1]),
         Year=c(2023, 2030)) ->
  annual_adds

emb_target %>% filter(data=='historical') %>% group_by(Variable) %>%
  mutate(Value=(Value-lag(Value)/(Year-lag(Year)))) %>%
  ggplot(aes(Year, Value, fill=Variable, col=Variable)) + geom_col(linewidth=1, color=NA) +
  facet_wrap(~Variable) +
  geom_line(data=annual_adds, aes(linetype='target for 2023-2030'), linewidth=1.5) +
  theme_crea(legend.position='top') +
  labs(x='', y='Annual additions, GW', title='Global renewable power capacity growth',
       subtitle='Annual capacity additions compared to the target of tripling capacity from 2022 to 2030',
       caption='Historical data from Ember; 2023 capacity additions from IEA&BNEF; 2030 capacity mix from IEA WEO 2023 (NZE scenario)',
       linetype='') +
  x_at_zero(labels=scales::comma) +
  scale_fill_crea_d(guide='none', col.index = c(5,1,11,9,2)) +
  scale_color_crea_d(guide='none', col.index = c(5,1,11,9,2)) +
  scale_linetype_manual(values='dotted') -> p
quicksave('~/annual additions vs target.png', plot=p)



