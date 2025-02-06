ember_yearly %>%
  filter(Area=='World',
         Unit=='TWh', Subcategory=='Fuel') %>%
  group_by(Variable=disambiguate(Variable, c(Fossil='Coal|Gas|Fossil')),
           year=Year) %>%
  summarise(across(Value, sum)) %>%
  mutate(growth=Value-Value[year==2014]) ->
  ember_world


ember_monthly %>%
  filter(Area=='United States of America',
         Unit=='TWh', Subcategory=='Fuel') %>%
  mutate(Variable=Variable %>% disambiguate(c(Fossil='Coal|Gas|Fossil')) %>%
           gsub(' ', '\n', .)) %>%
  group_by(Variable, year=year(Date)) %>%
  summarise(across(Value, sum)) %>%
  mutate(growth=Value-Value[year==2017]) ->
  ember_usa

ember_usa %>%
  filter(year>=2014) %>%
  ggplot(aes(year, growth)) + geom_line() + facet_wrap(~Variable)

ember_usa %>%
  filter(year==2020) %>%
  ggplot(aes(Variable, growth, fill=Variable)) + geom_col() +
  labs(title='Changes in U.S. power generation by source',
       subtitle='In the decade from 2014 to 2024',
       y='TWh/year', x='') +
  theme_crea() +
  theme(plot.title=element_text(size=rel(1.3))) +
  scale_fill_crea_d('CREA',
                    col.index = c('Green', 'Dark.gray', 'Blue', 'Orange',
                                  'Turquoise', 'Dark.red', 'Dark.blue'),
                    guide='none')
quicksave('outputs/US power generation growth by source.png', scale=.5)



ember_world %>%
  filter(year==2023) %>%
  ggplot(aes(Variable, growth, fill=Variable)) + geom_col() +
  labs(title='Changes in world power generation by source',
       subtitle='from 2014 to 2023',
       y='TWh', x='') +
  theme_crea() +
  scale_fill_crea_d('CREA', col.index = c('Green', 'Dark.gray', 'Blue', 'Orange', 'Turquoise', 'Dark.red', 'Dark.blue'), guide='none')
