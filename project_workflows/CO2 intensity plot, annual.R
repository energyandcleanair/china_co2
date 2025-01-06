paste.xl() -> intensity

intensity %>% rename(variable=X) %>%
  pivot_longer(starts_with('X'), names_to='year') %>%
  #mutate(across(c(year, value), force_numeric)) %>%
  mutate(value=force_numeric(value),
         year=gsub('X', '', year)) %>%
  ggplot(aes(year, value/100)) +
  geom_col(color=crea_palettes$CREA['Dark.red'], fill=crea_palettes$CREA['Dark.red']) + facet_wrap(~variable, ncol=1) +
  theme_crea() +
  scale_y_continuous(labels = scales::percent) +
  labs(y='', x='', title="Annual change in China's energy and CO2 intensity") ->
  p

quicksave('outputs/2024Q3/annual intensity.png', scale=.7)
