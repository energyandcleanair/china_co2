source('load_package.R')

paste.xl() %>% filter(Type=='Realized') %>% select(-Type) -> hist
paste.xl() -> targ

saveRDS(list(hist=hist, targ=targ), 'data/intensity_targets_plot_data.RDS')

readRDS('data/intensity_targets_plot_data.RDS') %>% attach()

hist %>% pivot_longer(starts_with('X')) %>%
  separate(name, c('year', 'type')) %>%
  mutate(across(year, force_numeric),
         type=ifelse(is.na(type), 'reported', 'corrected'),
         across(value, ~force_numeric(.x)/100)) ->
  hist_long

targ %>% pivot_longer(-year, names_to='Variable', values_to = 'target') %>%
  mutate(Variable=paste(Variable, 'intensity'),
         across(target, ~force_numeric(.x)/100),
         base_year=year-5, period=paste0(base_year, '-', year),
         target_yearly=1-(1-target)^(1/5)) %>%
  filter(!is.na(target)) ->
  targ_long

hist_long %<>% group_by(year) %>% filter(type=='corrected' | ('corrected' %notin% type)) %>%
  mutate(type='corrected') %>% bind_rows(hist_long %>% filter(type=='reported'))

hist_long %<>% ungroup %>%
  complete(year=2005:max(hist_long$year), Variable, type, fill=list(value=0)) %>%
  group_by(Variable, type) %>% arrange(year) %>%
  mutate(value_cumulative=cumprod(1-value))

targ_long %>%
  inner_join(hist_long %>% rename(base_year=year, base_value=value_cumulative) %>% filter(type=='reported')) %>%
  mutate(value_cumulative=base_value*(1-target)) ->
  targ_targetyear

targ_long %>% mutate(year=base_year) %>%
  inner_join(hist_long %>% filter(type=='reported')) ->
  targ_baseyear

bind_rows(targ_baseyear, targ_targetyear) %>% mutate(type='five-year plan target') -> targ_plot

tibble(year=2005:2025, type='Paris target', period='2005-2030', Variable='CO2 intensity') %>%
  mutate(value_cumulative=(1-.65)^((year-2005)/25),
         target_yearly=1-(1-.65)^(1/25)) -> targ_paris

hist_long %>%  bind_rows(targ_plot, targ_paris) %>%
  ggplot(aes(year, value_cumulative, col=type, groups=period)) + geom_line() +
  facet_wrap(~Variable)

hist_long %>% filter(year>2005) %>%
  group_by(Variable, type, period=ceiling(year/5)) %>%
  summarise(across(value, mean),
            min_year=min(year), max_year=max(year)) %>%
  mutate(period=paste0(min_year, '-', max_year)) %>%
  pivot_longer(contains('year'), values_to = 'year') ->
  hist_avg

hist_long %>% filter(year>2005) %>%
  group_by(Variable, year) %>%
  mutate(value=ifelse(type=='reported', value, value-value[type=='reported'])) %>%
  ggplot(aes(year, value)) + geom_col(aes(fill=type), alpha=.6) +
  geom_line(aes(x=year + ifelse(year==base_year, .6, .4),
                y=target_yearly, group=period,
                linetype='target'), data=targ_plot, linewidth=2) +
  geom_line(aes(x=year + ifelse(name=='min_year', -.4, .4),
                group=paste(period, type), col=type, linetype='realized average'), data=hist_avg, linewidth=1) +
  facet_wrap(~Variable, ncol=1) +
  theme_crea() +
  scale_color_crea_d('dramatic', col.index = c(6,3)) + scale_fill_crea_d('dramatic', col.index = c(6,3)) +
  scale_linetype_manual(values=c('dashed', 'solid'), guide=guide_legend(override.aes = list(linewidth=.7))) +
  scale_x_continuous(expand=expansion(mult=c(0.02,0))) +
  x_at_zero(labels=scales::percent) +
  labs(title = "China's progress towards five-year plan intensity targets", x='', y='',
       subtitle='annual reductions in CO2 and energy intensity',
       linetype='5-year plan period average',
       color='data', fill='data') -> p
quicksave(file.path('outputs', 'Chinas progress towards five-year plan targets.png'), plot=p, scale=1.2)


bind_rows(hist_long %>% mutate(data_type=type, type='historical', period='yearly') %>% filter(year>2005),
          hist_avg %>% mutate(data_type=type, type='historical'),
          targ_plot %>% mutate(year=year+ifelse(year==base_year, 1, 0), value=target_yearly)) %>%
  select(Variable, year, period, type, data_type, value) %>%
  write_csv(file.path('outputs', 'Chinas progress towards five-year plan targets.csv'))



hist_avg$period
hist_avg %>% filter(period=='2021-2023') %>% distinct(Variable, type, value)
targ_plot %>% filter(period=='2020-2025') %>% distinct(Variable, type, target_yearly)
