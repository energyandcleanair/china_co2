d.quarter %>% ungroup %>% filter(include_in_plots | prod=='Total', grepl(variant_to_highlight, name)) %>%
  select(name, prod, date, CO2_12m) -> plotdata

plotdata %>%
  filter(year(date) %in% 2013:2019) %>%
  lm(CO2_12m~prod*date, data=.) ->
  m

d.quarter %>% filter(grepl(variant_to_highlight, name)) %>% ungroup %>% distinct(prod, date) %>% mutate(CO2_12m=predict(m, .)) ->
  trend

prodcols_w_total=c(prodcols, Total=unname(crea_palettes$CREA['Red']))

bind_rows(plotdata %>% mutate(data_type='actual'),
          trend %>% mutate(data_type='pre-COVID trend')) %>%
  filter(date>='2013-01-01') %>%
  mutate(prod=factor(prod, levels=names(prodcols_w_total))) -> plotdata

plotdata %>%
  ggplot(aes(date, CO2_12m*12, col=prod, linetype=data_type)) +
  geom_line(, linewidth=1) +
  scale_x_date(expand=c(0.02,0.02), breaks=ymd(c('2015-12-31','2020-12-31','2023-12-31')), date_labels = '%Y') +
  theme_crea() +
  scale_color_manual(values=prodcols_w_total, name='Product', guide='none') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="12-month moving sum", y='Mt CO2 / year', x='') +
  theme(plot.title = element_text(size=rel(1.8))) +
  facet_wrap(~prod, scales='free_y') +
  x_at_zero(labels=scales::comma) + scale_linetype_discrete(guide='none') -> plt
quicksave(file.path(output_dir, 'CO2 12m sum, by product.png'), plot=plt)

plotdata %>% filter(prod=='Total') %>%
  mutate(prod=factor(prod, levels=names(prodcols_w_total))) %>%
  ggplot(aes(date, CO2_12m*12, col=prod, linetype=data_type)) +
  geom_line(linewidth=1) +
  scale_x_date(expand=c(0.02,0.02)) +
  theme_crea() +
  scale_color_manual(values=prodcols_w_total, name='Product', guide='none') +
  labs(title="China's CO2 emissions from energy and cement",
       subtitle="12-month moving sum", y='Mt CO2 / year', x='') +
  theme(plot.title = element_text(size=rel(1.8))) +
  x_at_zero(labels=scales::comma) + scale_linetype_discrete(guide='none') -> plt
quicksave(file.path(output_dir, 'CO2 12m sum, total, line plot.png'), plot=plt)

plotdata %>% write_csv(file.path(output_dir, 'CO2 12m, line plot.csv'))
