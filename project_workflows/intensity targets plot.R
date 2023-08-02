paste.xl() -> intensity_targets

intensity_targets %>% 
  pivot_longer(starts_with('X'), names_to='FYP') %>% 
  mutate(offset = case_when(!grepl('14', FYP) | variable=='Target'~0,
                            grepl('Required', variable)~.125,
                            grepl('Realized', variable)~-.125),
         width = case_when(grepl('14', FYP)~.25, T~.5),
    x = FYP %>% as.factor %>% as.numeric %>% add(offset),
    FYP = FYP %>% gsub('X', '', .) %>% gsub('$|\\.', 'th', .) %>% gsub('alternative.*', ' (alternative coal\nconsumption estimate)', .)) ->
  targets_plot

targets_plot %>% filter(variable == 'Target') %>% 
  group_by(FYP, target) %>% group_modify(function(df, ...) tibble(df %>% select(-x), x=df$x+c(-.5,.5))) ->
  target_lines

targets_plot %>% filter(variable != 'Target') %>% 
  ggplot(aes(x, value)) + facet_wrap(~target, ncol=1) + 
  geom_col(aes(fill=variable, width=width)) +
  geom_line(data=target_lines, aes(group=FYP, linetype='target'), linewidth=1, color=crea_palettes$dramatic[1]) +
  scale_x_continuous(labels=function(x) targets_plot$FYP[x], breaks=1:5) +
  scale_linetype_manual(values='dashed', name='') +
  scale_fill_crea_d(name='', guide=guide_legend(nrow=1)) +
  theme_crea(legend.position='top') +
  scale_y_continuous(labels=scales::percent, expand=expansion(mult=c(0,.05))) +
  labs(x='five-year period', y='improvement, %/year', title="China's progress on energy and carbon intensity targets") -> plt
quicksave(file.path(output_dir, "Chinas progress on energy and carbon intensity targets.png"), plot=plt)
