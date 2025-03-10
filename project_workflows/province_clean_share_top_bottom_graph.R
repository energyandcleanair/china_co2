changes_total %>%
  group_by(region=add_region(prov, 'East-West')) %>%
  mutate(top_bottom=case_when(share>=sort(share, decreasing=T)[3]~'top 3',
                              share<=sort(share, decreasing=F)[3]~'bottom 3') %>%
           factor(levels=c('top 3', 'bottom 3'))) %>%
  select(prov, region, top_bottom, clean_share_change=share) %>%
  left_join(changes) %>%
  ungroup %>%
  arrange(share) %>%
  mutate(prov=factor(prov, levels=changes_total %>% arrange(share) %>% use_series(prov))) %>%
  filter(source!='Fossil', !is.na(top_bottom)) ->
  changes_reg

changes_reg %>% select(prov, region, top_bottom, source, total_clean_share_change=clean_share_change,
                       share_change=share) %>%
  write_csv(file.path(output_dir, paste0('Changes in power generation mix, top-bottom, ',reg,'.csv')))

changes_reg %>% group_by(prov) %>%
  summarise(min=sum(share[share<0]), max=sum(share[share>0])) %>%
  summarise(min=min(min), max=max(max)) ->
  plot_lims

p <- list()
for(reg in unique(changes_reg$region)) {
  changes_reg %>% filter(region==reg) %>%
  ggplot(aes(prov)) +
    facet_wrap(~top_bottom, ncol=1, scales='free_y') +
    geom_col(aes(y=share, fill=source)) +
    geom_point(aes(y=clean_share_change, shape='Total'), size=2, col=crea_palettes$CREA['Red']) +
    coord_flip() +
    labs(title=reg, x='', y='') +
    #scale_x_discrete(limits=changes_reg %>% filter(region==reg) %>% distinct(prov, clean_share_change) %>%
    #                   arrange(clean_share_change) %>% use_series(prov)) +
    scale_shape_manual(values=16, name='') +
    ggthemes::theme_tufte() +
    theme(plot.title=element_text(hjust=.5), legend.position = 'top') +
    scale_fill_manual(values=fuel_cols, name='') +
    scale_y_continuous(limits = c(plot_lims$min, plot_lims$max), labels=scales::percent) -> p[[reg]]
}

library(gridExtra)
library(cowplot)

legend <- get_legend(p[[1]])


combined_plots <- plot_grid(
  p[['West']] + theme(legend.position = "none"),
  p[['Central']] + theme(legend.position = "none"),
  p[['East']] + theme(legend.position = "none"),
  ncol = 3
)


# Add the x-axis label
x_label <- ggdraw() + draw_label("percentage-points", hjust = 0.5, vjust=0, fontfamily = 'serif')

# Create a title and subtitle
title <- ggdraw() +
  draw_label('Changes in the share of clean power generation', fontface = 'bold',
             fontfamily = 'serif', x = 0.05, hjust = 0, size=16) +
  draw_label('From 2020 to 2024', fontfamily = 'serif', x = 0.05, y = -.025, hjust = 0, size=12)


# Combine everything into the final plot
final_plot <- plot_grid(
  title,
  legend,
  combined_plots,
  x_label,
  ncol = 1,
  rel_heights = c(0.15, 0.05, 1, 0.05)
)

quicksave(file.path(output_dir, paste0('Changes in power generation mix, top-bottom, ',reg,'.png')),
          plot=final_plot, logo=F, scale=.8)

