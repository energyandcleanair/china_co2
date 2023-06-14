source('R/wind mapping functions.R')
library(cowplot)
require(gridExtra)
require(grid)

focus_month <- today() %>% subtract(30) %>% 'day<-'(1)
#focus_month <- ymd("2023-04-01")
output_dir = paste0('outputs/monthly_snapshot_', format(focus_month, "%Y-%m")); dir.create(output_dir)

for(lang in c('EN', 'ZH')) {
  source('project_workflows/monthly_snapshot/capacity_plots.R')
  source('project_workflows/monthly_snapshot/industry_output_plot.R')
  source('project_workflows/monthly_snapshot/fuel_supply_plot.R')
  source('project_workflows/monthly_snapshot/air_quality_plot.R')
}