#required input data:
#"data/monthly industry stats.xlsx"
#"data/Power Capacity.xlsx"
#"data/New power capacity by province and type.xlsx"
#"data/fuel supply.xlsx"

source('R/wind mapping functions.R')
source('R/translation_functions.R')

list.files(pattern='\\.R$', path='project_workflows/monthly_snapshot', full.names = T) %>% 
  subset(!grepl('workflow', basename(.))) -> scripts_to_source
for(s in scripts_to_source) source(s)

library(cowplot)
require(gridExtra)
require(grid)
require(directlabels)

gis_dir <- '~/GIS'

focus_month <- today() %>% subtract(30) %>% 'day<-'(1)
#focus_month <- ymd("2023-04-01")
#output_dir = paste0('outputs/monthly_snapshot_', format(focus_month, "%Y-%m")); dir.create(output_dir)
output_dir = paste0('G:/Shared drives/CREA-China/monthly snapshot/monthly_snapshot_', format(focus_month, "%Y-%m")); dir.create(output_dir)

#preload air quality data
aq <- get_aq(start_date=ymd('2022-01-01'), update_data=T)
aq_dw <- get_deweathered_aq(china_admin_capitals, update_data=T)

for(lang in c('EN', 'ZH')) {
  focus_month <- ymd("2023-04-01")
  capacity_plots(focus_month=focus_month)
  focus_month <- ymd("2023-05-01")
  industry_output_plots(focus_month=focus_month)
  fuel_supply_plots(focus_month=focus_month)
  air_quality_plots(focus_month=focus_month, update_data=F, aq=aq, aq_dw=aq_dw)
}
