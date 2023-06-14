#required input data:
#"data/monthly industry stats.xlsx"
#"data/Power Capacity.xlsx"
#"data/New power capacity by province and type.xlsx"
#"data/fuel supply.xlsx"
#'~/../Downloads/deweathered_mee_20230518.csv'

source('R/wind mapping functions.R')
source('R/translation_functions.R')

list.files(pattern='\\.R$', path='project_workflows/monthly_snapshot', full.names = T) -> scripts_to_source
for(s in scripts_to_source) source(s)

library(cowplot)
require(gridExtra)
require(grid)
require(directlabels)

gis_dir <- '~/GIS'

focus_month <- today() %>% subtract(30) %>% 'day<-'(1)
#focus_month <- ymd("2023-04-01")
output_dir = paste0('outputs/monthly_snapshot_', format(focus_month, "%Y-%m")); dir.create(output_dir)

for(lang in c('EN', 'ZH')) {
  focus_month <- ymd("2023-04-01")
  capacity_plots(focus_month=focus_month, lang=lang)
  industry_output_plots(focus_month=focus_month, lang=lang)
  fuel_supply_plots(focus_month=focus_month, lang=lang)
  focus_month <- ymd("2023-05-01")
  air_quality_plots(focus_month=focus_month, lang=lang, update_data=F, deweathered_data_path='~/../Downloads/deweathered_mee_20230518.csv')
}
