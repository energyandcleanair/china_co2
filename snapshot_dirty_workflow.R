require(tidyverse)
require(magrittr)
require(rcrea)
require(creahelpers)
library(directlabels)
require(ggrepel)
require(readxl)
require(lubridate)
require(scales)
require(pbapply)
require(countrycode)
require(gsubfn)
require(zoo)
require(grid)
require(gridExtra)
require(cowplot)
require(openxlsx)
require(glue)

list.files(path='R', pattern='\\.R', full.names=T) -> s

lapply(s, source)


focus_month <- today() %>% subtract(30) %>% "day<-"(1)

output_dir <- paste0("outputs/monthly_snapshot_", format(as.Date(focus_month), "%Y_%m"))

dir.create(output_dir, showWarnings = F, recursive = T)

# preload air quality data
aq <- get_aq(start_date = ymd("2022-01-01"), update_data = T)
aq_dw <- get_deweathered_aq(china_admin_capitals, start_date = ymd("2022-01-01"), update_data = T)

for (target_lang in c('EN', 'ZH')) {

  # set lang in global environment
  assign("lang", target_lang, envir = .GlobalEnv)

  capacity_plots(focus_month = focus_month,
                 output_dir=output_dir,
                 lang=target_lang)

  industry_output_plots(focus_month = focus_month,
                        output_dir=output_dir,
                        lang=target_lang)

  steel_indicator_plots(output_dir=output_dir,
                        lang=target_lang)

  fuel_supply_plots(focus_month = focus_month,
                    output_dir=output_dir,
                    lang=target_lang)

  air_quality_plots(focus_month = focus_month, update_data = F, aq = aq, aq_dw = aq_dw,
                    output_dir=output_dir,
                    lang=target_lang)

  plot_aq_compliance(cities = china_admin_capitals,
                     update_data = T,
                     output_dir=output_dir,
                     lang=target_lang)
}

shared_dir=file.path("G:/Shared drives/CREA-China/monthly snapshot",basename(output_dir))

dir.create(shared_dir)

list.files(path=output_dir, full.names=T) %>%
  file.copy(., file.path(shared_dir, basename(.)), overwrite=T)
