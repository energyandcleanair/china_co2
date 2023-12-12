build_snapshot <- function(focus_month = NULL,
                           base_dir = ".",
                           month_subdir = NULL, # Month-specific sub-directory
                           langs=c("EN","ZH"),
                           gis_dir=Sys.getenv('GIS_DIR'),
                           update_aq_data=T) {
  # required input data:
  # "data/monthly industry stats.xlsx"
  # "data/Power Capacity.xlsx"
  # "data/New power capacity by province and type.xlsx"
  # "data/fuel supply.xlsx"
  # "steel plant operating rates.xlsx"

  trans_file = get_data_file('label_translations.xlsx')
  assign("trans_file", trans_file, envir = .GlobalEnv)
  assign("last.bumpup", list("last.points", "bumpup"), envir = .GlobalEnv) #last.bumpup is missing in latest directlabels

  # Packages needed if run manually
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(grid)
  library(glue)
  library(cowplot)
  library(chinatracker)
  library(creahelpers)
  library(zoo)
  library(ggrepel)

  if (is.null(focus_month)) {
    focus_month <- today() %>%
      subtract(30) %>%
      "day<-"(1)
  }

  if (is.null(month_subdir)) {
    month_subdir <- paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m"))
  }

  month_dir <- file.path(base_dir, month_subdir)
  dir.create(month_dir, showWarnings = F, recursive = T)

  # preload air quality data
  aq <- get_aq(start_date = ymd("2019-01-01"), update_data = update_aq_data, source='mee')
  aq_dw <- get_deweathered_aq(china_admin_capitals, update_data = update_aq_data)
  check_aq_data(aq=aq, aq_dw=aq_dw, focus_month=focus_month, cities=china_admin_capitals)

  for (target_lang in langs) {
    # set lang in global environment
    assign("lang", target_lang, envir = .GlobalEnv)

    # Month-specific charts
    capacity_plots(focus_month = focus_month,
                   output_dir=month_dir,
                   lang=target_lang)

    industry_output_plots(focus_month = focus_month,
                          output_dir=month_dir,
                          lang=target_lang)

    fuel_supply_plots(focus_month = focus_month,
                      output_dir=month_dir,
                      lang=target_lang)

    air_quality_plots(focus_month = focus_month,
                      update_data = F,
                      aq = aq,
                      aq_dw = aq_dw,
                      output_dir=month_dir,
                      lang=target_lang)

    # Non month-specific charts
    steel_indicator_plots(output_dir=base_dir,
                          lang=target_lang)

    aq_compliance_plots(cities = china_admin_capitals,
                        update_data = T,
                        output_dir=base_dir,
                        lang=target_lang,
                        aq_cache = NULL)
  }
}
