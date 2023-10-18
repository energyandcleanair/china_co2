build_snapshot <- function(focus_month = NULL, output_dir = NULL,
                           lang=c("EN","ZH"),
                           gis_dir=Sys.getenv('GIS_DIR'),
                           update_aq_data=T) {
  # required input data:
  # "data/monthly industry stats.xlsx"
  # "data/Power Capacity.xlsx"
  # "data/New power capacity by province and type.xlsx"
  # "data/fuel supply.xlsx"
  # "steel plant operating rates.xlsx"

  trans_file = get_data_file('label_translations.xlsx')

  # Packages needed if run manually
  # library(tidyverse)
  # library(magrittr)
  # library(lubridate)
  # library(grid)
  # library(glue)
  # library(cowplot)
  # library(chinatracker)
  # library(creahelpers)

  if (is.null(focus_month)) {
    focus_month <- today() %>%
      subtract(30) %>%
      "day<-"(1)
  }

  if (is.null(output_dir)) {
    output_dir <- paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m"))
  }
  dir.create(output_dir, showWarnings = F, recursive = T)

  # TODO
  # Upload all files to Google Drive

  # extrafont::font_import()

  # preload air quality data
  aq <- get_aq(start_date = ymd("2022-01-01"), update_data = update_aq_data, source='mee')
  aq_dw <- get_deweathered_aq(china_admin_capitals, update_data = update_aq_data)
  check_aq_data(aq=aq, aq_dw=aq_dw, focus_month=focus_month, cities=china_admin_capitals)

  for (target_lang in lang) {
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

    aq_compliance_plots(cities = china_admin_capitals,
                        update_data = T,
                        output_dir=output_dir,
                        lang=target_lang)
  }
}
