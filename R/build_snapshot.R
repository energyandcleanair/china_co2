build_snapshot <- function(focus_month = NULL,
                           output_dir = "output",
                           month_subdir = NULL, # Month-specific sub-directory
                           langs = c("EN", "ZH"),
                           gis_dir = Sys.getenv("GIS_DIR"),
                           update_aq_data = T,
                           snapshot_precheck = F,
                           plots = c(
                             "steel_indicator",
                             "aq_compliance",
                             "capacity",
                             "power_generation",
                             "industry_output",
                             "fuel_supply",
                             "air_quality"
                           )) {
  # required input data:
  # monthly industry stats with YoY.xlsx
  # Power Capacity.xlsx
  # New power capacity by province and type.xlsx
  # fuel supply.xlsx
  # steel plant operating rates.xlsx


  if (is.null(focus_month)) {
    focus_month <- today() %>%
      subtract(30) %>%
      "day<-"(1)
  }

  if (is.null(month_subdir)) {
    month_subdir <- paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m"))
  }

  month_dir <- file.path(output_dir, month_subdir)
  dir.create(month_dir, showWarnings = F, recursive = T)

  check_wind_update(output_dir = month_dir, stop_if_fail = snapshot_precheck)

  data_summary <<- list()

  trans_file <- get_data_file("label_translations.xlsx")
  assign("trans_file", trans_file, envir = .GlobalEnv)
  assign("last.bumpup", list("last.points", "bumpup"), envir = .GlobalEnv) # last.bumpup is missing in latest directlabels

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


  for (target_lang in langs) {
    # set lang in global environment
    assign("lang", target_lang, envir = .GlobalEnv)

    # Non month-specific charts
    if ("steel_indicator" %in% plots) {
      steel_indicator_plots(
        start_year = year(today()) - 5, # first year shown in plots
        output_dir = month_dir,
        lang = target_lang
      )
    }

    if ("capacity" %in% plots) {
      capacity_plots(
        focus_month = focus_month,
        output_dir = month_dir,
        lang = target_lang
      )
    }


    if ("power_generation" %in% plots) {
      power_generation_plots(
        focus_month = focus_month,
        output_dir = month_dir,
        lang = target_lang
      )
    }

    if ("industry_output" %in% plots) {
      industry_output_plots(
        plots = NULL, # list of plots to make, NULL to use default defined within the function
        include_yoy_labels = F, # include labels with year-on-year growth in plots?
        skip_yoy_adjustment = "Copper|Glass|Chemical Fibers|Solar$", # these products aren't retroactively adjusted to fit reported yoy numbers because there are anomalies
        focus_month = focus_month,
        output_dir = month_dir,
        lang = target_lang
      )
    }

    if ("fuel_supply" %in% plots) {
      fuel_supply_plots(
        focus_month = focus_month,
        output_dir = month_dir,
        lang = target_lang
      )
    }


    if(any(c("air_quality", "aq_compliance") %in% plots)) {
      # preload air quality data
      aq <- get_aq(
        start_date = ymd("2019-01-01"),
        update_data = update_aq_data,
        source = "mee"
      )

      aq_dw <- get_deweathered_aq(china_admin_capitals, update_data = update_aq_data)

      check_aq_data(
        aq = aq, aq_dw = aq_dw, focus_month = focus_month,
        cities = china_admin_capitals,
        stop_if_fail = snapshot_precheck
      )
    }

    if ("air_quality" %in% plots) {
      air_quality_plots(
        focus_month = focus_month,
        update_data = F,
        aq = aq,
        aq_dw = aq_dw,
        output_dir = month_dir,
        lang = target_lang
      )
    }

    if ("aq_compliance" %in% plots) {
      aq_compliance_plots(
        cities = china_admin_capitals,
        one_month_plots = T,
        update_data = update_aq_data,
        output_dir = output_dir,
        lang = target_lang,
        focus_month = focus_month
      )
    }
  }

  write.csv(data_summary, file.path(month_dir, "data_summary.csv"), row.names = F)
}
