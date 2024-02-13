build_snapshot <- function(focus_month = NULL,
                           base_dir = ".",
                           month_subdir = NULL, # Month-specific sub-directory
                           langs=c("EN","ZH"),
                           gis_dir=Sys.getenv('GIS_DIR'),
                           update_aq_data=T) {
  # required input data:
  # monthly industry stats with YoY.xlsx
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

  china_admin_capitals <- c(
    "harbin_chn.11_1_cn",
    "changsha_chn.14_1_cn",
    "taiyuan_chn.25_1_cn",
    "urumqi_chn.28_1_cn",
    "shenyang_chn.18_1_cn",
    "hefei_chn.1_1_cn",
    "hangzhou_chn.31_1_cn",
    "shanghai_chn.24_1_cn",
    "nanjing_chn.15_1_cn",
    "nanchang_chn.16_1_cn",
    "guangzhou_chn.6_1_cn",
    "lhasa_chn.29_1_cn",
    "xining_chn.21_1_cn",
    "lanzhou_chn.5_1_cn",
    "chengdu_chn.26_1_cn",
    "nanning_chn.7_1_cn",
    "changchun_chn.17_1_cn",
    "yinchuan_chn.20_1_cn",
    "zhengzhou_chn.12_1_cn",
    "guiyang_chn.8_1_cn",
    "fuzhou_chn.4_1_cn",
    "wuhan_chn.13_1_cn",
    "shijiazhuang_chn.10_1_cn",
    "tianjin_chn.27_1_cn",
    "beijing_chn.2_1_cn",
    "xi'an_chn.22_1_cn",
    "jinan_chn.23_1_cn",
    "kunming_chn.30_1_cn",
    "hohhot_chn.19_1_cn",
    "chongqing_chn.3_1_cn",
    "haikou_chn.9_1_cn",
    "hainan_chn.21_1_cn"
  )

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

    industry_output_plots(plots=NULL, #list of plots to make, NULL to use default defined within the function
                          include_yoy_labels=F, #include labels with year-on-year growth in plots?
                          skip_yoy_adjustment = 'Copper|Glass|Chemical Fibers|Solar$', #these products aren't retroactively adjusted to fit reported yoy numbers because there are anomalies
                          focus_month = focus_month,
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
    steel_indicator_plots(start_year=year(today())-6, #first year shown in plots
                          output_dir=base_dir,
                          lang=target_lang)

    aq_compliance_plots(cities = china_admin_capitals,
                        one_month_plots=T,
                        update_data = T,
                        output_dir=base_dir,
                        lang=target_lang,
                        aq_cache = NULL)
  }
}
