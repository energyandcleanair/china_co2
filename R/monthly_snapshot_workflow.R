build_snapshot <- function(focus_month = NULL, output_dir = NULL,
                           lang="EN",
                           update_aq_data=T) {
  # required input data:
  # "data/monthly industry stats.xlsx"
  # "data/Power Capacity.xlsx"
  # "data/New power capacity by province and type.xlsx"
  # "data/fuel supply.xlsx"

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


  # preload air quality data
  aq <- get_aq(start_date = ymd("2022-01-01"), update_data = update_aq_data)
  aq_dw <- get_deweathered_aq(china_admin_capitals, update_data = update_aq_data)

  #TODO add ZH once we have the translation file
  for (lang in c("EN")) {

    # set lang in global environment
    assign("lang", lang, envir = .GlobalEnv)

    capacity_plots(focus_month = focus_month,
                   output_dir=output_dir,
                   lang=lang)

    industry_output_plots(focus_month = focus_month,
                          output_dir=output_dir,
                          lang=lang)

    fuel_supply_plots(focus_month = focus_month,
                      output_dir=output_dir,
                      lang=lang)

    air_quality_plots(focus_month = focus_month,
                      update_data = F, aq = aq, aq_dw = aq_dw,
                      output_dir=output_dir,
                      lang=lang)
  }
}
