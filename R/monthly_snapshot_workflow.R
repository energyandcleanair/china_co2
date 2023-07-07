build_snapshot <- function(focus_month = NULL, output_dir = NULL) {
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

  # focus_month <- ymd("2023-04-01")
  # output_dir = paste0('outputs/monthly_snapshot_', format(focus_month, "%Y-%m")); dir.create(output_dir)
  if (is.null(output_dir)) {
    output_dir <- paste0("monthly_snapshot_", format(focus_month, "%Y-%m"))
  }
  dir.create(output_dir, showWarnings = F, recursive = T)

  # TODO
  # Upload all files to Google Drive


  # preload air quality data
  aq <- get_aq(start_date = ymd("2022-01-01"), update_data = T)
  aq_dw <- get_deweathered_aq(china_admin_capitals, update_data = T)

  for (lang in c("EN", "ZH")) {
    # focus_month <- ymd("2023-04-01")
    capacity_plots(focus_month = focus_month,
                   output_dir=output_dir,
                   lang="EN")
    # focus_month <- ymd("2023-05-01")
    industry_output_plots(focus_month = focus_month)
    fuel_supply_plots(focus_month = focus_month)
    air_quality_plots(focus_month = focus_month, update_data = F, aq = aq, aq_dw = aq_dw)
  }
}
