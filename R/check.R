check_aq_data <- function(aq, aq_dw, cities, focus_month = focus_month,
                          n_polls=3, stop_if_fail = FALSE,
                          allowed_missing = list(
                            no2 = 3,
                            pm25 = 3,
                            pm10 = 3,
                            so2 = 3,
                            o3 = 6
                          )){


  # Allowed missing tible (two columns pollutant min)
  allowed_missing_tbl <- tibble(
    pollutant = names(allowed_missing),
    allowed_missing = unlist(allowed_missing))

  # Check that for each location_id and pollutant, there is data for the whole month
  aq_count <- aq %>%
    filter(floor_date(date, unit="month") == focus_month) %>%
    group_by(location_id, pollutant) %>%
    summarise(n = n()) %>%
    left_join(allowed_missing_tbl, by = 'pollutant') %>%
    mutate(min_n =  unname(lubridate::days_in_month(focus_month)) - allowed_missing)

  if(any(aq_count$n < aq_count$min_n)){
    if(stop_if_fail){
      stop(glue(paste("Missing air quality data {min(aq_count$n)}",
                      "/ {days_in_month(focus_month)}")))
    } else {
      warning(glue(paste("Missing air quality data {min(aq_count$n)}",
                         "/ {days_in_month(focus_month)}")))
    }
  }

  if(any(aq$source != 'mee')){
    stop("Wrong AQ source")
  }

  aq_dw_count <- aq_dw %>%
    filter(floor_date(date, unit="month") == focus_month) %>%
    group_by(location_id, pollutant) %>%
    summarise(n = n()) %>%
    left_join(allowed_missing_tbl, by = 'pollutant') %>%
    mutate(min_n =  unname(lubridate::days_in_month(focus_month)) - allowed_missing)

  if(any(aq_dw_count$n < aq_dw_count$min_n)
     || (nrow(aq_dw_count) != length(cities) * n_polls)){
    if(stop_if_fail){
      stop(glue(paste("Missing deweathered air quality data {min(aq_dw_count$n)}",
                      "/ {days_in_month(focus_month)}")))
    } else {
      warning(glue(paste("Missing deweathered air quality data {min(aq_dw_count$n)}",
                         "/ {days_in_month(focus_month)}")))
    }
  }
}

check_wind_update <- function(output_dir, stop_if_fail = F){
  data_summary <- list()

  # industry plot data
  in_file <- get_data_file("monthly industry stats with YoY.xlsx")
  prod <- readwindEN(in_file, c('var', 'prod'),
                     read_vardata = T, zero_as_NA = T, skip = 3)

  in_file_compare <- get_data_file('monthly industry stats.xlsx')
  prod_compare <- readwindEN(in_file_compare, c('var', 'prod'),
                             read_vardata = T, zero_as_NA = T, skip = 1)

  data_summary <- data_summary %>%
    bind_rows(check_dates(data = prod,
                          file_name = "monthly industry stats with YoY.xlsx",
                          check_dates_stop = stop_if_fail))
  data_summary <- data_summary %>%
    bind_rows(check_dates(data = prod_compare,
                          file_name = "monthly industry stats.xlsx",
                          check_dates_stop = stop_if_fail))

  # steel plot data
  in_file <- get_data_file("steel plant operating rates.xlsx")

  steel <- readwindEN(in_file, paste0('V', 1:3),
                      read_vardata = T, zero_as_NA = T, force_last_of_month = F)

  data_summary <- data_summary %>%
    bind_rows(check_dates(data = steel,
                          file_name = "steel plant operating rates.xlsx",
                          check_dates_stop = stop_if_fail))

  # capacity plot data
  in_file <- get_data_file("Power Capacity.xlsx")
  cap <- readwindEN(in_file, c('var', 'source', 'fuel', 'YTD'),
                    read_vardata = T) %>%
    mutate(var = ifelse(grepl('New', var), 'Newly added capacity', 'Installed capacity'),
           source = source %>% gsub(' ?Power.*', '', ., ignore.case = T) %>%
             gsub('YTD', 'All', .),
           fuel = ifelse(is.na(fuel) | fuel %in% c('YTD', 'National'), 'All', fuel))

  data_summary <- data_summary %>%
    bind_rows(check_dates(data = cap,
                          file_name = "Power Capacity.xlsx",
                          check_dates_stop = stop_if_fail))

  # fuel supply plot
  in_file <- get_data_file("fuel supply.xlsx")

  fuelsupply <- readwindEN(in_file, c('var', 'prod'), read_vardata = T,
                           zero_as_NA = T) %>%
    replace_na(list(type = 'M'))

  data_summary <- data_summary %>%
    bind_rows(check_dates(data = fuelsupply,
                          file_name = "fuel supply.xlsx",
                          check_dates_stop = stop_if_fail))

  write.csv(data_summary,
            file.path(output_dir, paste0("wind_data_summary_", today(), ".csv")),
            row.names = F)
}
