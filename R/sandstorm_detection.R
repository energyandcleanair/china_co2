# download AQ data --------------------------------------------------------
get_aq <- function(date_from, date_to, cities = NULL){
  seq.Date(ymd(date_from) - day(1), ymd(date_to), by = 'week') %>%
    pbapply::pblapply(function(start_date) {
      message(start_date)
      end_date <- start_date + days(6)

      source_url <- paste0("https://api.energyandcleanair.org/measurements?",
                           "country=CN&source=mee&",
                           glue("date_from={start_date}&date_to={end_date}"),
                           "&pollutant=pm10,pm25&level=city&",
                           "process=city_hour_mad&format=csv")

      if(!is.null(cities)){
        source_url <- source_url %>% paste0('&city=', paste(cities, collapse = ","))
      }

      conc_1h <- read_csv_and_retry(url = source_url) %>%
        select(-any_of('...1')) %>%
        mutate(across(date, convert_dt), across(value, as.numeric))

    }) %>% bind_rows()
}


#' Get a list of sandstorm dates for each location
#'
#' This function detects sandstorm events based on the following criteria:
#' 1. Determination of Start Time
#' The start time of the impact of sandstorm weather can be determined using two methods:
#'  (i) The start time is considered when the city's hourly average PM10 concentration is
#'  greater than or equal to twice the average PM10 concentration of the previous 6 hours
#'  and greater than 150 micrograms per cubic meter.
#'  (ii) The start time is considered when the ratio of the city's hourly PM2.5 to PM10
#'  concentration is less than or equal to 50% of the average ratio of the previous 6 hours.
#' 2. Determination of End Time
#' The end time of the sandstorm weather impact is determined when the city's hourly average
#' PM10 concentration first drops to within a relative deviation of less than or equal
#' to 10% compared to the average PM10 concentration of the 6 hours prior to the sandstorm weather.
#'
#' @param date_from
#' @param date_to
#' @param cities location_id for the cities to be included in the analysis. Default to NULL (all cities)
#'
#' @return A tibble with columns: location_id, sandstorm_date
#' @export
#'
#' @examples
get_sandstorm_dates <- function(date_from, date_to, cities = NULL){
  start_date <- date_from
  end_date <- date_to

  aq_data <- get_aq(start_date, end_date, cities = cities)


  # sandstorm detection -----------------------------------------------------

  # create hourly sequence
  date_seq <- tibble(date = seq(ymd_hms(paste(start_date, "00:00:00")),
                                ymd_hms(paste(end_date, "00:00:00")), by = "hour"))

  # generate complete combination for each date, location, and pollutant
  # fill in missing rows for rolling average
  aq_data_all <- date_seq %>% left_join(aq_data, by = c("date" = "date")) %>%
    complete(date, location_id, pollutant) %>%
    filter(!is.na(pollutant))

  # make table wider, generate required columns for sandstorm detection criteria
  aq_data_all_wide <- aq_data_all %>% pivot_wider(names_from = pollutant, values_from = value) %>%
    group_by(location_id) %>%
    mutate(pm_ratio = pm25/pm10,
           h6_running_pm10 = rollmean(pm10, 6, fill = NA, align = 'right', na.rm = T),
           h6_running_pm25 = rollmean(pm25, 6, fill = NA, align = 'right', na.rm = T),
           h6_running_pmratio = rollmean(pm_ratio, 6, fill = NA, align = 'right', na.rm = T)) %>%
    mutate(is_sandstorm_start = case_when((pm10 >= h6_running_pm10 * 2) & (pm10 > 150) ~ 1,
                                          pm_ratio <= 0.5 * h6_running_pmratio ~ 1,
                                          TRUE ~ 0))

  # get sandstorm start datetimes
  aq_data_all_wide <- aq_data_all_wide %>%
    mutate(sandstorm_start_time =
             case_when((is_sandstorm_start == 1) & (lag(is_sandstorm_start) == 0) ~ date,
                       TRUE ~ NA))

  # generate columns for sandstorm end detection criteria, get sandstorm end datetimes
  aq_data_all_wide <- aq_data_all_wide %>% group_by(location_id) %>%
    mutate(pm10_threshold = case_when(!is.na(sandstorm_start_time) ~ h6_running_pm10 * 1.1)) %>%
    fill(c(sandstorm_start_time, pm10_threshold), .direction = "down") %>%
    mutate(sandstorm_end_time = case_when((pm10 < pm10_threshold) & (date > sandstorm_start_time) ~ date,
                                          TRUE ~ NA)) %>%
    group_by(location_id, sandstorm_start_time) %>%
    mutate(sandstorm_end_time = min(sandstorm_end_time, na.rm = T),
           is_sandstorm_start = case_when(is.infinite(lag(sandstorm_end_time)) ~ 0,
                                          TRUE ~ is_sandstorm_start)) %>%
    ungroup

  # generate all the detected sandstorm dates for each location
  sandstorm_days <- aq_data_all_wide %>%
    filter(!is.na(sandstorm_start_time) & !is.infinite(sandstorm_end_time)) %>%
    select(location_id, sandstorm_start_time, sandstorm_end_time) %>%
    distinct() %>%
    rowwise() %>%
    mutate(sandstorm_date = seq(sandstorm_start_time, sandstorm_end_time, by = 'day') %>% date() %>% list) %>%
    unnest(sandstorm_date) %>%
    select(location_id, sandstorm_date) %>%
    distinct()
}





