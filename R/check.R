check_aq_data <- function(aq, aq_dw, cities, focus_month=focus_month, n_polls=3){

    # Check that for each location_id and pollutant, there is data for the whole month
    aq_count <- aq %>%
        filter(floor_date(date, unit="month") == focus_month) %>%
        group_by(location_id, pollutant) %>%
        summarise(n = n())

    if(any(aq_count$n != days_in_month(focus_month))){
      stop(glue("Missing air quality data {min(aq_count$n)} / {days_in_month(focus_month)}"))
    }

    if(any(aq$source != 'mee')){
      stop("Wrong AQ source")
    }

    aq_dw_count <- aq_dw %>%
        filter(floor_date(date, unit="month") == focus_month) %>%
        group_by(location_id, pollutant) %>%
        summarise(n = n())

    if(any(aq_dw_count$n != lubridate::days_in_month(focus_month))
       || (nrow(aq_dw_count) != length(cities) * n_polls)){
       stop(glue("Missing deweathered air quality data {min(aq_dw_count$n)} / {days_in_month(focus_month)}"))
    }
}
