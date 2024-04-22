check_wind_update <- function(output_dir){
  data_summary <- list()

  # industry plot data
  in_file <- get_data_file("monthly industry stats with YoY.xlsx")
  prod <- readwindEN(in_file, c('var', 'prod'),
                     read_vardata = T, zero_as_NA = T, skip = 3)

  in_file_compare <- get_data_file('monthly industry stats.xlsx')
  prod_compare <- readwindEN(in_file_compare, c('var', 'prod'),
                             read_vardata = T, zero_as_NA = T, skip = 1)

  data_summary <- data_summary %>% bind_rows(check_dates(data = prod,
                                                         file_name = "monthly industry stats with YoY.xlsx",
                                                         check_dates_stop = T))
  data_summary <- data_summary %>% bind_rows(check_dates(data = prod_compare,
                                                         file_name = "monthly industry stats.xlsx",
                                                         check_dates_stop = T))

  # steel plot data
  in_file <- get_data_file("steel plant operating rates.xlsx")

  steel <- readwindEN(in_file, paste0('V', 1:3),
                      read_vardata = T, zero_as_NA = T, force_last_of_month = F)

  data_summary <- data_summary %>% bind_rows(check_dates(data = steel,
                                                         file_name = "steel plant operating rates.xlsx",
                                                         check_dates_stop = T))

  # capacity plot data
  in_file <- get_data_file("Power Capacity.xlsx")
  cap <- readwindEN(in_file, c('var', 'source', 'fuel', 'YTD'), read_vardata = T) %>%
    mutate(var = ifelse(grepl('New', var), 'Newly added capacity', 'Installed capacity'),
           source = source %>% gsub(' ?Power.*', '', ., ignore.case = T) %>% gsub('YTD', 'All', .),
           fuel = ifelse(is.na(fuel) | fuel %in% c('YTD', 'National'), 'All', fuel))

  data_summary <- data_summary %>% bind_rows(check_dates(data = cap,
                                                         file_name = "Power Capacity.xlsx",
                                                         check_dates_stop = T))

  # fuel supply plot
  in_file <- get_data_file("fuel supply.xlsx")

  fuelsupply <- readwindEN(in_file, c('var', 'prod'), read_vardata = T, zero_as_NA = T) %>%
    replace_na(list(type = 'M'))

  data_summary <- data_summary %>% bind_rows(check_dates(data = fuelsupply,
                                                         file_name = "fuel supply.xlsx",
                                                         check_dates_stop = T))

  write.csv(data_summary,
            file.path(output_dir, paste0("wind_data_summary_", today(), ".csv")),
            row.names = F)
}
