#' Collect curtailment rates from various official sources before 2021 and from Wind platform 2021 onwards.
#' Values are estimated monthly and by province
#'
#' @return
#' @export
#'
#' @examples
get_wind_curtailment_rates <- function(){


  provs <- read_xlsx(get_data_file('provincesZH.xlsx'))


  # Manual collection -------------------------------------------------------
  manual_raw <- read_xlsx(get_data_file("wind_curtailment.xlsx"), sheet="Manual")

  manual <- manual_raw %>%
    select(province_zh=province, period, value, variable, type) %>%

    # Clean names
    mutate(province_zh=gsub(" ", "", province_zh)) %>%

    # English names
    left_join(
      provs %>% select(province_en = Province,
                       province_zh = ProvinceZH) %>%
        bind_rows(tibble(province_en="China", province_zh="全国"))
    ) %>%
    filter(!is.na(province_en)) %>%

    # convert utilisation to curtailment
    mutate(value = case_when(variable=="利用率" ~ 1-value, T ~ value)) %>%
    select(-c(variable)) %>%

    # Yearly to monthly
    rename(year=period) %>%
    group_by(province_en, type) %>%
    # filter(province_en=="Qinghai")
    group_modify(function(data, group){
      data_smoothed <- smooth_yearly_values(data %>% select(year, value))
      data %>%
        select(-c(value)) %>%
        left_join(data_smoothed, by="year")
    }) %>% ungroup()

  # Wind platform -----------------------------------------------------------
  wind_raw <- read_xlsx(get_data_file("wind_curtailment.xlsx"), sheet="WIND platform")

  # select columns ending with Utilization Rate of Wind Power
  wind <- wind_raw %>%
    select_at(vars(c("Name", ends_with("Utilization Rate of Wind Power")))) %>%
    pivot_longer(-Name, names_to="col", values_to="value") %>%
    #TODO improve this assumption
    # Extract Xinjiang from China: Xinjiang: Utilization Rate of Wind Power
    mutate(province_en = str_extract(col, "China: (.*): Utilization Rate of Wind Power", group = 1)) %>%
    # Group Inner Mongolia assuming 50/50. There's probably better to do
    mutate(province_en = case_when(grepl("Mongolia", province_en) ~ "Inner Mongolia",
                                   col=="China: Utilization Rate of Wind Power" ~ "China",
                                   T ~ province_en))  %>%
    group_by(date=Name, province_en) %>%
    summarise(value=mean(value)) %>%
    ungroup() %>%
    mutate(date=as.Date(date)) %>%
    mutate(value=1-value/100) %>%
    mutate(type="Wind")


  result <- bind_rows(
    manual,
    wind
  ) %>%
    tidyr::complete(nesting(province_en),
                    date=seq.Date(min(manual$date), max(wind$date), by="month"),
                    type,
                    fill=list(value=NA)) %>%
    group_by(province_en) %>%
    arrange(date) %>%
    # First interpolate
    mutate(value = na.approx(value, na.rm=F)) %>%
    # Then assume constant curtailment before and after
    #TODO improve this assumption
    tidyr::fill(value, .direction="downup") %>%
    ungroup()



  # Verification
  ggplot(result,
         aes(date, value, col=province_en)) + geom_line() + labs(title="Wind curtailment rates") +
      facet_wrap(~province_en, scales='free_y')


  return(result)
}


#' Convert year and yearly values to monthly values with constrained smoothing
#'
#' @param data tibble with two columns year and value
#' @param values
#'
#' @return tibble with three column year, monthdate value
#' @export
#'
#' @examples
smooth_yearly_values <- function(data){

  library(zoo)
  # library(optimize)

  # Function to perform constrained smoothing
  constrained_smoothing <- function(yearly_values, num_months = 12) {
    n_years <- length(yearly_values)
    if(n_years == 1) {
      return(rep(yearly_values, num_months))
    }
    # Create monthly values by repeating each yearly value 12 times
    monthly_values <- rep(yearly_values, each = num_months)

    # Apply initial smoothing
    smooth_values <- rollmean(monthly_values, k = num_months, align = "center", fill = "extend")

    # Function to adjust smoothed values to match yearly averages
    adjust_smoothing <- function(factor) {
      adjusted <- smooth_values * factor
      yearly_means <- tapply(adjusted, rep(1:n_years, each = num_months), mean)
      sum((yearly_means - yearly_values)^2)
    }

    # Find optimal adjustment factor
    opt_factor <- optimize(adjust_smoothing, c(0.5, 1.5))$minimum

    # Apply optimal adjustment
    final_smooth <- smooth_values * opt_factor

    # Ensure exact yearly averages
    for (i in 1:n_years) {
      year_indices <- ((i-1) * num_months + 1):(i * num_months)
      current_mean <- mean(final_smooth[year_indices])
      final_smooth[year_indices] <- final_smooth[year_indices] * (yearly_values[i] / current_mean)
    }

    return(final_smooth)
  }

  data <- data %>% arrange(year)
  monthly_values <- constrained_smoothing(data$value)

  return(tibble(year = rep(data$year, each = 12),
                month = rep(1:12,length(data$year)),
                value = monthly_values) %>%
           mutate(date = as.Date(paste(year, month, "01", sep="-")) %>%
                    as.Date()) %>%
           select(-month)
           )
}
