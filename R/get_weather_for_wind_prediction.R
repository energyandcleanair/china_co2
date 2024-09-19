get_weather_for_wind_prediction <- function(avg_jan_feb=T, use_cache=T, plot=F){


  weather_provincial <- get_weather_provincial(avg_jan_feb=avg_jan_feb, use_cache=use_cache)
  wind_power_density <- get_wind_power_density_from_stations(avg_jan_feb=avg_jan_feb, use_cache=use_cache)
  wind_power_curve <- get_wind_power_curve_from_stations(avg_jan_feb=avg_jan_feb, use_cache=use_cache)

  weather <- bind_rows(weather_provincial, wind_power_density, wind_power_curve) %>%
    select(prov, variable, date, value)


  # Plot for verification
  if(plot){

    weather %>%
      filter(variable %in% c('temperature', 'wind_power_density', 'wind_power_curve')) %>%
      # normalise values by variable
      group_by(prov,variable) %>%
      # mutate(value=(value-min(value, na.rm=T))/(max(value, na.rm=T)-min(value, na.rm=T))) %>%
      ggplot(aes(date, value, color=variable)) +
      geom_line() +
      facet_wrap(~prov, scales="free_y") +
      theme_minimal() +
      # hide y axis
      theme(axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(
        x="Date",
        y="Value",
        title="Weather data for wind power prediction"
      )
  }

  return(weather)
}

get_weather_provincial <- function(avg_jan_feb=T, use_cache=T){

  filepath <- glue("cache/weather_provincial{ifelse(avg_jan_feb, '_avg_jan_feb', '')}.csv")
  if (use_cache & file.exists(filepath)){
    return(read_csv(filepath))
  }

  dir.create("cache", showWarnings = FALSE)

  url <- "https://api.energyandcleanair.org/v1/weather?region_type=gadm1&region_iso2=CN&variable=temperature,solar_radiation,wind_speed&format=csv&gzip=true"
  met <- read_csv(gzcon(url(url)))

  #add cubed wind speed
  met %<>% filter(variable=='wind_speed') %>%
    mutate(value=value^3, variable='wind_speed_cubed') %>%
    bind_rows(met %>% filter(variable!='wind_speed_cubed'))


  #add cubed wind speed divided by temperature in K
  met %<>% filter(variable %in% c('wind_speed', 'temperature')) %>%
    select(-c(unit)) %>%
    spread(variable, value) %>%
    mutate(value=wind_speed^3/temperature, variable='wind_speed_cubed_div_temp') %>%
    select(-wind_speed, -temperature) %>%
    bind_rows(met %>% filter(variable!='wind_speed_cubed_div_temp'))



  #aggregate to monthly averages and Jan-Feb to Feb
  met <- met %>%
    rename(data_source=source) %>%
    mutate(prov=region_name %>% fix_province_names(),
           date = case_when(
             avg_jan_feb & month(date)==1 ~ date %>% 'month<-'(2),
             T~date) %>%
             'day<-'(days_in_month(.))) %>%
    group_by(prov, variable, date) %>%
    summarise(
      value = mean(value)
    ) %>% ungroup

  write_csv(met, filepath)
  return(met)
}

get_wind_power_density_from_stations <- function(avg_jan_feb=T, use_cache=T){
  get_weather_from_stations("wind_power_density", avg_jan_feb=avg_jan_feb, use_cache=use_cache)
}

get_wind_power_curve_from_stations <- function(avg_jan_feb=T, use_cache=T){
  get_weather_from_stations("wind_power_curve", avg_jan_feb=avg_jan_feb, use_cache=use_cache)
}


get_weather_from_stations <- function(variable, avg_jan_feb=T, use_cache=T){

  filepath <- glue("cache/{variable}.csv")
  dir.create("cache", showWarnings = FALSE)

  # We cache before processing, in case we want to change processing...

  if (use_cache & file.exists(filepath)){
    wind <- read_csv(filepath)
  }else{
    # Too much weather data to download withou gzipping
    library(httr)
    library(readr)
    library(memoise)
    timeout_seconds <- 300

    years <- seq(2015, 2024)

    get_year <- function(year){
      url <- glue("https://api.energyandcleanair.org/v1/weather?region_type=station&station_source=gem&region_iso2=CN&variable={variable}&format=csv&gzip=true&aggregate_by=region_id,month,aggregate_fn=sum")
      year_filter <- glue("&date_from={year}-01-01&date_to={year}-12-31")
      url <- glue(url, year_filter)

      response <- GET(url, timeout=timeout_seconds)

      # Check if the request was successful
      if (status_code(response) == 200) {
        # Read the gzipped content into a data frame using readr
        wind <- read_csv(gzcon(rawConnection(content(response, "raw"))))
      } else {
        print(glue("Failed for year {year}"))
        wind <- NULL
      }
      return(wind)
    }

    wind <- pbapply::pblapply(years, get_year) %>%
      bind_rows() %>%
      rename(date=month)
    write_csv(wind, filepath)
  }


  # Read GEM data
  locations <- read_xlsx(get_data_file("Global-Wind-Power-Tracker-June-2024.xlsx"), sheet="Data")

  # Create locations matched with capacity and year
  locations_year_cap <- locations %>%
    filter(Status=="operating",
           `Country/Area`=="China") %>%
    tidyr::crossing(year=unique(year(wind$date))) %>%
    filter(`Start year` <= year) %>%
    group_by(region_id=glue('gem_{tolower(`GEM location ID`)}'),
             prov=`State/Province`,
             year) %>%
    summarise(capacity_mw=sum(`Capacity (MW)`)) # Some locations have multiple provinces...


  result <- wind %>%
    mutate(year=year(date)) %>%
    left_join(
      locations_year_cap,
      by=c("region_id", "year"),
      relationship="many-to-many" # Certain location ids have two provinces...
      ) %>%
    group_by(
      date,
      variable,
      unit,
      averaging_period,
      source,
      prov
    ) %>%
    summarise(
      value=weighted.mean(value, capacity_mw, na.rm=TRUE)
    ) %>%
    group_by(
      date=case_when(
        avg_jan_feb & month(date) %in% 1:2 ~ date %>% `month<-`(2),
        TRUE ~ date
      ),
      variable,
      prov
    ) %>%
    summarise(
      value=mean(value, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    mutate(date='day<-'(date, days_in_month(date)))

  return(result)
}

