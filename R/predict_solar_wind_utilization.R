#wind and solar utilization prediction using data from CREA API

read_weather <- function(weather_url = "https://api.energyandcleanair.org/v1/weather?region_type=gadm1&region_iso2=CN&variable=wind_speed,temperature,solar_radiation&format=csv") {
  tryCatch({
    read_csv(weather_url) ->
      met_from_API
  }, error=function(e) {
    message("Reading directly failed, downloading...")

    download.file(weather_url, file.path(tempdir(), "met_from_API.csv"),
                  mode = "wb")
    met_from_API <- read_csv(file.path(tempdir(), "met_from_API.csv"))
  })

  return(met_from_API)
}

predict_solar_wind_utilization <- function(pwr_data_monthly, output_plots=F, output_full_data=F,
                                           output_dir='outputs') {
  met_from_API <- read_weather()

  get_data_file("wind_solar_utilization_capacity_by_province.xlsx") %>%
    readwindEN(c('prov', 'var', 'source', 'source2'),
               read_vardata = T, skip=2, columnExclude = "National|China: Installed") ->
    provdata

  provdata %<>% filter(!grepl('Above.*Solar', Name)) %>%
    mutate(source=disambiguate(paste(source, source2), c('Wind', 'Solar')),
           var=disambiguate(var, c('Capacity', 'Utilization')),
           type=ifelse(var=='Capacity', 'cumulative', 'YTD')) %>%
    #calculate monthly utilization from YTD
    group_by(source, var, prov, type) %>% unYTD %>%
    filter(month(date)!=1) %>% ungroup

  #add cubed wind speed
  met_from_API %<>% filter(variable=='wind_speed') %>%
    mutate(value=value^3, variable='wind_speed_cubed') %>%
    bind_rows(met_from_API %>% filter(variable!='wind_speed_cubed'))

  last_full_month <- met_from_API %>% filter(day(date)==days_in_month(date)) %>%
    use_series(date) %>% max

  #aggregate to monthly averages and Jan-Feb to Feb
  met_from_API %>%
    filter(date<=last_full_month) %>%
    rename(data_source=source) %>%
    mutate(prov=region_name %>% fix_province_names(),
           date = case_when(month(date)==1~date %>% 'month<-'(2), T~date) %>%
             'day<-'(days_in_month(.))) %>%
    group_by(prov, variable, date) %>%
    summarise(across(value, mean)) %>% ungroup %>%
    spread(variable, value) ->
    met_for_model

  target_dates = seq.Date(ymd('2015-01-01'), last_full_month, by='month') %>% 'day<-'(days_in_month(.))

  provdata %<>% ungroup %>%
    select(date, Value1m, var, source, prov) %>%
    spread(var, Value1m) %>%
    filter(!is.na(Capacity)) %>%
    complete(prov, source, date=target_dates) %>%
    arrange(date) %>%
    group_by(source, prov) %>%
    fill(Capacity, .direction='down') %>%
    mutate(is_sane=Utilization>30*24*0.03 & Utilization<30*24*0.35 | is.na(Utilization),
           days_in_month = ifelse(month(date)==2,
                                  (31 + days_in_month(date))/2,
                                  days_in_month(date)),
           Utilization=Utilization * 30/days_in_month)

  #merge datasets
  left_join(met_for_model, provdata) -> alldata

  #aggregate to national data
  alldata %>%
    #filter(is_sane) %>%
    mutate(Utilization=ifelse(is_sane, Utilization, NA)) %>%
    group_by(source, date) %>%
    summarise(across(is.numeric, ~weighted.mean(.x, Capacity))) %>%
    select(-Capacity, -Utilization) ->
    national_data

  pwr_data_monthly %>% filter(source %in% c('Wind','Solar'), var=='Utilization') %>%
    ungroup %>% select(date, source, Utilization=Value1m) %>%
    full_join(national_data) ->
    national_data


  national_data %>% filter(source=='Solar') %>%
    lm(Utilization~solar_radiation*temperature+date, data=.) ->
    m_solar

  national_data %>% filter(source=='Wind') %>%
    lm(Utilization~wind_speed_cubed+temperature+date, data=.) -> m_wind


  national_data %<>% ungroup %>%
    mutate(Utilization_predicted = case_when(source=='Wind'~predict(m_wind, national_data),
                                             source=='Solar'~predict(m_solar, national_data)) *
             days_in_month / 30) %>%
    group_by(month(date), source) %>%
    mutate(Utilization_YoY = Utilization/lag(Utilization)-1,
           Utilization_predicted_YoY = Utilization_predicted/lag(Utilization_predicted)-1)

  #to do: add output to files
  if(output_plots) {
    m_solar %>% summary

    m_wind %>% summary

    national_data %>% filter(Utilization_YoY<.35) %>%
      ggplot(aes(Utilization_YoY, Utilization_predicted_YoY)) + geom_point() + facet_wrap(~source) +
      geom_smooth(method='lm') + geom_abline()

    national_data %>% filter(Utilization_YoY<.35) %>%
      ggplot(aes(Utilization, Utilization_predicted)) + geom_point() + facet_wrap(~source) +
      geom_smooth(method='lm') + geom_abline()
  }

  if(output_full_data) {
    alldata %>% saveRDS(file.path(output_dir, 'wind_solar_prediction_province_data.RDS'))
    national_data %>% saveRDS(file.path(output_dir, 'wind_solar_prediction_national_data.RDS'))
  }

  national_data %>% ungroup %>%
    select(date, source, Utilization_predicted, Utilization_predicted_YoY)
}
