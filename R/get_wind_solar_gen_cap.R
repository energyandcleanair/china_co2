get_wind_solar_gen_cap <- function(use_cache=T){

  filepath <- "cache/wind_solar_gen_cap.csv"
  if(use_cache & file.exists(filepath)){
    return(read_csv(filepath))
  }

  dir.create(dirname(filepath), showWarnings = FALSE)

  get_data_file("wind_solar_utilization_capacity_by_province.xlsx") %>%
    readwindEN(c('prov', 'var', 'source', 'source2'),
               read_vardata = T, skip=2, columnExclude = "National|China: Installed") ->
    provdata


  provdata %<>% filter(!grepl('Above.*Solar', Name)) %>%
    mutate(source=disambiguate(paste(source, source2), c('Wind', 'Solar')),
           var=disambiguate(var, c('Capacity', 'Utilization')),
           type=ifelse(var=='Capacity', 'cumulative', 'YTD')) %>%
    #calculate monthly utilization from YTD
    group_by(source, var, prov, type) %>%
    unYTD %>%
    filter(month(date)!=1)

  provdata_wide <- provdata %>% ungroup %>%
    select(date, Value1m, var, source, prov) %>%
    spread(var, Value1m) %>%
    filter(!is.na(Capacity))

  # Filter out un-sane values
  provdata_wide %<>%
    mutate(is_sane=Utilization>30*24*0.03 & Utilization<30*24*0.35)

  # # Plot
  # provdata_wide %>%
  #   filter(source=="Wind", is_sane) %>%
  #   tidyr::gather(var, Value1m, Utilization, Capacity) %>%
  #   ggplot(aes(date, Value1m, color=prov)) +
  #   geom_line() +
  #   facet_wrap(prov~var, scales="free_y")
  #
  #
  # # Plot yearly
  # provdata_wide %>%
  #   filter(source=="Wind", is_sane) %>%
  #   tidyr::gather(var, Value1m, Utilization, Capacity) %>%
  #   group_by(prov, var) %>%
  #   mutate(year=year(date)) %>%
  #   ungroup %>%
  #   group_by(prov, var, year) %>%
  #   summarise(Value1m=mean(Value1m)) %>%
  #   ggplot(aes(year, Value1m, color=prov)) +
  #   geom_line() +
  #   facet_wrap(prov~var, scales="free_y")


  # Add curtailment rate
  provdata_wide %<>%
    left_join(
      get_wind_curtailment_rates() %>%
        select(province_en, value, type, date) %>%
        rename(prov=province_en,
               curtailment=value,
               source=type) %>%
        mutate(date = date %>% 'day<-'(lubridate::days_in_month(date)))
    ) %>%
    mutate(curtailment=coalesce(curtailment, 0)) %>%
    mutate(Utilization_corrected=Utilization / (1-curtailment))


  # Plot yearly
  provdata_wide %>%
    filter(source=="Wind", is_sane) %>%
    tidyr::gather(var, Value1m, Utilization, Capacity, Utilization_corrected) %>%
    group_by(prov, var) %>%
    mutate(year=year(date)) %>%
    ungroup %>%
    group_by(prov, var, year) %>%
    summarise(Value1m=mean(Value1m)) %>%
    ggplot(aes(year, Value1m, color=prov)) +
    geom_line() +
    facet_wrap(prov~var, scales="free_y")


  # Save
  write_csv(provdata_wide, filepath)

  return(provdata_wide)
}
