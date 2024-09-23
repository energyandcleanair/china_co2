read_power_generation <- function(predict_solar_wind=F) {
  readwindEN(get_data_file('generation-consumption-utilization-capacity.xlsx'),
             c('var', 'source', 'subtype'), read_vardata = T, skip=3, zero_as_NA = T) -> monthly_raw

  #monthly_raw %>% distinct(var, source, subtype, type, Unit, Frequency) %>% print(n=30)

  monthly_raw %>% filter(Frequency=='Month', !(Source=='Wind' & var=='Electricity Generation')) %>%
    mutate(type=case_when(grepl('Coal Consumption Rate', var) ~ 'YTD mean',
                          grepl('YTD', Name) & !grepl('Capacity', var) ~ 'YTD',
                          T ~ 'single month'),
           source=ifelse(source=='YTD'&grepl('Solar', var), 'Solar Power', source),
           subtype=ifelse(subtype=='YTD', NA, subtype),
           source=disambiguate(source, c('Thermal', 'Hydro', 'Wind', 'Solar', 'Nuclear')),
           var=disambiguate(var, c(Utilization='Utilization',
                                   'Heat rate'='Consumption Rate',
                                   Consumption='Consumption',
                                   Capacity='Capacity',
                                   Generation='Generation|Output'))) ->
    monthly

  #calculate one-month heat rates from year-to-date averages
  monthly %<>% filter(var=='Heat rate') %>% addmonths() %>%
    group_by(year=year(date)) %>% group_modify(function(df, ...) {
      df %>% fill(Value, .direction='updown') %>%
        mutate(Value1m = ifelse(month(date)==1, Value, Value * month(date) - lag(Value) * (month(date) - 1)))
    }) %>% ungroup %>% select(-year) %>%
    bind_rows(monthly %>% filter(var!='Heat rate'))

  monthly %<>% group_by(var, source, subtype, type) %>% unYTD()

  #fill missing data:
  #Thermal utilization by fuel: last month's value
  monthly %>% filter(var=='Utilization' & source=='Thermal' & !is.na(subtype)) %>%
    group_by(subtype) %>% fill(Value1m, .direction='down') -> monthly_filled

  if(predict_solar_wind) {
    predict_solar_wind_utilization(monthly, output_plots=T) -> pred

    monthly %>% filter(var=='Utilization' & source %in% c('Wind', 'Solar')) %>%
      left_join(pred) %>% arrange(date) %>%
      group_by(source, month=month(date)) %>%
      mutate(Value1m = na.cover(Value1m, lag(Value1m) * (1+Utilization_predicted_YoY))) %>%
      ungroup %>% select(-month, -starts_with('Utilization_predicted')) %>%
      bind_rows(monthly_filled) ->
      monthly_filled

  } else {
    #Wind&Solar utilization: last year's value
    monthly %>% filter(var=='Utilization' & source %in% c('Solar', 'Wind')) %>%
      group_by(source, month=month(date)) %>%
      fill(Value1m, .direction='down') %>%
      ungroup %>% select(-month) %>%
      bind_rows(monthly_filled) ->
      monthly_filled
  }

  #wind & solar capacity: last year's monthly addition * last three months yoy
  monthly %>% filter(var=='Capacity' & source %in% c('Solar', 'Wind')) %>%
    group_by(source) %>%
    group_modify(function(df, group) {
      monthly_capacity_filled = na.approx(df$Value1m, na.rm=F)
      monthly_additions = monthly_capacity_filled - lag(monthly_capacity_filled)

      first_nonna <- which(!is.na(monthly_capacity_filled))[1]
      to_fill <- monthly_capacity_filled %>% is.na %>% which %>% subset(.>first_nonna)

      for(i in to_fill) {
        last_year = df$date[i] %>% subtract(366) %>% 'day<-'(days_in_month(.))
        last_year_additions = monthly_additions[df$date==last_year]

        last_month = df$date[i] %>% subtract(31) %>% 'day<-'(days_in_month(.))

        last_months = df$date[i] %>% subtract(31*1:3) %>% 'day<-'(days_in_month(.))
        last_months_additions = mean(monthly_additions[df$date %in% last_months])

        last_months_last_year = df$date[i] %>% subtract(31*1:3+366) %>% 'day<-'(days_in_month(.))
        last_months_last_year_additions = mean(monthly_additions[df$date %in% last_months_last_year])

        filled_value = monthly_capacity_filled[df$date==last_month] +
          last_year_additions * last_months_additions / last_months_last_year_additions

        if(length(filled_value)==1 & !is.na(filled_value)) {
          df$Value[i] <- filled_value
          df$Value1m[i] <- filled_value
          message('filling ', filled_value, ' for ', group$source, ' on ', df$date[i])
        }
      }
      return(df)
    }) %>%
    bind_rows(monthly_filled) ->
    monthly_filled

  monthly %<>% anti_join(monthly_filled %>% select(var, source, subtype)) %>%
    bind_rows(monthly_filled)

  #normalize consumption to 30 days
  monthly %<>% mutate(Value1m=Value1m*ifelse(var=='Consumption', 30/days_in_month(date), 1))

  #interpolate gaps in capacity data
  monthly %<>% filter(var=='Capacity') %>%
    group_by(source, subtype) %>% arrange(source, subtype, date) %>%
    mutate(Value1m=na.approx(Value1m, date, date, na.rm=F, rule=2)) %>%
    bind_rows(monthly %>% filter(var!='Capacity'))

  #fill in biomass utilization
  monthly %<>% filter(var=='Utilization') %>%
    group_by(date) %>% filter("Biomass" %notin% subtype) %>%
    filter(subtype=='Coal') %>%
    mutate(Value1m=4515*days_in_month(date)/365, subtype='Biomass') %>%
    bind_rows(monthly)

  monthly %<>% group_by(date, source, subtype) %>%
    mutate(days_in_feb = days_in_month(ymd(paste(year(date), 2, 1))),
           days_in_month = ifelse(month(date)<=2,
                                  (31 + days_in_feb)/2,
                                  days_in_month(date))) %>%
    summarise(Value1m=Value1m[var=='Capacity']*Value1m[var=='Utilization']/1e4*30/days_in_month(unique(date))) %>%
    mutate(var='Generation, calculated', Unit='100 million kwh') %>%
    bind_rows(monthly %>% filter(var!='Generation, calculated'))

  monthly %<>%
    #normalize thermal generation by fuel to the reported total
    group_by(date) %>%
    group_modify(function(df, group) {
      df$Value1m[df$source=='Thermal' & is.na(df$subtype) & df$var=='Generation'] -> thermal_total

      ind <- df$source=='Thermal' & !is.na(df$subtype) & df$var=='Generation, calculated'

      sum(df$Value1m[ind]) -> thermal_byfuel

      if(!is.na(thermal_byfuel)) {
        adj = thermal_total - thermal_byfuel
        df$Value1m[ind & df$subtype=='Coal'] -> orig_value
        df$Value1m[ind & df$subtype=='Coal'] %<>% add(adj)
        message('adjusting coal output on ', group$date, ' by ', adj/orig_value)
      }
      return(df)
    }) %>%
    #select reported or calculated generation
    group_by(source, subtype, date) %>%
    filter((var=='Generation' & source %notin% c('Wind', 'Solar')) |
             (var=='Generation, calculated' & 'Generation' %notin% var) |
             (var=='Generation, calculated' & source %in% c('Wind', 'Solar'))) %>%
    mutate(var='Generation, hybrid') %>%
    bind_rows(monthly %>% filter(var!='Generation, hybrid'))

  #add totals
  monthly %<>% filter(grepl('Generation', var),
                     source != 'Total',
                     source!='Thermal' | !is.na(subtype) | var=='Generation') %>%
    group_by(var, Unit, date) %>%
    summarise(across(Value1m, sum)) %>%
    mutate(source='Total') %>%
    bind_rows(monthly %>% filter(source != 'Total'), .)

  #predict last month's consumption from generation growth
  monthly %>% ungroup %>%
    filter(var %in% c('Consumption', 'Generation, hybrid'),
           source %in% c('Total', 'Whole Society')) %>%
    mutate(var=ifelse(var=='Consumption', var, 'Generation')) %>%
    select(date, var, Value1m) %>%
    spread(var, Value1m) -> gen_cons

  gen_cons %>% lm(Consumption~Generation+Generation:date, data=.) -> m_cons

  gen_cons %<>%
    mutate(Consumption_predicted=predict(m_cons, .)) %>%
    group_by(month=month(date)) %>%
    mutate(Consumption_predicted_YoY=Consumption_predicted/lag(Consumption_predicted)-1,
           Consumption_YoY=Consumption/lag(Consumption)-1,
           Generation_YoY=Generation/lag(Generation)-1)

  gen_cons %>%
    ggplot(aes(Consumption_YoY, Consumption_predicted_YoY)) + geom_point() + geom_abline()

  gen_cons %>%
    ggplot(aes(Consumption_YoY, Generation_YoY)) + geom_point() + geom_abline()

  gen_cons %>%
    ggplot(aes(Generation, Consumption)) + geom_point() + geom_abline()

  monthly %>% filter(var=='Consumption') %>%
    left_join(gen_cons %>% select(date, month, Consumption_predicted_YoY)) %>%
    group_by(month) %>%
    mutate(Value1m=na.cover(Value1m, lag(Value1m) * (1+Consumption_predicted_YoY))) ->
    cons_filled

  monthly %<>% filter(var !='Consumption') %>% bind_rows(cons_filled)

  "source	year	Value
Wind	2022	7626.7
Solar	2022	4272.7
Wind	2023	8858.7
Solar	2023	5841.5" %>% textConnection() %>%
    read.table(sep='\t', header=T) %>%
    mutate(Unit="100 million kwh") ->
    gongbao

  #compile yearly data
  monthly_raw %>% filter(Frequency=='Year', month(date)==12, !is.na(Value)) %>%
    mutate(year=year(date), source=disambiguate(source, c('Wind', 'Solar'))) %>%
    bind_rows(gongbao) %>% mutate(var='Generation, annual reporting') -> yearly

  return(list(monthly=monthly, yearly=yearly))
}
