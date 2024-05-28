read_power_generation <- function() {
  readwindEN('inst/extdata/generation-consumption-utilization-capacity.xlsx',
             c('var', 'source', 'subtype'), read_vardata = T, skip=3, zero_as_NA = T) -> monthly_raw

  #monthly_raw %>% distinct(var, source, subtype, type, Unit, Frequency) %>% print(n=30)

  monthly_raw %>% filter(Frequency=='Month') %>%
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

  monthly %<>% filter(var=='Heat rate') %>% addmonths() %>%
    group_by(year=year(date)) %>% group_modify(function(df, ...) {
      df %>% fill(Value, .direction='updown') %>%
        mutate(Value1m = ifelse(month(date)==1, Value, Value * month(date) - lag(Value) * (month(date) - 1)))
    }) %>% ungroup %>% select(-year) %>%
    bind_rows(monthly %>% filter(var!='Heat rate'))

  monthly %<>% group_by(var, source, subtype, type) %>% unYTD()

  #normalize consumption to 30 days
  monthly %<>% mutate(Value1m=Value1m*ifelse(var=='Consumption', 30/days_in_month(date), 1))

  #interpolate gaps in capacity data
  monthly %<>% filter(var=='Capacity') %>%
    group_by(source, subtype) %>% arrange(source, subtype, date) %>%
    mutate(Value1m=na.approx(Value1m, date, date, na.rm=F)) %>%
    bind_rows(monthly %>% filter(var!='Capacity'))

  #fill in biomass utilization
  monthly %<>% filter(var=='Utilization') %>%
    group_by(date) %>% filter("Biomass" %notin% subtype) %>%
    filter(subtype=='Coal') %>%
    mutate(Value1m=4515*days_in_month(date)/365, subtype='Biomass') %>%
    bind_rows(monthly)

  monthly %<>% group_by(date, source, subtype) %>%
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
        adj = thermal_total/thermal_byfuel
        df$Value1m[ind] %<>% multiply_by(adj)
        message('adjusting thermal output on ', group$date, ' by ', adj)
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
