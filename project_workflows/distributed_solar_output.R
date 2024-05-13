source('load_package.R')

pwr_data <- read_power_generation()
attach(pwr_data)

monthly %>% distinct(var, source, subtype, type, Unit, Frequency) %>% print(n=30)



ember_raw <- get_ember_monthly_data() %>%
  set_names(make.names(names(.))) %>%
  filter(Country.code=='CHN', Unit=='TWh')

ember_raw %>% mutate(var=ifelse(Variable=='Demand', 'Consumption, Ember', 'Generation, Ember'),
                     Date=Date %>% 'day<-'(days_in_month(Date)), Value1m=Value*10) %>%
  rename(source=Variable, date=Date) %>%
  filter(source %in% c('Demand', 'Wind', 'Solar', 'Fossil', 'Nuclear', 'Hydro, Bioenergy and Other Renewables')) ->
  ember



monthly %>% ungroup %>% distinct(var, Unit)



monthly %>% bind_rows(ember) %>%
  filter(is.na(subtype), grepl('Generation|Consumption', var)) %>%
  group_by(date, var) %>% summarise(across(Value1m, ~sum(.x, na.rm=T))) %>%
  mutate(Value1m=Value1m/ Value1m[var=='Consumption']) %>%
  filter(Value1m>0, year(date)>=2020) %>%
  ggplot(aes(date, Value1m, col=var)) + geom_line() #+ facet_wrap(~var)


#analyse annual numbers
monthly %>% bind_rows(ember) %>% filter(is.na(subtype), grepl('Generation|Consumption', var)) %>%
  group_by(var, source, year=year(date)) %>% summarise(across(c(Value=Value1m), ~sum(.x, na.rm=T))) %>%
  bind_rows(yearly) %>%
  filter(Value>0, year>=2020, source %in% c('Wind', 'Solar')) %>%
  ggplot(aes(year, Value, col=var, shape=var)) + geom_line() + geom_point(alpha=.5) + facet_wrap(~source)


monthly %>% filter(var=='Utilization', year(date)>=2019) %>%
  mutate(year=as.factor(year(date)), plotdate=date %>% 'year<-'(2024)) %>%
  ggplot(aes(plotdate, Value1m, col=year)) + geom_line() + facet_wrap(~source+subtype, scales='free_y')

monthly %>% filter(var=='Utilization', year(date)>=2019, month(date)==3) %>%
  mutate(year=as.factor(year(date)), plotdate=date %>% 'year<-'(2024)) %>%
  ggplot(aes(year, Value1m)) + geom_col() + facet_wrap(~source+subtype, scales='free_y')

monthly %>% filter(var=='Generation, calculated') %>%
  group_by(var, source, subtype, month(date)) %>%
  mutate(yoy=Value1m/lag(Value1m)-1, change=Value1m-lag(Value1m)) %>%
  group_by(date) %>%
  mutate(share_of_generation=Value1m/sum(Value1m[is.na(subtype)]),
         share_of_growth=change/sum(change[is.na(subtype)])) %>%
  group_by(source, subtype, month(date)) %>%
  mutate(share_previous=lag(share_of_generation),
         share_change=share_of_generation-share_previous) %>%
  ungroup %>% filter(date==max(date)) %>%
  select(source, subtype, yoy, starts_with('share'))

monthly %>% filter(var=='Utilization', year(date)>=2019) %>%
  mutate(Value1m=Value1m/(days_in_month(date)*24)) %>%
  group_by(var, source, subtype, month(date)) %>%
  mutate(previous=lag(Value1m),
         average_5yr=mean(Value1m)) %>%
  ungroup %>% filter(date==max(date)) %>%
  select(source, subtype, Value1m, previous, average_5yr)


monthly %>%
  filter(var=='Capacity', source=='Solar') %>%
  group_by(source, subtype) %>% filter(month(date)!=1) %>%
  mutate(added=Value-lag(Value),
         plotdate=date %>% 'year<-'(2024), year=as.factor(year(date))) %>%
  filter(year(date)>=2018) %>%
  ggplot(aes(plotdate, added, col=year)) + geom_line()
