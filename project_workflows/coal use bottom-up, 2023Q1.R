in_file = "data/monthly industry stats.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption', read_vardata = T) -> prod

prod %<>%
  group_by(var, prod, type) %>%
  group_modify(function(df, k) {
    message(k)
    df %>% unYTD()
  }) %>% ungroup

prod %<>% mutate(sector_coal=case_when(grepl('Thermal Power$', prod)~'Power',
                                       grepl('Ethylene|Chemical Fibers|Sulfuric Acid|Soda', prod)~'Chemical',
                                       grepl('Paper|Clothes|Yarn', prod)~'Other',
                                       grepl('Crude Steels|Non-ferrous Metals', prod)~'Metallurgy',
                                       grepl('Coke', prod)~'Coking',
                                       grepl('Cement$|Glass', prod)~'Building Materials')) %>%
  filter(!is.na(sector_coal))

coal_sectors <- unique(prod$sector_coal)

d %>% ungroup %>% filter(grepl('Coking|Steam', prod), sector != 'Total') %>%
  mutate(sector_coal = case_when(grepl('Coking', prod)~'Coking',
                                 T~disambiguate(sector, coal_sectors))) ->
  d_coal

read_csv('data/monthly_full_release_long_format-4.csv') %>%
  set_names(make.names(names(.))) %>%
  filter(Variable=='Coal', Country.code=='CHN', Unit=='TWh') %>%
  select(date=Date, generation_twh=Value) -> coal_gen

last_month_firstday <- last_month %>% 'day<-'(1)
if(max(coal_gen$date) < last_month_firstday) {
  message("The Ember dataset available locally has data until ", max(coal_gen$date), ' - backfilling')

  prod %>% group_by(prod) %>% mutate(YoY=get.yoy(Value1m, date)) %>% filter(date==last_month, prod=='Thermal Power') %>%
    use_series(YoY) -> thermal_power_growth
  coal_gen %<>% filter(date==last_month_firstday) %>% mutate(date=date %>% 'year<-'(2023), generation_twh=generation_twh*(1+thermal_power_growth)) %>%
    bind_rows(coal_gen, .)
}

coal_gen %>% ggplot(aes(date, generation_twh)) + geom_line()

coal_gen %<>% mutate(date = date %>% 'day<-'(days_in_month(.))) %>%
  mutate(prod='Coal Power', sector_coal='Power') %>%
  rename(Value1m=generation_twh)

prod %<>% bind_rows(coal_gen)

prod %>%
  filter(date>='2012-01-01',
         sector_coal != 'Other' | date>='2018-01-01',
         sector_coal != 'Chemical' | date>='2016-01-01') %>%
  select(date, industrial_product=prod, sector_coal, output=Value1m) %>%
  left_join(d_coal %>% select(date, sector_coal, var, sector, energy_product=prod, coal_use=Value1m)) %>%
  na.omit %>%
  group_by(sector_coal) %>%
  mutate(across(industrial_product, make.names), month=as.factor(month(date))) %>%
  group_map(function(df, group) {
    df$industrial_product %>% unique %>% paste(collapse='+') -> indys
    form=paste0('coal_use~(',indys,'):month+(',indys,'):date+0')
    message(form)
    df %<>% spread(industrial_product, output) %>%
      mutate(sector_coal=group$sector_coal)

    df %>% filter(date<'2022-07-01') %>% lm(as.formula(form), data=.) -> m
    df$coal_use_predicted <- predict(m, df)
    print(summary(m))
    list(data=df, model=m)
  }) -> coaluse_pred


coaluse_pred %>% lapply('[[', 'data') %>% bind_rows() %>%
  #bind_rows(coaluse_heating) %>%
  mutate(coal_use_predicted = case_when(grepl('Chemical|Heating|Other', sector_coal)~coal_use, T~coal_use_predicted)) %>%
  pivot_longer(starts_with('coal_use')) %>%
  mutate(name = recode(name, 'coal_use'='reported','coal_use_predicted'='predicted based on output')) ->
  coaluse_df

coaluse_df %<>% group_by(name, date) %>%
  summarise(across(value, sum)) %>%
  mutate(sector_coal='Total') %>%
  bind_rows(coaluse_df)

#drought effect

in_file = 'data/power generation by type.xlsx'
getwindvars(in_file)
readwindEN(in_file, c('var', 'prod'),
           columnFilter = "Electricity Production|Thermal|Hydro|Nuclear|Solar|Wind", read_vardata = T) %>%
  filter(!is.na(Value)) -> pwr

pwr %<>% group_by(var, prod, type) %>%
  unYTD() %>% filter(Value1m>0) %>% roll12m

pwr %>% filter(date>='2017-01-01') -> pwr_plot


pwr_plot %>%
  ggplot(aes(date, Value12m*12/10)) + geom_line(linewidth=1) +
  facet_wrap(~prod, scales='free_y') +
  geom_smooth(data=pwr_plot %>% filter(date<='2020-01-01'),
              fullrange=T, method='lm', col='darkred') +
  geom_vline(aes(xintercept=ymd('2020-01-29'), linetype='First COVID-19 lockdown')) +
  theme_crea() + labs(title="Trends in power generation in China",
                      y="TWh/year, 12-month moving sum", x="", linetype='') +
  theme(legend.position = 'top') +
  scale_linetype_manual(values='dashed') -> p
quicksave(file.path(output_dir, 'Trends in power generation in China.png'), plot=p, footer_height = .03)


readwindEN('data/Power Capacity.xlsx', c('var', 'prod'), columnExclude = 'New|Conventional|Coal|Gas',
           zero_as_NA = T) %>% filter(prod!='YTD') -> cap

pwr %<>% mutate(prod = case_when(grepl('Hydro', prod)~'Hydro', T~prod)) %>%
  left_join(cap %>% mutate(prod = case_when(grepl('Hydro', prod)~'Hydro', grepl('Solar', prod)~'Solar', T~prod)) %>%
              select(date, prod, capacity=Value)) %>%
  group_by(prod) %>% mutate(capacity = na.approx(capacity, date, date, na.rm=F),
                            FLH = Value1m/capacity)


pwr %<>%
  group_by(prod, month=month(date)) %>%
  mutate(FLH_norm=median(FLH[year(date)>2015], na.rm=T),
         Value1m_norm=capacity*FLH_norm)

pwr %>% filter(grepl('Wind|Solar|Hydro', prod)) %>%
  ggplot(aes(date)) + geom_line(aes(y=Value1m-Value1m_norm), col='blue') + facet_wrap(~prod, scales='free_y')

pwr %>% group_by(date, prod) %>%
  filter(grepl('Hydro', prod)) %>%
  mutate(TWh = (Value1m-Value1m_norm)/10) %>%
  summarise(across(TWh, ~sum(.x, na.rm=T))) %>% filter(date>='2016-01-01') -> drought_effect


pwr_plot %>% filter(grepl('Electri|Hydro|Thermal', prod), year(date) %in% 2019:year(last_month)) %>%
  mutate(year=year(date), month=month(date)) %>%
  ggplot(aes(month, Value1m, col=as.factor(year))) + geom_line() + facet_wrap(~prod, scales='free_y')

read_csv('https://api.energyandcleanair.org/v1/weather?variable=HDD&format=csv&region_id=CN')

pwr_plot %>% filter(grepl('Electri', prod), year(date) %in% 2021:year(last_month)) %>%
  mutate(year=year(date), month=month(date), summer=month %in% 7:8) %>%
  group_by(year, month, summer) %>%
  summarise(across(Value1m, sum)) %>% ungroup %>%
  mutate(bau_growth = sum(Value1m[!summer & year==2022])/sum(Value1m[!summer & year==2021])) %>%
  group_by(month) %>%
  mutate(TWh=(Value1m - Value1m[year==2021] * bau_growth)/10) %>%
  filter(summer, year==2022) ->
  heat_effect

bind_rows(heat_effect, drought_effect) %>%
  mutate(TWh=abs(TWh), Mt=TWh*.305) %>%
  group_by(date) %>%
  summarise(across(c(Mt, TWh), sum)) ->
  heat_effect

coaluse_df %>% filter(sector_coal %in% c('Power', 'Total'), grepl('predict', name)) %>%
  full_join(heat_effect) %>%
  mutate(value = value - pmax(0, Mt, na.rm=T)*100,
         name = 'without drought&heatwave') %>%
  bind_rows(coaluse_df) %>%
  mutate(name = factor(name, levels=c('reported','without drought&heatwave',
                                      'predicted based on output'))) %>%
  group_by(sector_coal, name) %>%
  mutate(across(c(value_12m=value), zoo::rollmeanr, k=12, fill=NA)) ->
  coaluse_plot

coaluse_plot %>% filter(date>='2016-01-01', sector_coal == 'Power') %>%
  mutate(value_12m=value_12m*12/100,
         variable='Power sector coal consumption in China, 12-month moving mean',
         unit='Mt/year') %>%
  select(scenario=name, variable, date, value_12m, unit) %>%
  write_csv(file.path(output_dir, 'Power sector coal consumption in China.csv'))

coaluse_plot %>%
  filter(date>='2016-01-01', sector_coal == 'Power') %>% #, sector_coal %in% c('Power', 'Total')
  ggplot(aes(date, value_12m*12/100, col=name)) + facet_wrap(~sector_coal, scales='free_y') +
  geom_line(size=1) +
  labs(title='Power sector coal consumption in China', subtitle='12-month moving mean',
       x='', y='Mt/year') +
  theme_crea() + scale_color_crea_d('change', col.index = c(7,1,5)) -> plt
quicksave(file.path(output_dir, 'Power sector coal consumption in China.png'), plot=plt)

