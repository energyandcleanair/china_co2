source('load_package.R')

output_dir='outputs'

cap <- readwindEN(get_data_file('Power Capacity.xlsx'), c('var', 'source'), columnExclude = 'New|Conventional|Coal|Gas',
           zero_as_NA = T, read_vardata = T) %>% filter(source!='YTD') %>%
  mutate(GW=Value/100)

gen_cons <- read_csv(get_data_file('monthly_full_release_long_format-4.csv')) %>%
  set_names(tolower(make.names(names(.)))) %>% rename(source=variable, TWh=value) %>%
  filter(country.code=='CHN', unit=='TWh')

#projected 2023 capacity: Wind, Solar, Hydro, Nuclear 43000, 53000, 42000, 5846
#https://www.cec.org.cn/detail/index.html?3-323217
#projected solar additions: 210 GW
#https://www.bloomberg.com/news/articles/2023-09-06/china-s-breakneck-solar-expansion-starts-to-meet-power-grid-resistance
tibble(source=c('Wind', 'Solar', 'Hydro', 'Nuclear'),
       date=ymd('2023-12-01'),
       GW=c(43000, 53000, 42000, 5846)/100) -> cap_fut

cap_fut$GW[cap_fut$source=='Solar'] <- cap$GW[cap$date=='2022-12-31' & grepl('Solar', cap$source)] + 210

dates <- seq.Date(min(gen_cons$date), ymd('2025-12-01'), by='month')


cap %>% mutate(source = disambiguate(source, unique(gen_cons$source)),
               date = date %>% 'day<-'(1)) %>%
  right_join(gen_cons) %>%
  group_by(source) %>% filter(any(!is.na(GW))) %>% ungroup %>%
  bind_rows(cap_fut) %>%
  complete(date=dates, source) %>%
  group_by(source) %>%
  mutate(annual_add_GW=case_when(source=='Nuclear'~(70-GW[date=='2023-12-01'])/2, T~GW[date=='2023-12-01']-GW[date=='2022-12-01'])) %>%
  group_by(source, month(date)) %>%
  mutate(GW = case_when(date>='2024-01-01'~GW[year(date)==2023]+annual_add_GW*(year(date)-2023), T~GW)) %>%
  group_by(source) %>% arrange(date) %>%
  mutate(GW = na.approx(GW, date, na.rm=F)) %>%
  bind_rows(gen_cons %>% filter(source=='Demand') %>% complete(date=dates, source)) -> gen_cons

gen_cons %>% ggplot(aes(date, GW, col=source)) + geom_line()

gen_cons %>% filter(source=='Demand') -> cons
cons %>% lm(TWh~date*as.factor(month(date)), data=.) -> m
cons %<>% mutate(TWh = na.cover(TWh, predict(m, cons)))
cons %>% ggplot(aes(date, TWh)) + geom_line()

gen_cons %<>% filter(source!='Demand') %>% bind_rows(cons)

gen_cons %<>% mutate(utilization = TWh/(GW*days_in_month(date)*24/1000), month=month(date))

gen_cons %<>% filter(grepl('Hydro|Nuclear|Solar|Wind', source), year(date)<=2021) %>%
  group_by(source, month) %>% summarise(mean_utilization=mean(utilization, na.rm=T)) %>%
  left_join(gen_cons, .) %>%
  mutate(TWh=na.cover(TWh, mean_utilization * (GW*days_in_month(date)*24/1000)))

gen_cons %<>% group_by(date) %>% summarise(TWh = TWh[source=='Demand']-sum(TWh[source!='Demand'])) %>%
  mutate(source='Thermal') %>% bind_rows(gen_cons)

gen_cons %<>% mutate(TWh_per_day=TWh/days_in_month(date)) %>%
  group_by(source) %>% roll12m(incol='TWh_per_day', outcol='TWh_per_day_12m') %>%
  mutate(YoY=get_yoy(TWh_per_day, date), YoY_12m=get_yoy(TWh_per_day_12m, date),
         data_type=ifelse(date>'2023-08-01', 'projection', 'actual'))


gen_cons %>%
  rename('monthly'=TWh_per_day, '12-month rolling mean'=TWh_per_day_12m) %>%
  pivot_longer(c('monthly', '12-month rolling mean')) %>%
  ggplot(aes(date, value, col=source, alpha=name, linetype=data_type)) +
  geom_line(linewidth=1) +scale_alpha_manual(values=c(1,.3)) +
  labs(title="China's projected power generation",
       subtitle="Assuming solar, wind & hydro capacity growth in 2024-25 equal to 2023; 2025 nuclear target met; historic average capacity utilization and demand growth",
       linetype='', y='TWh/day', x='') +
  theme_crea() + scale_color_crea_d('CREA', col.index=c(12,2,9,11,10,5)) + x_at_zero() + snug_x_date

gen_cons %>% filter(year(date)>=2018) %>%
  ggplot(aes(date+15, YoY, fill=source, alpha=data_type)) + facet_wrap(~source, scales='free', ncol=1)+ geom_col() +
  labs(title="China's projected power generation growth",
       subtitle="Assuming non-fossil capacity growth in 2024-25 equal to 2023, historic average capacity utilization and demand growth at trend",
       alpha='', y='monthly year-on-year growth', x='') +
  scale_alpha_manual(values=c(1,.5)) +
  theme_crea() + scale_fill_crea_d('CREA', col.index=c(12,2,9,11,10,5)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(expand=expansion(), date_breaks = '1 year', date_labels = '%Y') -> plt
quicksave(file.path(output_dir, 'projected power generation growth.png'), plot=plt)

gen_cons %>% select(source, date, TWh, TWh_per_day_12m, YoY, YoY_12m) %>%
  write_csv(file.path(output_dir, 'projected power generation growth.csv')) %>%
  filter(month(date)==12, year(date)>=2023)


gen_cons %>% filter(year(date) %in% 2022:2023, month(date)==12) %>%
  group_by(source) %>%
  summarise(GW=GW[2]-GW[1]) -> gw_adds

gen_cons %>% filter(year(date)==2023) %>% group_by(source) %>%
  summarise(mean_utilization=weighted.mean(mean_utilization, days_in_month(date))) -> mean_cf

full_join(gw_adds, mean_cf) %>% mutate(TWh=GW*8.76*mean_utilization) %>% na.omit %>%
  arrange(desc(TWh)) %>%
  write_csv(file.path(output_dir, 'clean power additions.csv'))





gen_cons %>%
  rename('monthly'=TWh_per_day, '12-month rolling mean'=TWh_per_day_12m) %>%
  pivot_longer(c('monthly', '12-month rolling mean')) %>%
  filter(grepl('12-month', name), source!='Demand') %>%
  mutate(source=factor(source, levels=c('Thermal', 'Hydro', 'Nuclear','Wind',  'Solar') %>% rev)) ->
  gen_cons_plot

gen_cons_plot %>%
  ggplot(aes(date, value, fill=source)) + #, alpha=data_type
  geom_area() +scale_alpha_manual(values=c(1,.3)) +
  labs(title="China's projected power generation",
       subtitle='12-month moving mean',
       caption="Assuming solar, wind & hydro capacity growth in 2024-25 equal to 2023; 2025 nuclear target met; historic average capacity utilization and demand growth",
       linetype='', y='TWh/day', x='') +
  theme_crea() + scale_fill_crea_d('CREA', col.index=c(11,5,9,2,12)) + x_at_zero() + snug_x_date -> p
quicksave(file.path(output_dir, 'projected power generation.png'), plot=p)

gen_cons_plot %>% select(source, variable=name, date, value) %>% na.omit %>%
  mutate(name=ifelse(date<=last_month, 'historical', 'projected')) %>%
  write_csv(file.path(output_dir, 'projected power generation.csv'))

source('project_workflows/2023Q3 CO2 from wind.R')

output_dir='outputs'


alignment_period <- d.quarter$date %>% unique %>% subset(.<=last_month & .>last_month-365)

d.quarter %>%
  filter(grepl('predicted', name), is.na(sector)|sector!='Total', prod!='Total') %>%
  mutate(sector=na.cover(sector, 'Total')) %>%
  group_by(sector, prod) %>%
  group_modify(function(df, group) {
    df %<>% complete(date=seq.Date(df$date %>% min() %>% 'day<-'(1), ymd('2026-01-01'), by='month')-1)
    if(group$sector=='Power Industry') {
      gen_cons %>% ungroup %>% mutate(date=date %>% 'day<-'(days_in_month(date))) %>%
        filter(source=='Thermal') %>%
        select(date, TWh_per_day) %>%
        left_join(df, .) %>%
        mutate(intensity_reduction=.99^((as.numeric(date-ymd('2022-06-30')))/365),
               CO2 = na.cover(CO2, TWh_per_day * mean(CO2[date %in% alignment_period])/mean(TWh_per_day[date %in% alignment_period])*intensity_reduction)) ->
        df
    } else {
      start_year = case_when(grepl('Other', group$sector)~2018, grepl('Heating', group$sector)~2021, grepl('Coking', group$prod)~2014, grepl('Coal', group$prod)~2015, grepl('Gas', group$prod)~2022, T~2021)
      lm(CO2~date+month(date), data=df %>% filter(year(date)>=start_year)) -> m
      CO2_pred <- predict(m, df)
      df %<>% mutate(CO2=na.cover(CO2, CO2_pred * mean(CO2[date %in% alignment_period])/mean(CO2_pred[date %in% alignment_period])))
    }
    return(df)
  }) -> d.pred

d.pred %<>% group_by(sector, prod) %>% roll12m(incol = 'CO2', outcol='CO2_12m') %>% roll12m(3, incol = 'CO2', outcol='CO2_3m') %>% filter(year(date)>=2017)

d.pred %>% ggplot(aes(date, CO2_12m)) + facet_wrap(~sector+prod, scales='free_y') + geom_line()


bind_rows(d.pred %>% filter(date<=last_month) %>% mutate(name='historical'),
          d.pred %>% filter(date>=last_month) %>% mutate(name='projected')) %>%
  ggplot(aes(date, CO2_12m*12/1e3, fill=paste0(prod, ': ', sector), alpha=name)) + geom_area() +
  geom_vline(aes(xintercept=last_month), linetype='dashed', linewidth=.75) +
  scale_alpha_manual(values=c(1,.75), name='') +
  scale_y_continuous(limits=c(0,13), expand = expansion()) + snug_x_date +
  scale_fill_crea_d() +
  labs(y='Gt/year', x='', fill='source', title="China's CO2 emissions: past and projected", subtitle='12-month moving sum',
       caption='Assuming solar, wind & hydro capacity growth in 2024-25 equal to 2023; 2025 nuclear target met; historic average capacity utilization and demand growth;\nnon-power sector emissions follow historical trend') +
  theme_crea() -> p

quicksave(file.path(output_dir, 'Chinas CO2 emissions past and projected.png'), plot=p)

d.pred %>% mutate(name=ifelse(date<=last_month, 'historical', 'projected'), CO2_12m=CO2_12m*12/1e3) %>%
  select(sector, product=prod, date, name, CO2_12m) %>%
  write_csv(file.path(output_dir, 'Chinas CO2 emissions past and projected.csv'))


d.pred %<>% group_by(date) %>% summarise(across(starts_with('CO2'), sum)) %>%
  mutate(sector='Total', prod='Total') %>% bind_rows(d.pred)

d.pred %>%
  mutate(sector=case_when(grepl('Coal', prod) & grepl('Power', sector)~'Coal: power sector',
                          grepl('Coal', prod)~'Coal: other sectors',
                          T~prod)) %>%
  group_by(sector, date) %>% summarise(across(starts_with('CO2'), sum)) -> d.agg

d.agg %>% group_by(sector) %>%
  mutate(YoY=get_yoy(CO2, date),
         YoY_3m=get_yoy(CO2_3m, date),
         YoY_12m=get_yoy(CO2_12m, date)) %>%
  filter(month(date) %in% seq(3,12,3)) %>%
  select(sector, date, YoY_3m, YoY_12m) %>% na.omit %>%
  mutate(name=ifelse(date<=last_month, 'historical', 'projected')) %>%
  write_csv(file.path(output_dir, 'Chinas CO2 emissions quarterly growth past and projected.csv')) ->
  d.plot

for(yesno in c(F, T)) {
  d.plot %>% filter((sector=='Total') == yesno) %>%
    ggplot(aes(date-45, YoY_3m, fill=YoY_3m)) + geom_col() + facet_wrap(~sector, scales='free_y') +
    scale_y_continuous(labels = scales::percent) +
    theme_crea() + scale_fill_crea_c('change', guide='none', col.index = c(1:2,6:7)) +
    labs(title="China's CO2 emissions: quarterly growth", y='', x='') +
    scale_x_date(expand=expansion(mult=.02)) +
    geom_vline(aes(xintercept=last_month), linetype='dashed', linewidth=.75) -> p
  quicksave(file.path(output_dir, paste0('Chinas CO2 emissions quarterly growth past and projected, ',
                                         ifelse(yesno, 'total', 'by sector'),
                                         '.png')), plot=p, footer_height=.03)
}


for(yesno in c(F, T)) {
  d.plot %>% filter((sector=='Total') == yesno, month(date)==12) %>%
    ggplot(aes(date-365, YoY_12m, fill=YoY_12m)) + geom_col() + facet_wrap(~sector, scales='free_y') +
    scale_y_continuous(labels = scales::percent) +
    theme_crea() + scale_fill_crea_c('change', guide='none', col.index = c(1:2,6:7)) +
    labs(title="China's CO2 emissions: annual growth", y='', x='') +
    scale_x_date(expand=expansion(mult=.02)) +
    geom_vline(aes(xintercept=ymd(paste(year(last_month),1,1))-180), linetype='dashed', linewidth=.75) -> p
  quicksave(file.path(output_dir, paste0('Chinas CO2 emissions annual growth past and projected, ',
                                         ifelse(yesno, 'total', 'by sector'),
                                         '.png')), plot=p, footer_height=.03)
}
