base_date=ymd('2020-12-31')

plotdata %>% filter(source!='Thermal', variant=='Actual utilization') %>%
  (function(df) df %>% group_by(prov, date) %>% summarise(across(c(Value1m, Value_rollmean_12m), ~sum(.x, na.rm=T))) %>% mutate(source='Total generation') %>% bind_rows(df)) %>%
  mutate(Value=Value_rollmean_12m*12) ->
  plotdata_w_totals

pwr_data$monthly %>% filter(var=='Consumption') %>%
  group_by(year=year(date)) %>%
  summarise(Value_fullyear=Value[month(date)==12],
            Value_3to12m=Value_fullyear-Value[month(date)==2],
            ratio=Value_fullyear/Value_3to12m) %>%
  mutate(date=ymd(paste(year, 12, 31))) ->
  consumption_scaleups

power_demand %>% filter(month(date) %in% 3:12) %>%
  group_by(prov, date=ymd(paste(year(date), 12, 31))) %>%
  summarise(across(Value, sum)) %>%
  left_join(consumption_scaleups %>% select(date, ratio)) %>%
  mutate(Value=Value*ratio*1e4, source='Consumption') ->
  power_demand_yearly


bind_rows(plotdata_w_totals, power_demand_yearly) %>%
  (function(df) df %>% group_by(prov, date) %>% summarise(Value=Value[source=='Consumption']-Value[source=='Total generation']) %>% mutate(source='Net imports') %>% bind_rows(df)) %>%
  group_by(prov, source) %>%
  summarise(base_level=if_null(Value[date==base_date], 0),
            end_date_level=if_null(Value[date==last_month], 0),
            change=end_date_level-base_level) %>%
  ungroup %>%
  mutate(source=factor(source, levels=c('Solar', 'Wind', 'Nuclear', 'Bioenergy', 'Hydro', 'Fossil', 'Total generation', 'Net imports', 'Consumption'))) ->
  changes_perc

#install.packages("waterfalls")
library(waterfalls)

changes_perc %>% group_by(prov) %>%
  mutate(change_perc=change/base_level[source=='Total generation']) %>%
  ggplot(aes(source, change_perc)) + geom_col() + facet_wrap(~prov) + coord_flip() + scale_x_discrete(limits=rev)

gsub_factor <- function(p, r, x) { factor(gsub(p,r,x), levels=gsub(p,r,levels(x)))}

changes_perc %>%
  mutate(region=add_region(prov, split='East-West')) %>% #North-South
  group_by(region, source) %>% summarise(across(c(base_level, change), ~sum(.x, na.rm=T))) %>%
  mutate(change_perc=change/base_level[source=='Total generation']) %>%
  filter(!grepl('Total', source)) %>%
  mutate(start_level=c(0,lag(cumsum(change_perc))[-c(1,8)],0), end_level=c(cumsum(change_perc)[-8], change_perc[8])) %>%
  ungroup %>%
  select(region, source, change_perc, start_level, end_level) %>%
  write_csv(file.path(output_dir, 'waterfall by region.csv')) %>%
  mutate(source=droplevels(source) %>% gsub_factor(' ', '\n', .),
         id=as.numeric(source)) %>%
  ggplot(aes(source, xmin=id-.45, xmax=id+.45, ymin=start_level, ymax=end_level, fill=source)) + geom_rect() + facet_wrap(~region, scales='free_x') + #coord_flip() + #scale_x_discrete(limits=rev) +
  scale_fill_manual(values=c(fuel_cols, 'Net imports'=unname(crea_palettes$CREA['Dark.gray']), Consumption=unname(crea_palettes$CREA['Turquoise'])), guide=F) +
  scale_y_continuous(labels=scales::percent) +
  ggthemes::theme_tufte() +
  labs(title='Changes in power generation and consumption',
       subtitle='from 2020 to 2024, by region',
       x='')
quicksave(file.path(output_dir, 'waterfall by region.png'), logo=F, scale=.8)


