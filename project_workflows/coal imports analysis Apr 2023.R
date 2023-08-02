readwindEN('data/fossil fuel imports.xlsx', c('var', 'prod'), read_vardata = T) %>% mutate(type='M') -> exim

require(ggrepel)
exim %<>% filter(grepl('Oil|Petroleum', prod)) %>% 
  mutate(Value=Value * ifelse(grepl('Export', var), -1, 1)) %>% 
  group_by(date) %>% summarise(across(Value, sum)) %>% 
  mutate(var='Net imports', prod='Oil (net imports)') %>% 
  bind_rows(exim)

exim %>% filter(year(date)>=2015,
                !grepl('Coking|Steam', prod),
                grepl('Coal|Gas|^Oil', prod),
                Value>0) %>% 
  filter(grepl('Import', var, ignore.case=T)) %>% 
  mutate(month=date %>% 'year<-'(2022) %>% 'day<-'(1), year=as.factor(year(date)),
         Value=Value/100,
         prod=prod %>% gsub(' \\(.*Customs.*', '', .)# %>% paste0(' (', Unit, ')')
         ) -> plotdata

plotdata %>% group_by(year, prod) %>% filter(Value==max(Value)) -> labdata

plotdata %>% 
  ggplot(aes(month, Value, col=year)) + 
  facet_wrap(~prod, scales = 'free_y', ncol=2) + 
  geom_line(linewidth=1) +
  geom_label_repel(data=labdata, aes(label=year)) +
  scale_x_date(date_labels = '%b', expand=expansion(mult=c(.01,.01))) +
  x_at_zero() +
  theme_crea() +
  labs(title="China's fossil fuel imports", x='', y='Mt/month') +
  ggsci::scale_color_futurama(guide='none') -> plt
quicksave(file.path(output_dir, "China's fossil fuel imports.png"), plot=plt)

plotdata %>% filter(grepl('Coal', prod)) %>% 
  ggplot(aes(month, Value, col=year)) + 
  geom_line(linewidth=1) +
  geom_label_repel(data=labdata %>% filter(grepl('Coal', prod)), 
                   aes(label=year)) +
  scale_x_date(date_labels = '%b', expand=expansion(mult=c(.01,.01))) +
  x_at_zero() +
  theme_crea() +
  labs(title="China's monthly coal and lignite imports", x='', y='Mt/month') +
  ggsci::scale_color_futurama(guide='none') -> plt
quicksave(file.path(output_dir, "China's coal imports.png"), plot=plt)

read_xlsx('data/China monthly coal imports Kpler.xlsx') %>% pivot_longer(-date, names_to='origin_country') %>% 
  mutate(date = ymd(paste0(date, '-01'))) -> kpler

kpler %>% filter(year(date)==2023) %>% group_by(origin_country) %>% summarise(across(value, sum)) %>% arrange(desc(value)) ->
  country_ranking

kpler %>% filter(date<'2023-05-01') %>% 
  mutate(origin_country = case_when(origin_country %in% country_ranking$origin_country[1:6]~origin_country, T~'Others'),
         origin_country = factor(origin_country, levels=c(country_ranking$origin_country[1:6], 'Others') %>% rev)) %>%
  group_by(origin_country, date) %>% summarise(across(value, sum)) %>% 
  ggplot(aes(date, value/1e6, fill=origin_country)) + geom_area(position='fill') +
  theme_crea() + snug_x_date + 
  scale_y_continuous(expand=c(0,0), label=scales::percent) +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[c(1,2,3,4,6,7)], crea_palettes$dramatic[1]))) +
  labs(title='Seaborne coal shipments to Chinese ports', y='Mt/month', x='', fill='Origin') -> plt
quicksave(file.path(output_dir, "Seaborne coal shipments mix.png"), plot=plt)

kpler %>% filter(date<'2023-05-01') %>% 
  mutate(origin_country = case_when(origin_country=='China'~'Domestic', T~'Imported')) %>%
  group_by(origin_country, date) %>% summarise(across(value, sum)) %>% 
  ggplot(aes(date, value/1e6, fill=origin_country)) + geom_area() +
  theme_crea() + snug_x_date + x_at_zero() +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[c(1,2,3,4,6,7)], crea_palettes$dramatic[1]))) +
  labs(title='Seaborne coal shipments to Chinese ports', y='Mt/month', x='', fill='Origin') -> plt
quicksave(file.path(output_dir, "Seaborne coal shipments total imports vs domestic.png"), plot=plt)


readwindEN('data/coal prices 3500 vs 5500.xlsx', c('var', 'prod', 'port', 'V4'), zero_as_NA = T, read_vardata = T) -> prices

prices %>% 
  mutate(kcal = Name %>% str_match('Q[0-9]{4}') %>% force_numeric() %>% as.factor) %>% 
  filter(year(date)>=2019, kcal!=3800) %>% 
  group_by(date, kcal) %>% 
  summarise(across(Value, mean, na.rm=T)) %>% 
  group_by(kcal) %>% 
  mutate(index=Value/Value[date==min(date)]*100) %>% 
  ggplot(aes(date, Value, col=kcal)) + geom_line(size=1.5) + x_at_zero() +
  theme_crea() + scale_color_crea_d('change', col.index = c(2,5,7)) +
  labs(title='Coal prices in Chinese ports by calorific value',
       color='kcal/kg', y='yuan/tonne', x='') -> plt #+
  #scale_x_date(labels = function(x) paste0(year(x), '年'))
quicksave(file.path(output_dir, "Coal prices in Chinese ports by calorific value.png"), plot=plt)






#chinese graphs
zh_theme <- list(theme(text=element_text(family='Source Sans'),
                       plot.title = element_text(size=rel(2), margin=margin(c(12,12,12,12)))))

plotdata %>% filter(grepl('Coal', prod)) %>% 
  ggplot(aes(month, Value*100, col=year)) + 
  geom_line(linewidth=1) +
  geom_label_repel(data=labdata %>% filter(grepl('Coal', prod)), 
                   aes(label=yearlab(year))) +
  scale_x_date(labels = monthlab, expand=expansion(mult=c(.01,.01))) +
  x_at_zero() +
  theme_crea() + zh_theme +
  labs(title="中国煤及褐煤进口情况", x='', y='万吨/月') +
  ggsci::scale_color_futurama(guide='none') -> plt
quicksave(file.path(output_dir, "China's coal imports ZH.png"), plot=plt, scale=1)

kpler %>% filter(date<'2023-05-01') %>% 
  mutate(origin_country = case_when(origin_country %in% country_ranking$origin_country[1:6]~origin_country, T~'Others'),
         origin_country = factor(origin_country, levels=c(country_ranking$origin_country[1:6], 'Others') %>% rev)) %>%
  group_by(origin_country, date) %>% summarise(across(value, sum)) %>% ungroup %>% 
  mutate(origin_country = recode_factor(origin_country,
                                        'Indonesia'='印度尼西亚',
                                        'Russian Federation'='俄罗斯',
                                        'Australia'='澳大利亚',
                                        'Canada'='加拿大',
                                        'United States'='美国',
                                        'Others'='其他',
                                        'China'='中国',)) %>% 
  ggplot(aes(date, value/1e6*100, fill=origin_country)) + geom_area(position='fill') +
  theme_crea() + zh_theme +
  scale_x_date(expand=expansion(mult=c(0,0)), label=yearlab) +
  scale_y_continuous(expand=c(0,0), label=scales::percent) +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[c(1,2,3,4,6,7)], crea_palettes$dramatic[1]))) +
  labs(title='中国港口接收的海运煤炭产地分布', y='万吨/月', x='', fill='产地') -> plt
quicksave(file.path(output_dir, "Seaborne coal shipments mix ZH.png"), plot=plt, scale=1)

prices %>% 
  mutate(kcal = Name %>% str_match('Q[0-9]{4}') %>% force_numeric() %>% as.factor) %>% 
  filter(year(date)>=2019, kcal!=3800) %>% 
  group_by(date, kcal) %>% 
  summarise(across(Value, mean, na.rm=T)) %>% 
  group_by(kcal) %>% 
  mutate(index=Value/Value[date==min(date)]*100) %>% 
  ggplot(aes(date, Value, col=kcal)) + geom_line(size=1.5) + x_at_zero() +
  theme_crea() + zh_theme +
  scale_color_crea_d('change', col.index = c(2,5,7)) +
  scale_x_date(labels = yearlab) +
  labs(title='不同热值煤的中国港口煤价走势',
       color='千卡/千克', y='元/吨', x='') -> plt #+
#scale_x_date(labels = function(x) paste0(year(x), '年'))
quicksave(file.path(output_dir, "Coal prices in Chinese ports by calorific value ZH.png"), plot=plt, scale=1)
