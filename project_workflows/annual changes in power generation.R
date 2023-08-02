read_delim('data/emberChartData.csv', skip=1, delim=';') -> gen_ember

gen_ember %<>% filter(grepl("Bio|Coal|Gas|Hydro|Nuclear|Other Fossil|Wind|Solar", variable)) %>% 
  mutate(variable = case_when(grepl('Gas|Other Fossil', variable)~"Gas and other fossil", T~variable)) %>% 
  group_by(country_or_region, date, variable) %>% summarise_all(sum)

crea_cols = crea_palettes %>% unname %>% unlist %>% subset(!duplicated(.))
prodcols = list(Wind=crea_cols['Light.blue'],
                Nuclear=crea_cols['Orange'],
                Solar=crea_cols['Red'],
                Bioenergy=crea_cols['Turquoise'],
                Hydro=crea_cols['Dark.blue'],
                'Gas and other fossil'=crea_cols['Light.gray'],
                Coal=crea_cols['Dark.gray'])

gen_ember %>% 
  group_by(country_or_region, variable) %>% arrange(date) %>% 
  mutate(generation_twh_12m=rollapplyr(generation_twh, 12, mean, na.rm=T, fill=NA),
         change=(generation_twh_12m-lag(generation_twh_12m,12))*12) %>% 
  mutate(variable=variable %>% factor(levels=names(prodcols) %>% rev),
         year=as.factor(year(date))) %>% 
  filter(month(date)==12 | date==max(date), year(date)>=2016) -> plotdata_cn

plotdata_cn %>% filter(year(date) %in% 2016:2019) %>% 
  group_by(year) %>% summarise(across(change, sum)) %>% 
  ungroup %>% summarise(across(change, mean)) -> pre.COVID.trend

plotdata_cn %>% filter(!grepl('Coal|Fossil', variable)) %>% 
  group_by(year) %>% summarise(across(change, sum)) -> non.fossil.trend


plotdata_cn %>% bind_cols(pre.COVID.trend %>% rename(pre.COVID.trend=change)) %>% 
  #write_csv('Annual changes in power generation in China.csv') %>% 
  ggplot(aes(year, change)) + geom_col(aes(fill=variable)) + 
  #geom_hline(size=1.5,color=crea_cols['Green'],
  #           mapping=aes(yintercept=pre.COVID.trend,
  #                       linetype='pre-COVID demand growth\n(2016-2019 average)')) +
  theme_crea() + 
  scale_fill_manual(values=prodcols) + 
  scale_linetype_manual(values='dashed') +
  labs(title='Annual changes in power generation in China',
       subtitle='change compared with the preceding year',
       x='', y='TWh', linetype='') -> plt
quicksave(file.path(output_dir, 'Annual changes in power generation in China.png'), plot=plt)

plotdata_cn %>% 
  ggplot(aes(date, generation_twh_12m, col=variable)) + geom_line(size=1) + 
  #facet_wrap(~Country, scales='free') +
  geom_smooth(data=plotdata_cn %>% filter(year(date)<=2019), method='lm', aes(linetype='pre-COVID trend'),
              fullrange=T, se=F) +
  theme_crea() + 
  #scale_color_crea_d('dramatic') + 
  scale_linetype_manual(values='dotted', name='') +
  expand_limits(y=0) +
  labs(title='Trends in fossil power generation',
       subtitle='12-month moving sum',
       x='', y='TWh')



