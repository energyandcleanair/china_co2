lang='ZH'

plot_formats =
  list(geom_vline(aes(xintercept=ymd('2020-02-10'), linetype=trans('First COVID-19 lockdown'))),
       x_at_zero(), theme_crea(legend.position='top'),
       scale_linetype_manual(values='dashed', name=''),
       scale_color_crea_d('dramatic', name=''),
       scale_y_continuous(labels=scales::comma))

d.quarter %>% filter(prod=='Total', grepl('predicted', name),
                     month(date) %in% (3*1:4),
                     year(date)>=2015) %>%
  #write_csv(file.path(output_dir, 'CO2 quarterly.csv')) %>%
  ggplot(aes(date-45, CO2_3m*3/100)) +
  geom_col(aes(fill=month(date) %in% 5:6)) +
  scale_x_date(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  theme_crea() +
  scale_fill_manual(values=unname(c(crea_palettes$CREA[2], crea_palettes$dramatic[1])), guide='none') +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y', expand=expansion(mult=c(.01,.01))) +
  labs(title=trans("China's CO2 emissions from energy and cement"),
       subtitle=trans("Quarterly"), y=trans('Mt CO2 / quarter'), x='') -> plt
quicksave(file.path(output_dir, 'CO2 quarterly ZH.png'), plot=plt, footer_height=.025)



d.quarter %>% replace_na(list(sector='All Sectors')) %>%
  filter(grepl('predicted', name),
         date>='2017-01-01',
         grepl('consumption', var, ignore.case=T) | prod=='Cement',
         grepl('Cement|Oil Products|Coal|Coking|Natural', prod),
         grepl('All|Power|Total', sector) | grepl('Coking', prod),
         !grepl('WIND', var)) ->
  energy_plot


energy_plot %<>% filter(prod=='Steam Coal') %>%
  group_by(prod, date, Unit, var) %>%
  summarise(across(is.numeric, function(x) x[sector=='Total']-x[sector=='Power Industry'])) %>%
  mutate(sector='Non-power use') %>%
  bind_rows(energy_plot) %>% filter(sector != 'Total')

energy_plot %>%
  ggplot(aes(date, CO2_12m*12/100)) + geom_line(size=1, col=crea_palettes$dramatic[2]) +
  facet_wrap(~trans(prod)+trans(sector), scales='free_y') +
  geom_smooth(data=energy_plot %>% filter(date<='2020-01-01', date>='2017-01-01'),
              aes(col=trans('pre-COVID trendline')),
              fullrange=T, method='lm') +
  plot_formats + snug_x_date +
  labs(title=trans('Trends in China CO2 by fuel and sector'),
       x='', y=trans('Mt/year, 12-month moving sum')) -> plt
quicksave(file.path(output_dir, 'Trends in China CO2 by fuel and sector ZH.png'), plot=plt, footer_height=.03)



col_index <- c(7,1,5)
if(lang=='ZH') col_index <- c(7,1,5)

coaluse_plot %>%
  filter(date>='2016-01-01', sector_coal %in% c('Power', 'Total')) %>%
  ggplot(aes(date, value_12m*12/10000, col=trans(name))) +
  facet_wrap(~trans(sector_coal), scales='free_y') +
  geom_line(size=1) +
  labs(title=trans('Coal consumption in China'), subtitle=trans('12-month moving sum'),
       x='', y=trans('Mt/year, 12-month moving sum')) +
  theme_crea(legend.position='top', legend.spacing.x = unit(0.95, 'cm')) +
  scale_color_crea_d('change', col.index = col_index, name='', guide=guide_legend(nrow=1)) +
  scale_x_date(labels=yearlab) -> plt
quicksave(file.path(output_dir, 'Power sector coal consumption in China ZH.png'), plot=plt, footer_height=.03)





in_file = get_data_file("monthly industry stats.xlsx")
readwindEN(in_file, c('var', 'prod'), columnExclude = 'Consumption', read_vardata = T) -> prod

prod %<>%
  group_by(var, prod, type) %>%
  group_modify(function(df, k) {
    message(k)
    df %>% unYTD() %>%
      filter(Value1m>0) %>% roll12m %>% seasonal(year_range = 2012:2019) })

prod$date %>% max -> latest_date
prod %>% group_by(prod) %>%
  filter(latest_date %in% date) ->
  prod_withlatest

prod_withlatest %<>%
  group_by(prod) %>%
  roll12m(months=3, incol='Value.seasonadj', outcol='Value3m.seasonadj') %>%
  mutate(YoY=get.yoy(Value3m.seasonadj, date))

prod_withlatest %>% filter(year(date)>=2017, grepl('Automob|Vehicle', prod)) -> plotdata1

plotdata1 %>%
  ggplot(aes(date, convert_value(Value12m, Unit)*12, col=prod))+
  geom_line(size=1.2)+geom_point(size=.8)+
  facet_wrap(~trans(prod), scales='free_y',ncol=1) +
  scale_color_crea_d('dramatic', guide=F) +
  labs(title=trans('Vehicle production'), subtitle='', x='', y=trans('million units, 12-month moving sum')) +
  theme_crea() + theme(strip.text = element_text(size=rel(1)), legend.position = 'top',
                       plot.title=element_text(size=rel(3))) +
  geom_vline(aes(linetype=trans('COVID-19 lockdown'), xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='') +
  scale_x_date(labels=yearlab) +
  expand_limits(y=0) + x_at_zero() -> p1

plotdata1 %>%
  group_by(prod) %>%
  roll12m(months=3, outcol='Value3m') %>%
  group_by(date) %>% summarise(share = Value3m[grepl('New Energy', prod)]/Value3m[grepl('Auto', prod)]) %>%
  mutate(prod='new sales, 3-month mean', Unit='percent') -> plotdata2

prod_withlatest %>% filter(grepl('Auto|New Energy', prod)) %>%
  (function(df) {
    df$cumulative_share <- as.numeric(NA)
    for(i in seq_along(df$date)) {
      end_date <- df$date[i]
      message(end_date)
      start_date <- end_date %>% 'day<-'(1) %>% 'year<-'(year(.)-10) %>% 'day<-'(days_in_month(.))
      df %>% filter(date>start_date, date<=end_date) %>% group_by(prod) %>% summarise(across(Value1m, sum, na.rm=T)) %>%
        summarise(share=Value1m[grepl('New Energy', prod)]/Value1m[grepl('Auto', prod)]) %>% unlist -> share
      df$share[df$date==end_date] <- share
    }
    return(df)
  }) %>% mutate(prod='cumulative sales over 10 years', Unit='percent') %>%
  bind_rows(plotdata2) ->
  plotdata2

plotdata2 %>%
  filter(year(date)>=2017) %>%
  ggplot(aes(date, share, col=trans(prod)))+
  geom_line(size=1.2)+geom_point(size=.8)+
  scale_color_crea_d('dramatic', col.index = c(3,6), guide=guide_legend(nrow=ifelse(lang=='EN',1,2))) +
  labs(y=trans('new energy vehicle share'), title=' ', subtitle=' ', x='', col='') +
  theme_crea() + theme(legend.position = 'top') +
  geom_vline(aes(linetype=trans('COVID-19 lockdown'), xintercept=ymd('2020-02-01')), size=1, alpha=.7) +
  scale_linetype_manual(values='dashed', name='', guide=F) +
  scale_y_continuous(labels = scales::percent, breaks=function(x) seq(0,x[2],.05),
                     expand=expansion(mult=c(0,.05))) +
  scale_x_date(labels=yearlab) +
  expand_limits(y=0) -> p2

plot_grid(p1,p2, nrow=1) -> g
quicksave(file.path(output_dir, paste0('Vehicle production, ',lang,'.png')), plot=g, footer_height=.03)
