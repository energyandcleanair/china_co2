library(directlabels)

in_file = get_data_file("Power Capacity.xlsx")
readwindEN(in_file, c('var', 'source', 'fuel', 'YTD'), read_vardata = T) %>%
  mutate(var = ifelse(grepl('New', var), 'Newly added capacity', 'Installed capacity'),
         source = source %>% gsub(' ?Power.*', '', ., ignore.case=T) %>% gsub('YTD', 'All', .),
         fuel = ifelse(is.na(fuel) | fuel %in% c('YTD', 'National'), 'All', fuel)) -> cap

cap %>%
  filter(year(date)>=2018, fuel=='All', grepl('New', var)) %>%
  group_by(source, fuel, year=as.factor(year(date))) %>%
  group_modify(function(df, groups) {
    df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>%
      bind_rows(df)
  }) -> plotdata

plotdata %<>%
  group_by(source, fuel) %>%
  mutate(YoY=get.yoy(Value, date) %>% scales::percent(accuracy = 1, style_positive='plus'),
         Value=convert_value(Value, '10MW', lang=lang),
         plotdate=date %>% 'year<-'(2022),
         source=ifelse(fuel=='All', source, fuel))

plotdata %>% filter(date == max(date)) -> yoy_labels

fuel_cols = crea_palettes$CREA[c(1, 4, 2, 6, 5)]
names(fuel_cols) = cap$source %>% unique %>% subset(.!='All') %>% sort #%>% translateSources()
cap$date %>% year %>% max %>% seq(2010, ., 1) -> yrs
cap$date %>% max %>% month -> ytd_month

cap %>% filter(fuel=='All', grepl('Wind|Solar', source), month(date)==month(ytd_month), grepl('New', var), year(date) %in% yrs) %>%
  ggplot(aes(year(date), convert_value(Value, '10MW'), fill=source, alpha=year(date))) +
  geom_col(size=1) + facet_wrap(~translateSources(source, lang=lang), ncol=2) +
  geom_label(data=yoy_labels %>% filter(grepl('Wind|Solar', source)),
             aes(label=YoY), vjust=-2, hjust=.65, fill='white') +
  labs(y=unit_label('10MW', lang=lang), x='', title=ifelse(lang=='EN',
                                                           paste0('Newly added power capacity, January to ', month.name[ytd_month]),
                                                           paste0('新增发电装机容量，前', ytd_month,'个月累计值'))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  scale_x_continuous(labels=yearlab, breaks=yrs) +
  theme_crea(axis.text.x=element_text(angle=35, hjust=1)) +
  lang_theme(lang=lang) +
  scale_fill_manual(values=unname(fuel_cols), guide='none') +
  scale_alpha(range=c(.5,1), guide='none') -> plt
quicksave(file.path(output_dir, paste0('Newly added power capacity, YTD, wind&solar, ',lang,'.png')), plot=plt, scale=1,
          footer_height=.03)


tribble(~source,~g.kWh,
        'Gas',55*3.6/.55,
        'Solar PV, manufactured in China',58,
        'Solar PV, manufactured in the EU',11) %>%
  ggplot(aes(source, g.kWh, fill=source)) + geom_col() + coord_flip() +
  theme_crea() +
  labs(title='CO2 intensity of power generation', x='', y='g/kWh') +
  x_at_zero() + scale_fill_crea_d('change', col.index=c(6,5,1), guide='none') -> plt
quicksave(file.path('outputs', 'CO2 intensity of Chinese PV.png'), plot=plt, scale=.85, footer_height=.05, logo_height=.05, logo_hjust=1)
