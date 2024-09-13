source('R/wind mapping functions.R')

output_dir = 'outputs/monthly_snapshot'; dir.create(output_dir)

in_file = "data/Power Capacity.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'fuel', 'YTD'), read_vardata = T) %>%
  mutate(var = ifelse(grepl('New', var), 'Newly added capacity', 'Installed capacity'),
         source = source %>% gsub(' ?Power.*', '', ., ignore.case=T) %>% gsub('YTD', 'All', .),
         fuel = ifelse(is.na(fuel) | fuel %in% c('YTD', 'National'), 'All', fuel)) -> cap

cap %>% distinct(var, source, fuel, YTD)

lang='EN'


library(directlabels)
cap %>%
  filter(year(date)>=2018, fuel=='All', grepl('New', var)) %>%
  group_by(source, fuel, year=as.factor(year(date))) %>%
  group_modify(function(df, groups) {
    df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>%
      bind_rows(df)
  }) %>%
  mutate(plotdate=date %>% 'year<-'(2022),
               source=ifelse(fuel=='All', source, fuel)) %>%
  write_csv(file.path(output_dir, 'newly added power capacity.csv')) %>%
  ggplot(aes(plotdate, Value/100, col=year)) + geom_line(size=1) +
  facet_wrap(~translateSources(source), ncol=2, scales='free_y') +
  labs(y='GW', x='', title='Newly added power capacity, year-to-date') +
  scale_x_date(date_breaks = '3 months', labels = monthlab, minor_breaks = 'month',
               expand = expansion(mult=c(.0,.17))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  theme_crea(axis.text.x=element_text(hjust=.2)) +
  lang_theme(lang=lang) +
  scale_color_crea_d(col.index = c(7, 2:5, 1), labels=yearlab, guide='none') +
  geom_dl(aes(label=yearlab(year)), method=list('last.bumpup', cex=.7)) -> plt
quicksave(file.path(output_dir, 'Newly added power capacity, year-to-date.png'),
          plot=plt,
          scale=1.2)

fuel_cols = crea_palettes$CREA[c(1, 4, 2, 6, 5)]
names(fuel_cols) = cap$source %>% unique %>% subset(.!='All') %>% sort
cap$date %>% year %>% max %>% seq(2010, ., 1) -> yrs
cap$date %>% max %>% month -> ytd_month

cap %>% filter(fuel=='All', month(date)==month(max(date)), grepl('New', var), year(date) %in% yrs) %>%
  write_csv(file.path(output_dir, 'Newly added power capacity, YTD.csv')) %>%
  ggplot(aes(year(date), Value/100, fill=source, alpha=year(date))) +
  geom_col(size=1) + facet_wrap(~translateSources(source), ncol=2, scales='free') +
  labs(y='GW', x='', title=paste0('Newly added power capacity, January to ', month.name[ytd_month])) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  scale_x_continuous(labels=yearlab, breaks=yrs) +
  theme_crea(axis.text.x=element_text(angle=25, hjust=1)) +
  lang_theme(lang=lang) +
  scale_fill_manual(values=unname(fuel_cols), guide='none') +
  scale_alpha(range=c(.5,1), guide='none') -> plt
quicksave(file.path(output_dir, paste0('Newly added power capacity, YTD, ',lang,'.png')), plot=plt, scale=1.33)

in_file = get_data_file("New power capacity by province and type.xlsx")
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T) -> provcap

pwr_sources <- c('Thermal', 'Wind', 'Solar', 'Nuclear', 'Hydro')
provs <- read_xlsx(get_data_file('provincesZH.xlsx'))
provcap$prov %>% unique %>% subset(!grepl(paste(pwr_sources, collapse='|'), .))
provcap %<>% mutate(prov = disambiguate(Name, c(provs$Province, 'National')), source=disambiguate(Name, pwr_sources))

ytd_date = max(provcap$date)
period_name = c(ytd_date %>% 'month<-'(1), max(provcap$date)) %>% format('%B %Y') %>% paste(collapse=' - ')

provcap %>%
  filter(Value>0, date %in% dates_to_include, prov != 'National') %>%
  mutate(source = source %>% gsub(' Energy| Power|power', '', .)) %>%
  group_by(source) %>%
  group_by(source, prov) %>% summarise(across(Value, sum)) %>%
  slice_max(Value, n=10) %>%
  group_split %>%
  lapply(function(df) {
    df %>%
      mutate(prov=translateProvinces(prov)) %>%
      mutate(prov = factor(prov, levels=rev(prov))) %>%
      ggplot(aes(prov, Value/1e2)) +
      geom_col(fill=fuel_cols[unique(df$source)]) +
      facet_wrap(~translateSources(source)) +
      coord_flip() +
      theme_crea() +
      lang_theme(lang=lang) +
      theme(#strip.text = element_text(size=rel(2)),
            #axis.text = element_text(size=rel(1.8)),
            #axis.title = element_text(size=rel(2)),
            plot.margin = unit(c(.5, 1.5, .2, .2), 'line')) +
      labs(y='GW', x='') +
      scale_y_continuous(expand=expansion(mult=c(0,.05)))
  }) -> p

library(cowplot)
require(gridExtra)
require(grid)
title.grob = textGrob("Newly installed power capacity by province",
                      gp=gpar(fontface='bold', cex=2, col="#35416C"),
                      just=.5)
subtitle.grob = textGrob(period_name,
                         gp=gpar(fontface='italic',cex=1.33),
                         just=.5)
p.grid = plot_grid(plotlist=p,ncol=2,align="v")
margin <- unit(1, "line")

grid.arrange(title.grob, subtitle.grob, p.grid,
             heights = unit.c(grobHeight(title.grob) + 2*margin,
                              grobHeight(subtitle.grob) + .5*margin,
                              unit(1,"null"))) -> plt
quicksave(file.path(output_dir, paste0('power capacity additions by province, ', period_name, ', ',lang,'.png')), plot=plt)



focusmonth=provcap$date %>% max %>% month
cap %>% filter(month(date) %in% c(focusmonth, 12), grepl('Wind|Solar|Thermal', source), fuel=='All', grepl('New', var)) %>%
  mutate(month=ifelse(month(date)==12, 'whole_year', 'focus_month'), year=year(date), Value=Value/100) %>%
  select(year, month, Value, source) %>%
  spread(month, Value) -> scatterplotdata

provcap %>% filter(grepl('Wind|Solar', source), date==max(date)) %>%
  group_by(prov) %>% summarise(across(Value, sum)) %>% arrange(desc(Value)) -> prov_ranking


require(ggrepel)
scatterplotdata %>%
  filter(focus_month>=1) %>%
  ggplot(aes(focus_month, whole_year)) +
  geom_smooth(formula=y~x, method='lm', fullrange=T, color=crea_palettes$CREA[4]) +
  geom_point() +
  geom_text_repel(aes(label=yearlab(year))) +
  facet_wrap(~translateSources(source), scales='free') +
  geom_vline(data=scatterplotdata %>% filter(year==2023),
             aes(xintercept = focus_month), size=1, linetype='dotted', color=crea_palettes$dramatic[1]) +
  geom_text_repel(data=scatterplotdata %>% filter(year==2023),
                  aes(label=yearlab(year), y=focus_month),
                  color=crea_palettes$dramatic[1]) +
  geom_vline(data=scatterplotdata %>% filter(year==2023),
             aes(xintercept = focus_month*1.05), color=NA) +
  scale_x_continuous(expand=expansion(mult=c(0,.05))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  coord_cartesian(ylim=c(0,NA)) + expand_limits(x=0) +
  theme_crea() +
  lang_theme(lang=lang) +
  labs(title=ifelse(lang=='EN', 'Capacity installations in January-November vs. full year',
                    '各年前几个月与全年新增发电容量对比'),
       x=ifelse(lang=='EN', paste0('Capacity added, GW, Jan-', month.abb[ytd_month]), paste0('新增发电容量，前',ytd_month,'个月')),
       y=ifelse(lang=='EN', 'Capacity added, GW, full year', '新增发电容量，全年')) -> plt
quicksave(file.path(output_dir, paste0('YTD vs full year wind&solar, ',lang,'.png')), plot=plt, footer_height=.03)


provcap %>% filter(grepl('Wind|Solar', source)) %>%
  filter(year(date)>=2018, prov %in% prov_ranking$prov[2:7]) %>%
  group_by(source, prov, year=as.factor(year(date))) %>%
  group_modify(function(df, groups) {
    df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>%
      bind_rows(df)
  }) %>%
  mutate(plotdate=date %>% 'year<-'(2022), source=gsub(' Energy', '', source)) %>%
  ggplot(aes(plotdate, Value/100, col=yearlab(year))) + geom_line(size=1) +
  facet_grid(translateProvinces(prov)~translateSources(source), scales='free_y') +
  labs(y='GW', x='',
       title=ifelse(lang=='EN', 'Newly added power capacity, year-to-date',
                    '新增发电容量，分月累计值'),
       col='') +
  scale_x_date(date_breaks = '3 months', labels = monthlab, minor_breaks = 'month',
               expand = expansion(mult=c(.0,.1))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  theme_crea() +
  lang_theme(lang=lang) +
  scale_color_crea_d(col.index = c(7, 2:5, 1)) -> plt
quicksave(file.path(output_dir, paste0('Newly added power capacity, top provinces, year-to-date, ',lang,'.png')), plot=plt)


provcap %>% filter(grepl('Wind|Solar', source)) %>% replace_na(list(Value=0)) %>%
  mutate(source = gsub(' Energy', '', source)) %>%
  filter(year(date)>=2010, prov %in% prov_ranking$prov[2:17], month(date)==4) %>%
  ggplot(aes(date, Value/100, col=source)) + geom_line(size=1) + facet_wrap(~prov) +
  theme_crea() +
  lang_theme(lang=lang) +
  scale_color_crea_d(guide=guide_legend(nrow=1)) +
  labs(title='Newly added wind and solar power capacity',
       subtitle = paste0(period_name, ', by year'),
       color='') +
  theme(legend.position = 'top') -> plt
quicksave(file.path(output_dir, 'newly added wind&solar by province.png'), plot=plt, footer_height=.03)
