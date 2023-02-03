loadlibs()

"CO2data/china" %>% boxpath %>% setwd
source('wind mapping functions.R')
source('wind mapping functions 2.R')

in_file = "Newly-added Installed Capacity of Power Generation_YTD.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'fuel'), read_vardata = T) -> cap

lang='EN'


require(directlabels)
cap %>% 
  filter(year(date)>=2017, fuel=='YTD') %>% 
  group_by(source, fuel, year=as.factor(year(date))) %>% 
  group_modify(function(df, groups) {
    df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>% 
      bind_rows(df)
  }) %>% 
  mutate(plotdate=date %>% 'year<-'(2022),
               source=ifelse(fuel=='YTD', source, fuel)) %>% 
  ggplot(aes(plotdate, Value/100, col=year)) + geom_line(size=1) + 
  facet_wrap(~translateSources(source), ncol=2) +
  labs(y='GW', x='', title='Newly added power capacity, year-to-date') +
  scale_x_date(date_breaks = '3 months', labels = monthlab, minor_breaks = 'month',
               expand = expansion(mult=c(.0,.13))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
  theme_crea() + 
  scale_color_crea_d(col.index = c(7, 2:5, 1), labels=yearlab) +
  geom_dl(aes(label=yearlab(year)), method=list('last.bumpup', cex=.7))

fuel_cols = crea_palettes$CREA[c(1, 4, 2, 6, 5)]
names(fuel_cols) = cap$source %>% unique %>% sort
cap %>% filter(fuel=='YTD', month(date)==5) %>% 
  ggplot(aes(year(date), Value/100, fill=source, alpha=year(date))) + 
  geom_col(size=1) + facet_wrap(~translateSources(source), ncol=2) +
  labs(y='GW', x='', title='Newly added power capacity, January to May') +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
  scale_x_continuous(labels=yearlab) +
  theme_crea() + 
  scale_fill_manual(values=fuel_cols, guide='none') + 
  scale_alpha(range=c(.5,1), guide='none') +
  theme(axis.text.x = element_text(hjust=.1))
ggsave(paste0('Newly added power capacity, January to May, ',lang,'.png'), width=8, height=6)

in_file = "New power capacity by province and type.xlsx"
getwindvars(in_file)
readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T) -> provcap

fuel_cols['Hydropower'] <- fuel_cols['Hydro Power']

provcap %>% filter(date==max(date), prov != 'National') %>% 
  mutate(source = source %>% gsub(' Energy', '', .)) %>% 
  group_by(source) %>% slice_max(Value, n=10) %>% 
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
      theme(strip.text = element_text(size=rel(2)),
            axis.text = element_text(size=rel(1.8)),
            axis.title = element_text(size=rel(2))) +
      labs(y='GW', x='') +
      scale_y_continuous(expand=expansion(mult=c(0,.05)))
  }) -> p

require(gridExtra)
require(grid)
title.grob = textGrob("Newly installed power capacity by province", 
                      gp=gpar(fontface='bold', cex=4.5, col="#35416C"),
                      just=.5)
subtitle.grob = textGrob('January-April 2022', 
                         gp=gpar(fontface='italic',cex=3),
                         just=.5)
p.grid = arrangeGrob(grobs=p)
margin <- unit(0.5, "line")

png(paste0('power capacity additions by province, ',lang,'.png'), width=2000, height=1500, res=100)
grid.arrange(title.grob, subtitle.grob, p.grid,
             heights = unit.c(grobHeight(title.grob) + 3*margin, 
                              grobHeight(subtitle.grob) + 2*margin, 
                              unit(1,"null")))
dev.off()




cap %>% filter(month(date) %in% c(5, 12), grepl('Wind|Solar|Thermal', source), fuel=='YTD') %>% 
  mutate(month=paste0('M', month(date)), year=year(date), Value=Value/100) %>% 
  select(year, month, Value, source) %>% 
  spread(month, Value) -> scatterplotdata 
  
require(ggrepel)
scatterplotdata %>% 
  filter(M5>=1) %>% 
  ggplot(aes(M5, M12)) + 
  geom_smooth(formula=y~x, method='lm', fullrange=T, color=crea_palettes$CREA[4]) +
  geom_point() + 
  geom_text_repel(aes(label=yearlab(year))) +
  facet_wrap(~translateSources(source), scales='free') + 
  geom_vline(data=scatterplotdata %>% filter(year==2022), 
             aes(xintercept = M5), size=1,5, linetype='dotted', color=crea_palettes$dramatic[1]) +
  geom_text_repel(data=scatterplotdata %>% filter(year==2022), 
                  aes(label=yearlab(year), y=M5),
                  color=crea_palettes$dramatic[1]) +
  geom_vline(data=scatterplotdata %>% filter(year==2022), 
             aes(xintercept = M5*1.05), color=NA) +
  scale_x_continuous(expand=expansion(mult=c(0,0))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  coord_cartesian(ylim=c(0,NA)) + expand_limits(x=0) +
  theme_crea() +
  labs(title=ifelse(lang=='EN', 'Capacity installations in January-May vs. full year',
                    '各年前几个月与全年新增发电容量对比'),
       x=ifelse(lang=='EN', 'Capacity added, GW, Jan-May', '新增发电容量，前五个月'),
       y=ifelse(lang=='EN', 'Capacity added, GW, full year', '新增发电容量，全年'))
ggsave(paste0('Jan-May vs full year wind&solar, ',lang,'.png'), width=8, height=6)

provcap %>% filter(grepl('Wind|Solar', source), date==max(date)) %>% 
  group_by(prov) %>% summarise(across(Value, sum)) %>% arrange(desc(Value)) -> prov_ranking

provcap %>% filter(grepl('Wind|Solar', source)) %>% 
  filter(year(date)>=2017, prov %in% prov_ranking$prov[2:7]) %>% 
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
  scale_color_crea_d(col.index = c(7, 2:5, 1))
ggsave(paste0('Newly added power capacity, year-to-date, ',lang,'.png'), width=8, height=6)


provcap %>% filter(grepl('Wind|Solar', source)) %>% replace_na(list(Value=0)) %>% 
  mutate(source = gsub(' Energy', '', source)) %>% 
  filter(year(date)>=2010, prov %in% prov_ranking$prov[2:17], month(date)==4) %>% 
  ggplot(aes(date, Value/100, col=source)) + geom_line(size=1) + facet_wrap(~prov) +
  theme_crea() + scale_color_crea_d(guide=guide_legend(nrow=1)) +
  labs(title='Newly added wind and solar power capacity',
       subtitle = 'January-April',
       color='') + 
  theme(legend.position = 'top')
ggsave('newly added wind&solar by province.png', width=8, height=6)


provcap %>% filter(month(date) %in% c(4, 12), grepl('Solar', source)) %>% 
  mutate(month=paste0('M', month(date)), year=year(date), Value=Value/100) %>% 
  select(prov, year, month, Value, source) %>% 
  spread(month, Value) -> provscatterplotdata 

require(ggrepel)
provscatterplotdata %>% 
  #filter(M5>=.1) %>% 
  ggplot(aes(M4, M12)) + 
  geom_smooth(formula=y~x, method='lm', fullrange=T, color=crea_palettes$CREA[4]) +
  geom_point() + 
  #geom_text_repel(aes(label=year)) +
  facet_wrap(~prov, scales='free') + 
  geom_vline(data=provscatterplotdata %>% filter(year==2022), 
             aes(xintercept = M4), size=1,5, linetype='dotted', color=crea_palettes$dramatic[1]) +
  #geom_text_repel(data=scatterplotdata %>% filter(year==2022), aes(label=year, y=M5),
  #                color=crea_palettes$dramatic[1]) +
  geom_vline(data=provscatterplotdata %>% filter(year==2022), 
             aes(xintercept = M4*1.05), color=NA) +
  scale_x_continuous(expand=expansion(mult=c(0,0))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  coord_cartesian(ylim=c(0,NA)) + expand_limits(x=0) +
  theme_crea() +
  labs(title='Capacity installations in January-May vs. full year',
       x='Capacity added, GW, Jan-May',
       y='Capacity added, GW, full year')


read_csv('China 2060 capacity projections.csv') -> cap2060

cap2060 %>% pivot_longer(starts_with('X'), names_to='period', values_to='MW') %>% 
  mutate(period=period %>% gsub('X', '', .) %>% gsub('\\.', '-', .),
         Scenario=Scenario %>% gsub(' .*', '', .)) %>% 
  ggplot(aes(period, MW/1000, fill=Scenario)) + facet_wrap(~translateSources(Source), scales='free_y') + 
  geom_col(position='dodge') +
  theme_crea() + scale_fill_crea_d() +
  #labs(title='Annual capacity additions under 2060 goal', y='GW/year', x='', fill='scenario') +
  labs(title=ifelse(lang=='ZH', '双碳目标下的各年新增发电容量', 
                    'Capacity additions under the 2060 carbon neutrality goal'), 
       y=ifelse(lang=='ZH', 'GW/年', 'GW/year'), 
       x='', 
       fill=ifelse(lang=='ZH', '路径', 'scenario')) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        strip.text=element_text(size=rel(1.1)),
        plot.title = element_text(size=rel(ifelse(lang=='ZH', 2, 1.5)))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  scale_x_discrete(labels=function(x) x %>% gsub('-20', '-', .) %>% paste0(ifelse(lang=='ZH', '年', '')))
ggsave(paste0('capacity additions under 2060 goal, ',lang,'.png'), width=8, height=6)

print('end')





















