
require(showtext)
showtext_auto(F)
zh_theme <- list(theme(text=element_text(family='Source Sans'),
                       plot.title = element_text(size=rel(2), margin=margin(c(20,12,16,12)))))

strsplit_zh <- function(x, width) {
  starts <- seq(1, nchar(x), by=width) %>% c(nchar(x)) %>% unique
  
  # chop it up
  out <- character()
  for(i in 1:(length(starts)-1)) {
    if(substr(x, starts[i], starts[i]) %in% c("，", "。","：")) starts[i] %<>% add(1)
    out[i] <- substr(x, starts[i], starts[i+1])
  }
  out %>% paste(collapse='\n')
}

d.quarter %>% filter(month(date) %in% seq(3,12,3), prod=='Total', year(date)>=2014, name=='predicted based on output') %>% 
  mutate(CO2_3m = case_when(date=='2023-03-31'~CO2_3m[date=='2022-03-31']*(1+3.81469e-2), T~CO2_3m)*3) %>% 
  ungroup %>% select(date, CO2_3m, prod) %>% mutate(sector='All') %>% 
  ggplot(aes(date-45, CO2_3m/100, fill=month(date)==3)) + geom_col(width=92) +
  theme_crea() + zh_theme + 
  theme(axis.text.x = element_text(vjust=-1.5), plot.title = element_text(lineheight=1)) +
  scale_fill_manual(values=c('lightgray', 'darkred'), guide=F) +
  scale_x_date(expand=expansion(mult=c(.0, .0)), date_breaks = '1 year', date_labels = '%Y年') + 
  x_at_zero() + 
  labs(title=strsplit_zh("2023年第一季度，中国CO2排放量增长了4%，创下了每年前三个月的历史新高", width=25),
       subtitle="每季度二氧化碳排放量，亿吨",
       y="亿吨/季度", x="") -> plt
quicksave(file.path(output_dir, "quarterly total CO2 ZH.png"), plot=plt, footer_height=.015, scale=1)

file.path(output_dir, "contributions to emissions growth, aligned to official totals.xlsx") %>% read_xlsx -> changes.adj

changes.adj %>% filter(prod != 'Total') %>% rename(YoY_change_3m='YoY change, Mt') %>% 
  ggplot(aes(prodZH, YoY_change_3m*100, fill=sectorZH)) +
  geom_col() +
  scale_y_continuous(expand=expansion(c(.05,.05))) +
  theme_crea() + zh_theme +
  theme(legend.text = element_text(size=rel(.9))) +
  scale_fill_crea_d(name='行业') +
  labs(title="导致二氧化碳排放量变化的影响因素",
       subtitle="2023年第一季度；与2022年相比", y='万吨/季度', x='') +
  coord_flip() + 
  scale_x_discrete(limits=rev) +
  x_at_zero() -> plt
quicksave(file.path(output_dir, 'Contributions to emissions growth ZH.png'), plot=plt, scale=.9)


require(directlabels)
lang='ZH'
cap %>% 
  filter(year(date)>=2018, fuel=='All', grepl('New', var)) %>% 
  group_by(source, fuel, year=as.factor(year(date))) %>% 
  group_modify(function(df, groups) {
    df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>% 
      bind_rows(df)
  }) %>% 
  mutate(plotdate=date %>% 'year<-'(2022) %>% 'day<-'(1),
         source=ifelse(fuel=='All', source, fuel)) %>% 
  ggplot(aes(plotdate, Value, col=year)) + geom_line(size=1) + 
  facet_wrap(~translateSources(source), ncol=2, scales='free_y') +
  labs(y='万千瓦', x='', title='新增发电装机容量；分月累计值') +
  scale_x_date(breaks = ymd(paste(2022,seq(3,12,3),1)), labels = monthlab, minor_breaks = 'month',
               expand = expansion(mult=c(.0,.15))) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
  theme_crea(axis.text.x=element_text(hjust=.2)) + 
  theme(text=element_text(family='Source Sans'),
        plot.title = element_text(size=rel(2), margin=margin(c(12,12,12,12)))) +
  #theme(text=element_text(size=40, lineheight = .01, margin=margin(-100,-100,-100,-100))) +
  scale_color_crea_d(col.index = c(7, 2:5, 1), labels=yearlab, guide=F) +
  geom_dl(aes(label=yearlab(year)), method=list('last.bumpup', cex=.7)) -> plt
quicksave(file.path(output_dir, 'Newly added power capacity, year-to-date ZH.png'), plot=plt, scale=.9)


prodcolsZH <- prodcols
names(prodcolsZH) %<>% translateFuels

d.quarter %>% filter(include_in_totals, grepl('predicted', name)) %>%
  mutate(prod=prod %>% translateFuels %>% factor(levels=names(prodcolsZH))) %>% 
  ggplot(aes(date, CO2_12m*12/100)) +
  geom_area(aes(fill=prod)) +
  geom_line(aes(col='疫情前趋势'), 
            data=trend %>% filter(year(date)>=2012), 
            linetype='dashed', size=1) +
  scale_x_date(expand=c(0,0), labels=function(x) paste0(year(x), '年')) +
  scale_y_continuous(expand=expansion(c(0,.05))) +
  theme_crea() +
  theme(text=element_text(family='Source Sans'),
        plot.title = element_text(size=rel(2), margin=margin(c(12,12,12,12)))) +
  scale_fill_manual(values=prodcolsZH, name='产品') +
  scale_color_crea_d(name='') +
  labs(title="中国能源和水泥行业的二氧化碳排放量",
       subtitle="12个月移动总和", y='亿吨/年', x='') -> plt
quicksave(file.path(output_dir, 'CO2 12m sum ZH.png'), plot=plt, scale=1)


