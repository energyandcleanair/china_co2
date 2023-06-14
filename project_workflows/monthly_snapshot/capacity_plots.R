capacity_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1), 
                           lang=parent.frame()$lang, 
                           output_dir=get('output_dir', envir=.GlobalEnv)) {
  in_file = "data/Power Capacity.xlsx"
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
    }) %>% 
    mutate(plotdate=date %>% 'year<-'(2022),
           source=ifelse(fuel=='All', source, fuel)) %>% 
    write_csv(file.path(output_dir, 'newly added power capacity.csv')) %>% 
    ggplot(aes(plotdate, convert_value(Value, '10MW'), col=year)) + geom_line(linewidth=1) + 
    facet_wrap(~translateSources(source), ncol=2, scales='free_y') +
    labs(y=unit_label('10MW'), x='', title=trans('Newly added power capacity, year-to-date')) +
    scale_x_date(date_breaks = '3 months', labels = monthlab, minor_breaks = 'month',
                 expand = expansion(mult=c(.0,.17))) +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
    theme_crea(axis.text.x=element_text(hjust=.2)) + 
    lang_theme() +
    scale_color_crea_d(col.index = c(7, 2:5, 1), labels=yearlab, guide='none') +
    geom_dl(aes(label=yearlab(year)), method=list('last.bumpup', cex=.7)) -> plt
  quicksave(file.path(output_dir, paste0('Newly added power capacity, year-to-date, ',lang,'.png')), plot=plt, scale=1.2)
  
  fuel_cols = crea_palettes$CREA[c(1, 4, 2, 6, 5)]
  names(fuel_cols) = cap$source %>% unique %>% subset(.!='All') %>% sort #%>% translateSources()
  cap$date %>% year %>% max %>% seq(2010, ., 1) -> yrs
  cap$date %>% max %>% month -> ytd_month
  
  cap %>% filter(fuel=='All', month(date)==month(max(date)), grepl('New', var), year(date) %in% yrs) %>% 
    write_csv(file.path(output_dir, 'Newly added power capacity, YTD.csv')) %>% 
    ggplot(aes(year(date), convert_value(Value, '10MW'), fill=source, alpha=year(date))) + 
    geom_col(size=1) + facet_wrap(~translateSources(source), ncol=2, scales='free') +
    labs(y=unit_label('10MW'), x='', title=ifelse(lang=='EN',
                                                  paste0('Newly added power capacity, January to ', month.name[ytd_month]),
                                                  paste0('新增发电装机容量，前', ytd_month,'个月累计值'))) +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) + 
    scale_x_continuous(labels=yearlab, breaks=yrs) +
    theme_crea(axis.text.x=element_text(angle=25, hjust=1)) + 
    scale_fill_manual(values=unname(fuel_cols), guide='none') + 
    scale_alpha(range=c(.5,1), guide='none') -> plt
  quicksave(file.path(output_dir, paste0('Newly added power capacity, YTD, ',lang,'.png')), plot=plt, scale=1.33)
  
  in_file = "data/New power capacity by province and type.xlsx"
  getwindvars(in_file)
  readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T) -> provcap
  
  pwr_sources <- c('Thermal', 'Wind', 'Solar', 'Nuclear', 'Hydro')
  provs <- read_xlsx('data/provincesZH.xlsx')
  provcap$prov %>% unique %>% subset(!grepl(paste(pwr_sources, collapse='|'), .))
  provcap %<>% mutate(prov = disambiguate(Name, c(provs$Province, 'National')), source=disambiguate(Name, pwr_sources))
  
  ytd_date = max(provcap$date)
  period_name = case_when(lang=='EN'~paste('January -', month.name[ytd_month], year(ytd_date)),
                          lang=='ZH'~paste0(year(ytd_date),'年1-',ytd_month,'月'))
  
  provcap %>% 
    filter(Value>0, year(date)==year(focus_month), month(date)<=month(focus_month), prov != 'National') %>% 
    mutate(source = source %>% gsub(' Energy| Power|power', '', .)) %>% 
    group_by(source) %>% 
    group_by(source, prov) %>% summarise(across(Value, sum)) %>% 
    slice_max(Value, n=10) %>% 
    group_split %>% 
    lapply(function(df) {
      df %>% 
        mutate(prov=translateProvinces(prov)) %>% 
        mutate(prov = factor(prov, levels=rev(prov))) %>%
        ggplot(aes(prov, convert_value(Value, '10MW'))) + 
        geom_col(fill=fuel_cols[unique(df$source)]) +
        facet_wrap(~translateSources(source)) +
        coord_flip() +
        theme_crea() +
        theme(#strip.text = element_text(size=rel(2)),
          #axis.text = element_text(size=rel(1.8)),
          #axis.title = element_text(size=rel(2)),
          plot.margin = unit(c(.5, 1.5, .2, .2), 'line')) +
        labs(y=unit_label('10MW'), x='') +
        scale_y_continuous(expand=expansion(mult=c(0,.05)))
    }) -> p
  
  
  title.grob = textGrob(trans("Newly installed power capacity by province"), 
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
  quicksave(file.path(output_dir, paste0('power capacity additions by province, ',lang,'.png')), plot=plt)
  
}
