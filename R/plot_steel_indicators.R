steel_indicator_plots <- function(lang=parent.frame()$lang,
                                  output_dir=get('output_dir', envir=.GlobalEnv)) {
  in_file = "data/steel plant operating rates.xlsx"

  readwindEN(in_file, paste0('V', 1:3), read_vardata = T, zero_as_NA = T, force_last_of_month = F) -> steel

  steel %>%
    filter(!is.na(Value), year(date)>=2015) %>%
    group_by(Name) %>% filter(max(year(date))>=year(today()-90)) %>%
    mutate(plotdate = 'year<-'(date, 2020),
           yr=as.character(year(date))) ->
    plotdata

  plotdata %>% filter(!is.na(Value), year(date)>=2015) %>%
    mutate(plotdate = 'year<-'(date, 2020),
           yr=as.character(year(date)),
           Name=gsub('Custeel: | \\(.*|: National|: China|China: |Refining Production | Steel Mills| of Major Steel Mills', '', Name)) %>%
    ggplot(aes(plotdate, Value, col=yr)) +
    facet_wrap(~Name, scales='free_y') +
    geom_line(size=1, alpha=.75) +
    geom_dl(aes(label=yr), method=list('last.bumpup', cex=.6, hjust=-.1)) +
    scale_x_date(date_labels = '%b', expand=expansion(add=c(0,40))) +
    #scale_y_continuous(labels=scales::percent) +
    x_at_zero() +
    theme_crea() + theme(strip.text = element_text(size=rel(.8))) +
    scale_color_crea_d('dramatic', col.index = rep(1:6,2), guide=F) +
    scale_linetype_manual(values=2:1) +
    labs(x='', y='', col='year', title='Steel industry weekly operating indicators',
         caption='Source: Wind Information') -> p
  quicksave(file.path(output_dir, 'Steel industry weekly operating indicators.png'), plot=p, scale=1.5)
}
