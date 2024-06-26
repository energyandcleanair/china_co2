steel_indicator_plots <- function(lang=parent.frame()$lang,
                                  start_year=year(today())-5,
                                  output_dir=get('output_dir', envir=.GlobalEnv)) {

  library(directlabels)

  in_file = get_data_file("steel plant operating rates.xlsx")

  readwindEN(in_file, paste0('V', 1:3), read_vardata = T, zero_as_NA = T, force_last_of_month = F) -> steel

  if(exists('data_summary')) {
    data_summary <<- data_summary %>% bind_rows(check_dates(data = steel,
                                                            file_name = "steel plant operating rates.xlsx"))
  }

  steel %>%
    filter(!is.na(Value), year(date)>=2015) %>%
    group_by(Name) %>% filter(max(year(date))>=year(today()-90)) %>%
    mutate(plotdate = 'year<-'(date, 2020),
           yr=as.character(year(date))) ->
    plotdata

  write_excel_csv(plotdata %>% select(Name, ID, Date = date, Frequency, Value, Unit, `Time Period`,
                                      Source),
                  file.path(output_dir, 'Steel industry weekly operating indicators.csv'))

  plotdata %>% filter(!is.na(Value), year(date)>=start_year) %>%
    filter(!grepl('Estimated.*Crude|Stove', Name)) %>% #remove duplicated datasets
    mutate(plotdate = 'year<-'(date, 2020),
           yr=as.character(year(date)),
           Name=gsub('Custeel: | \\(.*|: National|: China|China: |Refining Production | Steel Mills| of Major Steel Mills|: [0-9]+$', '', Name),
           Name = case_match(Name,
                             'Daily Average Output: Crude Steel' ~ 'Average Daily Output: Crude Steel',
                             'Operating Rate: Deformed Steel Bar: Major' ~ 'Deformed Steel Bar: Operating Rate',
                             'Operating Rate: Wire Rod: Major' ~ 'Operating Rate: Wire Rod',
                             .default = Name)) %>%
    ggplot(aes(plotdate, Value, col=yr)) +
    facet_wrap(~trans(Name), scales='free_y') +
    geom_line(linewidth=1, alpha=.75) +
    geom_dl(aes(label=yearlab(yr)), method=list('last.bumpup', cex=.6, hjust=-.1)) +
    scale_x_date(labels = monthlab, expand=expansion(add=c(0,60))) +
    #scale_y_continuous(labels=scales::percent) +
    x_at_zero() +
    theme_crea() + theme(strip.text = element_text(size=rel(.8))) +
    lang_theme(lang=lang) +
    scale_color_crea_d('dramatic', col.index = rep(1:6,2), guide='none') +
    scale_linetype_manual(values=2:1) +
    labs(x='', y='', col='year', title=trans('Steel industry weekly operating indicators'),
         caption=trans('Source: Wind Information')) -> p
  quicksave(file.path(output_dir, paste0('Steel industry weekly operating indicators, ',lang,'.png')), plot=p, scale=1.33,
            png = T)
}
