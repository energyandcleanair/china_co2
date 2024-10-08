capacity_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1),
                           lang=parent.frame()$lang,
                           output_dir=get('output_dir', envir=.GlobalEnv)) {

  library(directlabels)

  in_file = get_data_file("Power Capacity.xlsx")
  readwindEN(in_file, c('var', 'source', 'fuel', 'YTD'), read_vardata = T) %>%
    mutate(var = ifelse(grepl('New', var), 'Newly added capacity', 'Installed capacity'),
           source = source %>% gsub(' ?Power.*', '', ., ignore.case=T) %>% gsub('YTD', 'All', .),
           fuel = ifelse(is.na(fuel) | fuel %in% c('YTD', 'National'), 'All', fuel)) -> cap

  if(exists('data_summary')) {
    data_summary <<- data_summary %>% bind_rows(check_dates(data = cap,
                                                            file_name = "Power Capacity.xlsx"))
  }

  cap %>%
    filter(year(date)>=year(today()) - 5, fuel=='All', grepl('New', var)) %>%
    group_by(source, fuel, year=as.factor(year(date))) %>%
    group_modify(function(df, groups) {
      df %>% head(1) %>% mutate(date=ymd(paste(groups$year, 1, 1)), Value=0) %>%
        bind_rows(df)
    }) %>%
    write_csv(file.path(output_dir, 'newly added power capacity.csv')) ->
    plotdata

  plotdata %<>%
    group_by(source, fuel) %>%
    mutate(YoY=get.yoy(Value, date) %>% scales::percent(accuracy = 1, style_positive='plus'),
           Value=convert_value(Value, '10MW', lang=lang),
           plotdate=date %>% 'year<-'(2024),
           source=ifelse(fuel=='All', source, fuel))

  plotdata %>% filter(date %>% 'day<-'(1) %>% equals(focus_month)) -> yoy_labels

  plt <- plotdata %>%
    ggplot(aes(plotdate, Value, colour = year)) +
    geom_line(linewidth = 1) +
    geom_label(data = yoy_labels, aes(label = YoY), vjust = -2) +
    facet_wrap(~translateSources(source, lang = lang), ncol = 2, scales = 'free_y') +
    labs(y = unit_label('10MW', lang = lang), x = '',
         title = trans('Newly added power capacity, year-to-date'),
         caption = trans('Labels show year-on-year changes for the current year')) +
    scale_x_date(date_breaks = '3 months', labels = monthlab,
                 minor_breaks = 'month', expand = expansion(mult = c(.0, .17))) +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    theme_crea_new() +
    theme(axis.text.x = element_text(hjust = .2)) +
    lang_theme(lang = lang) +
    scale_color_crea_d(col.index = c(7, 2:5, 1), labels = yearlab, guide = 'none') +
    geom_dl(aes(label = yearlab(year, lang = lang)),
            method = list('last.bumpup', cex = .7))
  quicksave(file.path(output_dir,
                      paste0('Newly added power capacity, year-to-date, ', lang,
                             '.png')),
            plot = plt,
            scale = 1.2)

  fuel_cols = crea_palettes$CREA[c(1, 4, 2, 6, 5)]
  names(fuel_cols) = cap$source %>% unique %>% subset(.!='All') %>% sort #%>% translateSources()
  cap$date %>% year %>% max %>% seq(2010, ., 1) -> yrs
  cap$date %>% max %>% month -> ytd_month

  plt <- cap %>% filter(fuel == 'All', month(date) == month(ytd_month),
                        grepl('New', var), year(date) %in% yrs) %>%
    write_csv(file.path(output_dir, 'Newly added power capacity, YTD.csv')) %>%
    ggplot(aes(year(date), convert_value(Value, '10MW'), fill = source,
               alpha = year(date))) +
    geom_col(size = 1) +
    facet_wrap(~translateSources(source, lang = lang), ncol = 2, scales = 'free') +
    geom_label(data = yoy_labels, aes(label = YoY), vjust = -2, fill = 'white') +
    labs(y = unit_label('10MW', lang = lang), x = '',
         title = ifelse(lang == 'EN',
                        paste0('Newly added power capacity, January to ',
                               month.name[ytd_month]),
                        paste0("\u65b0\u589e\u53d1\u7535\u88c5\u673a\u5bb9\u91cf,\u524d",
                               ytd_month, "\u4e2a\u6708\u7d2f\u8ba1\u503c"))) +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    scale_x_continuous(labels = yearlab, breaks = yrs) +
    theme_crea_new() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    lang_theme(lang = lang) +
    scale_fill_manual(values = unname(fuel_cols), guide = 'none') +
    scale_alpha(range = c(.5, 1), guide = 'none')

  quicksave(file.path(output_dir,
                      paste0('Newly added power capacity, YTD, ', lang, '.png')),
            plot = plt)

  in_file = get_data_file("New power capacity by province and type.xlsx")
  getwindvars(in_file)
  readwindEN(in_file, c('var', 'source', 'prov'), read_vardata = T) -> provcap

  pwr_sources <- c('Thermal', 'Wind', 'Solar', 'Nuclear', 'Hydro')
  provs <- read_xlsx(get_data_file('provincesZH.xlsx'))
  provcap$prov %>% unique %>% subset(!grepl(paste(pwr_sources, collapse='|'), .))
  provcap %<>% mutate(prov = disambiguate(Name, c(provs$Province, 'National')), source=disambiguate(Name, pwr_sources))

  ytd_date = max(provcap$date)
  period_name = case_when(lang=='EN'~paste('January -', month.name[ytd_month], year(ytd_date)),
                          lang=='ZH'~paste0(year(ytd_date),'\u5e741-',ytd_month,'\u6708'))

  p <- provcap %>%
    filter(Value > 0,
           year(date) == year(focus_month),
           month(date) <= month(focus_month),
           prov != 'National') %>%
    mutate(source = source %>% gsub(' Energy| Power|power', '', .)) %>%
    group_by(source) %>%
    group_by(source, prov) %>%
    summarise(across(Value, max)) %>%
    slice_max(Value, n = 10) %>%
    arrange(desc(Value)) %>%
    ungroup %>%
    mutate(prov = translateProvinces(prov, lang = lang)) %>%
    mutate(prov = factor(paste(prov, source, sep = '_'),
                         levels = rev(paste(prov, source, sep = '_')))) %>%
    ggplot(aes(prov, convert_value(Value, '10MW'))) +
    geom_col(aes(fill = source)) +
    scale_x_discrete(labels = function(x) str_split_i(x, '_', 1)) +
    scale_fill_manual(values = fuel_cols, guide = 'none') +
    facet_wrap(~translateSources(source, lang = lang), scales = 'free', ncol = 2) +
    coord_flip() +
    theme_crea_new() +
    theme(plot.margin = unit(c(.5, 1.5, .2, .2), 'line')) +
    lang_theme(lang = lang) +
    labs(y = unit_label('10MW', lang = lang), x = '',
         title = trans("Newly installed power capacity by province"),
         subtitle = period_name) +
    scale_y_continuous(expand = expansion(mult = c(0, .05)))

  quicksave(file.path(output_dir,
                      paste0('power capacity additions by province, ', lang, '.png')),
            plot = p, scale = 1.15)
}
