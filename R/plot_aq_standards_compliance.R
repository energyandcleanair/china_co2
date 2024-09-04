aq_compliance_plots <- function(start_date = ymd('2019-01-01'),
                                cities,
                                pollutants = c('pm25', 'o3'),
                                one_month_plots = F,
                                aq_cache = 'cache/province_capital_air_quality_data.RDS',
                                update_data = T,
                                lang = parent.frame()$lang,
                                output_dir = get('output_dir', envir = .GlobalEnv),
                                focus_month) {

  # Assign global variables used by creahelpers for translation
  trans_file = get_data_file('label_translations.xlsx')
  assign("trans_file", trans_file, envir = .GlobalEnv)
  assign("lang", lang, envir = .GlobalEnv)

  # Collect data
  aq_capitals <- get_aq(start_date = start_date-365,
                        update_data = T,
                        aq_file = aq_cache,
                        country = 'CN',
                        cities = cities,
                        source = 'mee') %>%
    filter(pollutant %in% pollutants, location_id %in% cities)

  data_summary <<- data_summary %>%
    bind_rows(check_dates(data = aq_capitals,
                          file_name = 'aq data from database - compliance'))

  aqs <- tibble(pollutant = c('pm25', 'no2', 'o3'),
                aqs = c(35, 40, 160))

  aq_capitals <- aq_capitals %>% add_location_names(country = 'CN', lang = lang)

  aq_capitals_12m <- aq_capitals %>% arrange(date) %>%
    inner_join(aqs) %>%
    group_by(across(where(is.character))) %>%
    mutate(value_12m = case_when(pollutant != 'o3' ~ value %>% pmin(500) %>%
                                   rollapplyr(365, mean, fill = NA),
                                 T ~ value %>% rollapplyr(365, quantile, probs = .9,
                                                          na.rm = T, fill = NA)))


  aq_capitals_12m <- aq_capitals_12m %>%
    mutate(city_label = case_when(city_name == NAME_1 ~ city_name,
                                  T ~ paste0(city_name, ', ', fix_province_names(NAME_1))))

  p <- aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), pollutant == 'pm25', date >= start_date) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col = value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth = .75) +
    geom_text_repel(aes(label = round(value_12m, digits = 0)),
                    data = aq_capitals_12m %>% ungroup %>%
                      filter(!is.na(value_12m), pollutant == 'pm25') %>%
                      filter(date == max(date)),
                    size = 3, direction = 'y') +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype = trans('National air quality standard'), yintercept = 35),
               alpha = .5) +
    theme_crea(legend.position = 'top',
               axis.text.x = element_text(angle = 30, hjust=  1)) +
    lang_theme(lang = lang) +
    scale_color_crea_c('change', guide = 'none', darken = .15) +
    labs(title = trans('PM2.5 concentrations in provincial capitals'),
         x = '', y = trans('µg/m3'),
         subtitle = trans('12-month moving average'),
         caption = paste(trans('Data until'), max(date(aq_capitals_12m$date)))) +
    scale_linetype_manual(values = 'dotted', name = '') +
    scale_x_date(labels = yearlab)
  quicksave(file.path(output_dir, paste0('PM2.5 compliance in provincial capitals, ',
                                         lang, '.png')),
            plot = p, footer_height = .03, png = T)

  p <- aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), pollutant == 'pm25', date >= start_date,
           date < (focus_month + months(1))) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col = value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth = .75) +
    geom_text_repel(aes(label = round(value_12m, digits = 0)),
                    data = aq_capitals_12m %>% ungroup %>%
                      filter(!is.na(value_12m), pollutant == 'pm25',
                             date >= start_date, date < (focus_month + months(1))) %>%
                      filter(date == max(date)),
                    size = 3, direction = 'y') +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype = trans('National air quality standard'), yintercept = 35),
               alpha = .5) +
    theme_crea(legend.position = 'top',
               axis.text.x = element_text(angle = 30, hjust = 1)) +
    lang_theme(lang = lang) +
    scale_color_crea_c('change', guide = 'none', darken = .15) +
    labs(title = trans('PM2.5 concentrations in provincial capitals'),
         x = '', y = trans('µg/m3'),
         subtitle = trans('12-month moving average'),
         caption = paste(trans('Data until'),
                         ceiling_date(focus_month, unit = 'month') - days(1))) +
    scale_linetype_manual(values = 'dotted', name = '') +
    scale_x_date(labels = yearlab)
  quicksave(file.path(output_dir,
                      paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m")),
                      paste0('PM2.5 compliance in provincial capitals until ',
                             ceiling_date(focus_month, unit = 'month') - days(1),
                             ',', lang, '.png')),
            plot = p, footer_height = .03, png = T)

  p <- aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), pollutant == 'o3', date >= start_date) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col = value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth = .75) +
    geom_text_repel(aes(label = round(value_12m, digits = 0)),
                    data = aq_capitals_12m %>% ungroup %>%
                      filter(!is.na(value_12m), pollutant == 'o3') %>%
                      filter(date == max(date)),
                    size = 3, direction = 'y') +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype = trans('National air quality standard'), yintercept = 160),
               alpha = .5) +
    theme_crea(legend.position = 'top',
               axis.text.x = element_text(angle = 30, hjust = 1)) +
    lang_theme(lang = lang) +
    scale_color_crea_c('change', guide = 'none', darken = .3) +
    labs(title = trans('Ozone concentrations in provincial capitals'),
         x = '', y = trans('µg/m3, 90th percentile of daily 8-hour maximum'),
         subtitle = trans('90th percentile over 12 months'),
         caption = paste(trans('Data until'), max(date(aq_capitals_12m$date)))) +
    scale_linetype_manual(values = 'dotted', name = '') +
    scale_x_date(labels = yearlab)
  quicksave(file.path(output_dir, paste0('Ozone compliance in provincial capitals, ', lang, '.png')),
            plot = p, footer_height = .03, png = T)

  p <- aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), pollutant == 'o3', date >= start_date,
           date < (focus_month + months(1))) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col = value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth = .75) +
    geom_text_repel(aes(label = round(value_12m, digits = 0)),
                    data = aq_capitals_12m %>% ungroup %>%
                      filter(!is.na(value_12m), pollutant == 'o3',
                             date >= start_date,
                             date < (focus_month + months(1))) %>%
                      filter(date == max(date)),
                    size = 3, direction = 'y') +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype = trans('National air quality standard'), yintercept = 160),
               alpha = .5) +
    theme_crea(legend.position = 'top',
               axis.text.x = element_text(angle = 30, hjust = 1)) +
    lang_theme(lang = lang) +
    scale_color_crea_c('change', guide = 'none', darken = .3) +
    labs(title = trans('Ozone concentrations in provincial capitals'),
         x = '', y = trans('µg/m3, 90th percentile of daily 8-hour maximum'),
         subtitle = trans('90th percentile over 12 months'),
         caption = paste(trans('Data until'),
                         ceiling_date(focus_month, unit = 'month') - days(1))) +
    scale_linetype_manual(values = 'dotted', name = '') +
    scale_x_date(labels = yearlab)
  quicksave(file.path(output_dir,
                      paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m")),
                      paste0('Ozone compliance in provincial capitals until ',
                             ceiling_date(focus_month, unit = 'month') - days(1),
                             ',', lang, '.png')),
            plot = p, footer_height = .03, png = T)

  aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), date >= start_date) %>%
    select(location_id, pollutant, city_name_EN, NAME_1_EN, city_name_ZH,
           NAME_1_ZH, date, value_12m, unit, source) %>%
    write_excel_csv(file.path(output_dir,
                              'air quality compliance in provincial capitals.csv'))

  if(one_month_plots) {
    for(poll in pollutants) {
      pollname_EN <- recode(poll, o3 = 'ozone', pm25 = 'PM2.5', no2 = 'NO2')
      pollname <- ifelse(poll == 'o3', '\u81ed\u6c27', pollname_EN)

      plottitle_EN <- paste(format(focus_month, '%B'),
                            ifelse(poll == 'o3', '', 'average'),
                            pollname_EN, 'levels in provincial capitals')

      plottitle <- ifelse(lang == 'EN', plottitle_EN,
                          paste0("\u7701\u4f1a\u57ce\u5e02\u5404\u5e74",
                                 month(focus_month), "\u6708", pollname,
                                 ifelse(poll == 'o3', '', '\u5e73\u5747'),
                                 "\u6d53\u5ea6"))
      subtitle <- ifelse(poll == 'o3', trans('90th percentile of daily 8-hour maximum'), '')

      y_lab <- trans('µg/m3')

      maxval <- ifelse(poll == 'pm25', 100, NA)

      p <- aq_capitals %>% filter(pollutant == poll) %>%
        group_by(city_name, month = date %>% 'day<-'(1), pollutant) %>%
        summarise(across(value, ~ mean(.x, na.rm = TRUE))) %>%
        filter(month <= focus_month, month(month) == month(focus_month)) %>%
        write_csv(file.path(output_dir, paste0(plottitle_EN, '.csv'))) %>%
        group_by(city_name) %>%
        mutate(rank = rank(value)) %>%
        ggplot(aes(year(month), value, fill = rank)) +
        geom_col() +
        facet_wrap(~city_name) +
        coord_cartesian(ylim = c(0, maxval)) +
        labs(title = plottitle, subtitle = subtitle, x = '', y = y_lab) +
        theme_crea() +
        scale_fill_crea_c('change', guide = 'none', col.index = 5:7) +
        lang_theme(lang = lang) +
        x_at_zero()
      quicksave(file.path(output_dir,
                          paste0("monthly_snapshot_", format(as.Date(focus_month), "%Y_%m")),
                          paste0(plottitle_EN, ', ', lang, '.png')),
                plot = p, footer_height = .0, png = T)
    }
  }
}



