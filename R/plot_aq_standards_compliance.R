aq_compliance_plots <- function(start_date=ymd('2019-01-01'),
                               cities,
                               pollutants=c('pm25', 'o3'),
                               update_data = T,
                               lang=parent.frame()$lang,
                               output_dir=get('output_dir', envir=.GlobalEnv)) {

  aq_capitals <- get_aq(start_date=start_date-365,
                        update_data=T,
                        aq_file='cache/province_capital_air_quality_data.RDS',
                        country='CN',
                        cities=cities,
                        source='mee') %>%
    filter(pollutant %in% pollutants, location_id %in% cities)

  aqs = tibble(pollutant=c('pm25', 'no2', 'o3'), aqs=c(35,40,160))

  aq_capitals %<>% add_location_names(country = 'CN')

  aq_capitals %>%
    inner_join(aqs) %>%
    group_by(across(where(is.character))) %>%
    mutate(value_12m = case_when(pollutant!='o3'~value %>% pmin(500) %>% rollapplyr(365, mean, fill=NA),
                                 T~value %>% rollapplyr(365, quantile, probs=.9, fill=NA))) ->
    aq_capitals_12m

  aq_capitals_12m %<>% mutate(city_label = case_when(city_name==NAME_1~city_name,
                                                     T~paste0(city_name, ', ', fix_province_names(NAME_1))))

  aq_capitals_12m %>%
    ungroup %>% filter(!is.na(value_12m), pollutant=='pm25', date>=start_date) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col=value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth=.75) +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype=trans('National air quality standard'), yintercept = 35), alpha=.5) +
    theme_crea(legend.position='top', axis.text.x=element_text(angle=30, hjust=1)) +
    scale_color_crea_c('change', guide='none', darken=.15) +
    labs(title=trans('PM2.5 concentrations in province capitals'),
         x='', y=trans('µg/m3'),
         subtitle=trans('12-month moving average')) +
    scale_linetype_manual(values='dotted', name='') -> p
  quicksave(file.path(output_dir, paste0('PM2.5 compliance in province capitals, ',lang,'.png')), plot=p, footer_height=.03)

  aq_capitals_12m %>%
    ungroup %>% filter(!is.na(value_12m), pollutant=='o3', date>=start_date) %>%
    ggplot(aes(date, value_12m)) +
    geom_line(aes(col=value_12m %>% divide_by(aqs) %>% pmax(.8) %>% pmin(1.2)),
              linewidth=.75) +
    facet_wrap(~city_label) +
    geom_hline(aes(linetype=trans('National air quality standard'), yintercept = 160), alpha=.5) +
    theme_crea(legend.position='top', axis.text.x=element_text(angle=30, hjust=1)) +
    scale_color_crea_c('change', guide='none', darken=.3) +
    labs(title=trans('Ozone concentrations in province capitals'),
         x='', y=trans('µg/m3, 90th percentile of daily 8-hour maximum'),
         subtitle=trans('90th percentile over 12 months')) +
    scale_linetype_manual(values='dotted', name='') +
    scale_x_datetime(labels = yearlab) -> p
  quicksave(file.path(output_dir, paste0('Ozone compliance in province capitals, ',lang,'.png')), plot=p, footer_height=.03)

  aq_capitals_12m %>% ungroup %>%
    filter(!is.na(value_12m), date>=start_date) %>%
    select(location_id, pollutant, city_name_EN, NAME_1_EN, city_name_ZH, NAME_1_ZH, date, value_12m, unit, source) %>%
    write_excel_csv(file.path(output_dir, 'air quality compliance in provincial capitals.csv'))
}


