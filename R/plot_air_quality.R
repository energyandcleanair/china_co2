air_quality_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1),
                              lang=parent.frame()$lang,
                              output_dir=get('output_dir', envir=.GlobalEnv),
                              update_data=T,
                              country="CN",
                              cities=china_admin_capitals,
                              pollutants = c('no2', 'pm25', 'o3'),
                              aq=NULL, aq_dw=NULL,
                              aq_file = file.path('cache', 'city_air_quality_data.RDS'),
                              aq_dw_file = file.path('cache', 'deweathered_air_quality_data.RDS'),
                              gis_dir=Sys.getenv('GIS_DIR')) {

  dir.create(output_dir, F, T)

  aq_data_start=ymd(paste(year(focus_month)-1,1,1))
  if(is.null(aq)) aq <- get_aq(start_date=aq_data_start, update_data=update_data, aq_file=aq_file)
  if(is.null(aq_dw)) aq_dw <- get_deweathered_aq(cities, pollutants, start_date=aq_data_start, update_data=update_data, aq_file=aq_dw_file)

  #add city and pollutant names
  aq %<>%
    mutate(pollutant_name = case_when(pollutant=='pm25'~'PM2.5', T~toupper(pollutant)),
           date=date(date))

  #combine the measured and deweathered datasets
  aq %>%
    mutate(type='measured') %>%
    bind_rows(aq_dw %>% mutate(type='deweathered')) ->
    aq_all

  aq_all %<>% add_location_names(country=country, lang=lang)

  aq_all %>%
    filter(location_id %in% cities) %>%
    filter(month(date) == month(focus_month)) %>%
    group_by(city_name, pollutant_name, type, year=year(date)) %>%
    dplyr::summarise(dplyr::across(c(value, anomaly), mean)) ->
    aq_annual

  aq_dw$date %>% min() -> dw_start_date

  aq_annual %<>% group_by(city_name, pollutant_name) %>%
    mutate(mean_value = mean(value[type=='measured' & year>=year(dw_start_date)]),
           value=case_when(type=='measured'~value, T~anomaly + mean_value))

  aq_annual %>% filter(pollutant_name %in% c('PM2.5', 'O3', 'NO2')) %>%
    group_by(pollutant_name) %>%
    group_map(function(plotdata, group) {


      plotdata %>% ungroup %>% filter(year==year(focus_month), type=='measured') %>% arrange(value) %>%
        mutate(city_name = factor(city_name, levels=city_name), pollutant_name=group$pollutant_name) %>%
        select(-anomaly, -mean_value) -> plotdata_means

      plotdata_means %>%
        ggplot(aes(city_name, value, fill=value)) + geom_col() + coord_flip() +
        scale_fill_gradientn(colors=rcrea::pal_crea.change[c(1:2,5:7)], guide='none') +
        theme_crea() +
        lang_theme(lang=lang) +
        x_at_zero() +
        labs(x='',
             y=trans('µg/m3'),
             subtitle=toupper(trans(group$pollutant_name))) -> p_means

      plotdata %>%
        # Only keep cities with values for both years
        group_by(city_name, type) %>%
        filter(all(c(year(focus_month), year(focus_month)-1) %in% year)) %>%
        # Create common denominator
        group_by(city_name) %>%
        mutate(value_measured = value[year==year(focus_month) - 1 & (type=='measured')]) %>%
        # Compute y-o-y
        group_by(city_name, type) %>%
        summarise(yoy=(value[year==year(focus_month)]-value[year==year(focus_month)-1]) / unique(value_measured)) -> yoy


      yoy %<>% group_by(city_name) %>%
        reframe(yoy = yoy[type=='measured']-yoy[type=='deweathered']) %>%
        mutate(type='influence of weather') %>%
        bind_rows(yoy)

      yoy$type %<>% recode(measured='total', deweathered='due to changes in emissions')

      yoy %<>% ungroup %>% arrange(yoy) %>%
        mutate(city_name=factor(city_name, city_name[type=='total']),
               pollutant_name=toupper(trans(group$pollutant_name)))

      guide_to_use <- case_when(group$pollutant_name=='NO2'~'legend', T~'none')
      levels <- levels(yoy$city_name)
      ggplot(mapping=aes(city_name, yoy)) +
        geom_col(data=yoy %>% filter(type!='total'),
                 aes(fill=trans(type))) +
        geom_point(data=yoy %>% filter(type=='total'),
                   aes(shape=trans('year-on-year change')), size=3) +
        scale_x_discrete(limits=levels) +
        coord_flip() +
        theme_crea() +
        lang_theme(lang=lang) +
        scale_fill_crea_d('change', col.index = c(5,2), name='', guide=guide_to_use) +
        scale_shape_discrete(guide=guide_to_use, name='') +
        scale_y_continuous(labels=scales::percent) +
        labs(subtitle=toupper(trans(group$pollutant_name)), x='', y='') -> p_yoy

      return(list(plot_means=p_means, plot_yoy=p_yoy, data_means=plotdata_means, data_yoy=yoy))
    }) -> plots

  make_pollution_plot <- function(plotlist, plot_title, plot_subtitle, rel_widths=1) {
    plot_grid(plotlist=plotlist, nrow=1, rel_widths=rel_widths) -> g
    title <- ggdraw() +
      draw_label(trans(plot_title), size=22, fontface='bold', color=unname(pal_crea['Dark.blue']))

    title2 <- grid::textGrob(trans(plot_title),
                           gp = gpar(fontface = "bold", cex = 1.5, col = unname(pal_crea['Dark.blue'])),
                           just = 0,
                           x = unit(0.02, "npc")
    )

    subtitle <- grid::textGrob(trans(plot_subtitle),
                              gp = gpar(cex = 1),
                              just = 0,
                              x = unit(0.02, "npc")
    )


    plot_grid(title2, subtitle, g, ncol=1,
              rel_heights=c(0.06, 0.03, 1)
              ) -> p

    quicksave(file.path(output_dir, paste0(plot_title, ', ', lang,'.png')), plot=p, footer_height=.05,
              png = T)
  }

  plot_title="Monthly average pollutant concentrations in province capitals"
  plot_subtitle=monthyearlab(focus_month)
  plots %>% lapply('[[', 'plot_means') %>% rev %>% make_pollution_plot(plot_title=plot_title,
                                                                       plot_subtitle=plot_subtitle)

  plots %>% lapply('[[', 'data_means') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, ', ', lang, '.csv')))


  plot_title="Year-on-year changes in pollutant concentrations in province capitals"
  plots %>% lapply('[[', 'plot_yoy') %>% rev() %>%
    make_pollution_plot(plot_title=plot_title, plot_subtitle=plot_subtitle, rel_widths = c(.25,.25,.5))

  plots %>% lapply('[[', 'data_yoy') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, '.csv')))


  #worst episodes (PM2.5, non-sandstorm PM2.5, O3)
  replace_nil_with_1 <- function(x) { if(length(x)==1) {x} else {1} }
  replace_nil_with_0 <- function(x) { if(length(x)==1) {x} else {0} }
  replace_nil_with_NA <- function(x) { if(length(x)==1) {x} else {NA} }

  aq_all %>%
    filter(year(date)==year(focus_month), source=='mee', type=='measured') %>%
    mutate(violation=value>=case_when(
      pollutant_name=='PM2.5'~75,
      pollutant_name=='O3'~100,
      pollutant_name=='NO2'~200)) %>%
    group_by(city_name, NAME_1, location_id, date) %>%
    mutate(PM_ratio=replace_nil_with_1(value[pollutant_name=='PM2.5']/value[pollutant_name=='PM10']),
           sand_storm=replace_nil_with_NA(value[pollutant_name=='PM10'])>150 & PM_ratio<.7
           & replace_nil_with_NA(value[pollutant_name=='NO2']) <20
           & replace_nil_with_NA(value[pollutant_name=='SO2']) <20,
           sand_storm = tidyr::replace_na(sand_storm, F)
           ) ->
    aq_episodes

  bind_rows(aq_episodes %>% filter(pollutant_name=='PM2.5') %>% mutate(value = ifelse(sand_storm, value, 0), pollutant_name='sandstorms (PM2.5)'),
            aq_episodes %>% filter(pollutant_name=='PM2.5') %>% mutate(value = ifelse(!sand_storm, value, 0), pollutant_name='PM2.5 (excluding sandstorms)'),
            aq_episodes %>% filter(pollutant_name!='PM2.5')) ->
    aq_episodes_w_sandstorms

  date_format_fun <- function(date) {
    if(lang=='EN') format(date, "%b %d") -> date_out
    if(lang=='ZH') paste0(month(date), '月', day(date), '日') -> date_out
    return(date_out)
  }

  aq_episodes_w_sandstorms %>% filter(grepl('PM2.5|O3|NO2', pollutant_name), !is.na(value)) %>%
    group_by(city_name, NAME_1, location_id, pollutant_name) %>% arrange(date) %>%
    mutate(value_rollmean = zoo::rollapplyr(value, width=7, FUN=mean, fill=NA),
           worst_day = zoo::rollapplyr(value, width=7, FUN=max, fill=NA)) %>%
    filter(date>=focus_month, date<=as.Date(focus_month)+days_in_month(as.Date(focus_month))+5) %>%
    group_by(city_name, NAME_1, location_id, pollutant_name) %>%
    slice_max(value_rollmean, n=1, with_ties = F) %>%
    group_by(pollutant_name) %>%
    slice_max(value_rollmean, n=5) %>%
    select(pollutant_name, city_name, NAME_1, date, value_rollmean, worst_day) %>%
    ungroup %>%
    mutate(across(pollutant_name, trans)) %>%
    mutate(date = paste0(date_format_fun(date-6), " – ", date_format_fun(date))) %T>%
    write_excel_csv(file.path(output_dir, paste0('worst_episodes, ', lang,'.csv')))
}


china_admin_capitals <- c(
  "harbin_chn.11_1_cn",
  "changsha_chn.14_1_cn",
  "taiyuan_chn.25_1_cn",
  "urumqi_chn.28_1_cn",
  "shenyang_chn.18_1_cn",
  "hefei_chn.1_1_cn",
  "hangzhou_chn.31_1_cn",
  "shanghai_chn.24_1_cn",
  "nanjing_chn.15_1_cn",
  "nanchang_chn.16_1_cn",
  "guangzhou_chn.6_1_cn",
  "lhasa_chn.29_1_cn",
  "xining_chn.21_1_cn",
  "lanzhou_chn.5_1_cn",
  "chengdu_chn.26_1_cn",
  "nanning_chn.7_1_cn",
  "changchun_chn.17_1_cn",
  "yinchuan_chn.20_1_cn",
  "zhengzhou_chn.12_1_cn",
  "guiyang_chn.8_1_cn",
  "fuzhou_chn.4_1_cn",
  "wuhan_chn.13_1_cn",
  "shijiazhuang_chn.10_1_cn",
  "tianjin_chn.27_1_cn",
  "beijing_chn.2_1_cn",
  "xi'an_chn.22_1_cn",
  "jinan_chn.23_1_cn",
  "kunming_chn.30_1_cn",
  "hohhot_chn.19_1_cn",
  "chongqing_chn.3_1_cn",
  "haikou_chn.9_1"
)


convert_dt <- function(x) {
  if(!is.POSIXt(x)) x %<>% ymd_hms
  x
}


get_aq <- function(start_date=ymd('2022-01-01'),
                   update_data=T,
                   cache_folder='cache',
                   aq_file=file.path(cache_folder, 'city_air_quality_data.RDS'),
                   country='CN',
                   cities=NULL,
                   source='mee') {

  message('read measured air quality data')
  dir.create(cache_folder, showWarnings = F, recursive = T)
  aq <- NULL

  if(!is.null(aq_file) && file.exists(aq_file)) {
    readRDS(aq_file) -> aq
    start_date <- aq$date %>% max %>% lubridate::date()
  }

  if(update_data) {
    seq.Date(start_date, today(), by='month') %>%
      pbapply::pblapply(function(start_date) {
        message(start_date)
        end_date=start_date %>% 'day<-'(days_in_month(start_date))

        source_url <- paste0("https://api.energyandcleanair.org/measurements?",
                             glue("country={country}&source={source}&date_from={start_date}&date_to={end_date}"),
                             "&pollutant=no2,pm25,pm10,so2&level=city&process_id=city_day_mad&format=csv")

        if(!is.null(cities)) source_url %<>% paste0('&city=', paste(cities, collapse = ","))

        read_csv(source_url, show_col_types = FALSE) %>%
          select(-any_of('...1')) %>%
          mutate(across(date, convert_dt), across(value, as.numeric)) -> conc_24h

        #read 8-hour max ozone
        read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                        glue("country={country}&source={source}&pollutant=o3&process_id=city_8h_max_day_mad&"),
                        "date_from=",start_date,"&date_to=", end_date,
                        "&level=city&format=csv"), show_col_types = FALSE) %>%
          select(-any_of('...1')) %>%
          mutate(across(date, convert_dt), across(value, as.numeric)) -> conc_8h

        #read 1-hour max NO2
        if(F) {
          read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                          glue("country={country}&source={source}"),
                          "&pollutant=no2&process=city_max_day_mad",
                          "&level=city&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv"),
                   show_col_types = FALSE) -> conc_1h
        }

        bind_rows(conc_24h, conc_8h)
      }) %>% bind_rows(aq) %>% distinct(date, location_id, pollutant, .keep_all=T) -> aq

    if(!is.null(aq_file)) aq %>% saveRDS(aq_file)
  }
  return(aq)
}

get_deweathered_aq <- function(cities,
                               pollutants = c('no2', 'pm25', 'o3'),
                               start_date=ymd('2022-01-01'),
                               update_data=T,
                               cache_folder='cache',
                               source='mee',
                               aq_file=file.path(cache_folder, 'deweathered_air_quality_data.RDS')) {

  message('read deweathered air quality data')
  dir.create(cache_folder, showWarnings = F, recursive = T)
  aq <- NULL

  if(!is.null(aq_file) && file.exists(aq_file) & !update_data) {
    # Deweathered data should be taken as a whole, not only missing dates
    readRDS(aq_file) -> aq
    start_date <- aq$date %>% max %>% date()
  }

  if(update_data) {
    pollutants %>%
      pbapply::pblapply(
        function(poll) {
          read_csv(
            paste0('http://api.energyandcleanair.org/v1/measurements?',
            sprintf("location_id=%s&pollutant=%s&process_id=default_anomaly_2018_2099,default_anomaly_o3_2018_2099&variable=anomaly&format=csv&source=%s&date_from=%s",
                    paste0(cities, collapse = ","),
                    poll,
                    source,
                    start_date))
          )
        }
      ) %>%
      bind_rows %>%
      dplyr::rename(anomaly=value) %>%
      bind_rows(aq) -> aq

    if(!is.null(aq_file)) aq %>% saveRDS(aq_file)
  }
  return(aq)
}

add_location_names <- function(df, country, lang = 'EN') {
  cities_meta <- read_csv(glue('https://api.energyandcleanair.org/cities?country={country}&format=csv'))

  #add province names
  adm1 <- readr::read_csv(get_data_file('gadm1.csv'))

  # Remove if exists to prevent conflicts
  if("gadm1_id" %in% names(df)) df %<>% select(-gadm1_id)

  df %<>%
    left_join(cities_meta %>% select(location_id=id, gadm1_id),
              by=c('location_id')) %>%
    mutate(gadm1_id=toupper(gadm1_id)) %>%
    left_join(adm1 %>% select(gadm1_id=GID_1, NAME_1_EN=NAME_1))

  #add city and province names in Chinese
  df %<>% filter(country_id == !!country)

  if(is.null(df$city_name_EN)) df %<>% mutate(city_name_EN = location_id %>% gsub('_.*', '', .) %>% capitalize_first())

  # Add Chinese names
  if(country == "CN"){
    station_key <- read_csv(get_data_file('air_quality_station_codes.csv'))
    city_key <- station_key %>%
      distinct(city_name_EN=CityEN, NAME_1_EN=ProvinceEN, city_name_ZH=CityZH, NAME_1_ZH = ProvinceZH) %>%
      filter(!is.na(city_name_EN))

    df %<>% inner_join(city_key)

    if(lang == 'EN') df %<>% mutate(city_name = city_name_EN, NAME_1 = NAME_1_EN)
    if(lang == 'ZH') df %<>% mutate(city_name = city_name_ZH, NAME_1 = NAME_1_ZH)
  }
  df
}
