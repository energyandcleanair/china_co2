air_quality_plots <- function(focus_month=today() %>% subtract(30) %>% 'day<-'(1),
                              lang=parent.frame()$lang,
                              output_dir=get('output_dir', envir=.GlobalEnv),
                              update_data=T,
                              pollutants = c('no2', 'pm25', 'o3'),
                              aq_file = file.path('cached_data', 'city_air_quality_data.RDS'),
                              aq_dw_file = file.path('cached_data', 'deweathered_air_quality_data.RDS'),
                              gis_dir = '~/GIS/',
                              aq=NULL, aq_dw=NULL) {

  cities <- read_csv('https://api.energyandcleanair.org/cities?country=CN&format=csv')

  if(is.null(aq)) aq <- get_aq(start_date=ymd('2022-01-01'), update_data=update_data, aq_file=aq_file)
  if(is.null(aq_dw)) aq_dw <- get_deweathered_aq(china_admin_capitals, pollutants, update_data=update_data, aq_file=aq_dw_file)

  #read_csv('~/../Downloads/deweathered_mee_20230518.csv') %>% filter(pollutant=='o3', location_id %in% china_admin_capitals) %>%
  #  bind_rows(aq_dw) -> aq_dw

  #add city and pollutant names
  aq %<>% left_join(cities %>% select(location_id=id, city_name=name)) %>%
    mutate(pollutant_name = case_when(pollutant=='pm25'~'PM2.5', T~toupper(pollutant)),
           date=date(date))

  #combine the measured and deweathered datasets
  aq %>%
    mutate(type='measured') %>%
    bind_rows(aq_dw %>% mutate(type='deweathered')) ->
    aq_all

  #add province names
  adm1 <- get_adm(1, 'coarse')
  aq_all %<>% mutate(GID_1 = location_id %>% gsub('^[a-z]*_|_[a-z]*$', '', .) %>% toupper,
                     iso3c = substr(GID_1, 1, 3)) %>%
    inner_join(adm1@data %>% select(GID_1, NAME_1))

  #add city and province names in Chinese
  aq_all %<>% filter(iso3c=='CHN') %>% mutate(city_name = case_when(is.na(city_name)~location_id %>% gsub('_.*', '', .) %>% capitalize_first(), T~city_name))

  station_key <- read_csv('data/air_quality_station_codes.csv')

  # Add proper air_quality_station_codes.csv with ProvinceZH
  city_key <- station_key %>% distinct(city_name=CityEN, NAME_1=ProvinceEN, city_name_ZH=CityZH, NAME_1_ZH = ProvinceZH) %>%
    filter(is.na(city_name))

  aq_all %<>% inner_join(city_key) %>% dplyr::rename(city_name_EN=city_name, NAME_1_EN=NAME_1)

  if(lang == 'EN') aq_all %<>% mutate(city_name = city_name_EN, NAME_1 = NAME_1_EN)
  if(lang == 'ZH') aq_all %<>% mutate(city_name = city_name_ZH, NAME_1 = NAME_1_ZH)

  aq_all %>%
    filter(location_id %in% china_admin_capitals,
           month(date) == month(focus_month)) %>%
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
        scale_fill_gradientn(colors=crea_palettes$change[c(1:2,5:7)], guide='none') +
        theme_crea() +
        x_at_zero() +
        labs(x='', y=trans('µg/m3'), subtitle=toupper(trans(group$pollutant_name))) -> p_means

      plotdata %>%
        group_by(city_name, type) %>%
        reframe(yoy=value[year==year(focus_month)]/value[year==year(focus_month)-1]-1) ->
        yoy

      yoy %<>% group_by(city_name) %>%
        reframe(yoy = yoy[type=='measured']-yoy[type=='deweathered']) %>%
        mutate(type='influence of weather') %>%
        bind_rows(yoy)

      yoy$type %<>% recode(measured='total', deweathered='due to changes in emissions')

      yoy %<>% ungroup %>% arrange(yoy) %>%
        mutate(city_name=factor(city_name, city_name[type=='total']),
               pollutant_name=toupper(trans(group$pollutant_name)))

      guide_to_use <- case_when(group$pollutant_name=='NO2'~'legend', T~'none')

      ggplot(mapping=aes(city_name, yoy)) +
        geom_col(data=yoy %>% filter(type!='total'), aes(fill=trans(type))) +
        geom_point(data=yoy %>% filter(type=='total'), aes(shape=trans('year-on-year change')), size=3) +
        coord_flip() +
        theme_crea() +
        scale_fill_crea_d('change', col.index = c(5,2), name='', guide=guide_to_use) +
        scale_shape_discrete(guide=guide_to_use, name='') +
        scale_y_continuous(labels=scales::percent) +
        labs(subtitle=toupper(trans(group$pollutant_name)), x='', y='') -> p_yoy

      return(list(plot_means=p_means, plot_yoy=p_yoy, data_means=plotdata_means, data_yoy=yoy))
    }) -> plots

  make_pollution_plot <- function(plotlist, plot_title, rel_widths=1) {
    plot_grid(plotlist=plotlist,nrow=1, rel_widths=rel_widths) -> g
    title <- ggdraw() + draw_label(trans(plot_title), size=22, fontface='bold', color=unname(pal_crea['Dark.blue']))
    plot_grid(title, g, ncol=1, rel_heights=c(0.1, 1)) -> p
    quicksave(file.path(output_dir, paste0(plot_title, ', ', lang,'.png')), plot=p, footer_height=.03)
  }

  plot_title="Monthly average pollutant concentrations in province capitals"
  plots %>% lapply('[[', 'plot_means') %>% rev %>% make_pollution_plot(plot_title=plot_title)

  plots %>% lapply('[[', 'data_means') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, ', ', lang, '.csv')))


  plot_title="Year-on-year changes in pollutant concentrations in province capitals"
  plots %>% lapply('[[', 'plot_yoy') %>% rev() %>%
    make_pollution_plot(plot_title=plot_title, rel_widths = c(.25,.25,.5))

  plots %>% lapply('[[', 'data_yoy') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, '.csv')))


  #worst episodes (PM2.5, non-sandstorm PM2.5, O3)
  replace_nil <- function(x) { if(length(x)==1) {x} else {1} }
  aq_all %>% filter(year(date)==year(focus_month), source=='mee', type=='measured') %>%
    mutate(violation=value>=case_when(pollutant_name=='PM2.5'~75,pollutant_name=='O3'~100,pollutant_name=='NO2'~200)) %>%
    group_by(city_name, NAME_1, location_id, date) %>%
    mutate(PM_ratio=replace_nil(value[pollutant_name=='PM2.5']/value[pollutant_name=='PM10']),
           sand_storm=value[pollutant_name=='PM10']>150 & PM_ratio<.7 & value[pollutant_name=='NO2']<20 & value[pollutant_name=='SO2']<20) ->
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
  "fuzhou_chn.16_1_cn",
  "kunming_chn.30_1_cn",
  "hohhot_chn.19_1_cn"
)


convert_dt <- function(x) {
  if(!is.POSIXt(x)) x %<>% ymd_hms
  x
}


get_aq <- function(start_date=ymd('2022-01-01'),
                   update_data=T,
                   cache_folder='cache',
                   aq_file=file.path(cache_folder, 'city_air_quality_data.RDS')) {

  message('read measured air quality data')
  dir.create(cache_folder, showWarnings = F, recursive = T)
  aq <- NULL

  if(!is.null(aq_file) & file.exists(aq_file)) {
    readRDS(aq_file) -> aq
    start_date <- aq$date %>% max %>% lubridate::date()
  }

  if(update_data) {
    seq.Date(start_date, today(), by='month') %>%
      pbapply::pblapply(function(start_date) {
        message(start_date)
        end_date=start_date %>% 'day<-'(days_in_month(start_date))
        read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                        "country=CN&pollutant=no2,pm25,pm10,so2&",
                        "date_from=",start_date,"&date_to=", end_date,
                        "&level=city&do_average=true&averaging_period=1d&format=csv")) %>%
          select(-any_of('...1')) %>%
          mutate(across(date, convert_dt), across(value, as.numeric)) -> conc_24h

        #read 8-hour max ozone
        read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                        "country=CN&pollutant=o3&process=city_8h_max_day_mad&",
                        "date_from=",start_date,"&date_to=", end_date,
                        "&level=city&do_average=true&averaging_period=1d&format=csv")) %>%
          select(-any_of('...1')) %>%
          mutate(across(date, convert_dt), across(value, as.numeric)) -> conc_8h

        #read 1-hour max NO2
        if(F) {
          read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                          "country=CN&pollutant=no2&process=city_max_day_mad&",
                          "date_from=",start_date,"&date_to=", end_date,
                          "&level=city&do_average=true&averaging_period=1d&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv")) -> conc_1h
        }

        bind_rows(conc_24h, conc_8h)
      }) %>% bind_rows(aq) %>% distinct(date, location_id, pollutant, .keep_all=T) -> aq

    if(!is.null(aq_file)) aq %>% saveRDS(aq_file)
  }
  return(aq)
}

get_deweathered_aq <- function(cities, pollutants = c('no2', 'pm25', 'o3'),
                               update_data=T,
                               cache_folder='cache',
                               aq_file=file.path(cache_folder, 'deweathered_air_quality_data.RDS')) {

  message('read deweathered air quality data')
  dir.create(cache_folder, showWarnings = F, recursive = T)
  aq <- NULL

  if(!is.null(aq_file) & file.exists(aq_file)) {
    readRDS(aq_file) -> aq
    start_date <- aq$date %>% max %>% date()
  }

  if(update_data) {
    pollutants %>%
      pbapply::pblapply(
        function(poll) {
          read_csv(
            sprintf('http://api.energyandcleanair.org/v1/measurements?location_id=%s&pollutant=%s&process_id=default_anomaly_2018_2099,default_anomaly_o3_2018_2099&variable=anomaly&format=csv',
                    paste0(cities, collapse = ","),
                    poll)
          )
        }
      ) %>% bind_rows %>%
      bind_rows(aq) %>%
      dplyr::rename(anomaly=value) -> aq

    if(!is.null(aq_file)) aq %>% saveRDS(aq_file)
  }
  return(aq)
}
