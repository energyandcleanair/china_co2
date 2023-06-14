cities <- read_csv('https://api.energyandcleanair.org/cities?country=CN&format=csv')
gis_dir <- '~/GIS/'

aq_file <- file.path('cached_data', 'city_air_quality_data.RDS')

start_date <- ymd('2017-01-01')
aq <- NULL

if(file.exists(aq_file)) {
  readRDS(aq_file) -> aq
  start_date <- aq$date %>% max %>% date
}
  
seq.Date(start_date, today(), by='month') %>% 
  pbapply::pblapply(function(start_date) {
    end_date=start_date %>% 'day<-'(days_in_month(start_date))
    read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                    "country=CN&pollutant=no2,pm25,pm10,so2&",
                    "date_from=",start_date,"&date_to=", end_date,
                    "&level=city&do_average=true&averaging_period=1d&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv")) -> conc_24h
    
    #read 8-hour max ozone
    read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                    "country=CN&pollutant=o3&process=city_8h_max_day_mad&",
                    "date_from=",start_date,"&date_to=", end_date,
                    "&level=city&do_average=true&averaging_period=1d&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv")) -> conc_8h
    
    #read 1-hour max NO2
    if(F) {
      read_csv(paste0("https://api.energyandcleanair.org/measurements?",
                      "country=CN&pollutant=no2&process=city_max_day_mad&",
                      "date_from=",start_date,"&date_to=", end_date,
                      "&level=city&do_average=true&averaging_period=1d&sort_by=asc(location_id),asc(pollutant),asc(date)&format=csv")) -> conc_1h
    }
    
    bind_rows(conc_24h, conc_8h)
  }) %>% bind_rows(aq) %>% distinct(date, location_id, pollutant, .keep_all=T) -> aq

aq %>% saveRDS(aq_file)

aq_dw <- read_csv('~/../Downloads/deweathered_mee_20230518.csv')

china_admin_capitals <- c("Beijing", "Chongqing", "Shanghai", "Tianjin", "Lhasa", "Hohhot", "Nanning", "Shijiazhuang", "Taiyuan", 
                          "Xining", "Zhengzhou", "Wuhan", "Haikou", "Changsha", "Nanjing", "Shenyang", "Hefei", "Fuzhou", "Nanchang", 
                          "Changchun", "Harbin", "Hohhot", "Guiyang", "Kunming", "Nanning", "Lanzhou", "Yinchuan", "Xining", "Jinan", 
                          "Nanjing", "Hefei", "Fuzhou")

#add city and pollutant names
aq %<>% left_join(cities %>% select(location_id=id, city_name=name)) %>% 
  mutate(pollutant_name = case_when(pollutant=='pm25'~'PM2.5', T~toupper(pollutant)),
         date=date(date))

#add province names
adm1 <- get_adm(1, 'coarse')
aq %<>% mutate(GID_1 = location_id %>% gsub('^[a-z]*_|_[a-z]*$', '', .) %>% toupper) %>% 
  inner_join(adm1@data %>% select(GID_1, NAME_1))


aq %>%
  mutate(type='measured') %>% 
  bind_rows(aq_dw %>% mutate(type='deweathered')) ->
  aq_all

aq_all %>% 
  filter(city_name %in% china_admin_capitals,
         month(date) == month(focus_month)) %>%
  group_by(city_name, pollutant_name, type, year=year(date)) %>%
  summarise(across(c(value, anomaly), mean)) ->
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
      labs(x='', y='µg/m3', subtitle=group$pollutant_name) -> p_means
    
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
             pollutant_name=group$pollutant_name)
    
    guide_to_use <- case_when(group$pollutant_name=='NO2'~'legend', T~'none')
    
    ggplot(mapping=aes(city_name, yoy)) + 
      geom_col(data=yoy %>% filter(type!='total'), aes(fill=type)) +
      geom_point(data=yoy %>% filter(type=='total'), aes(shape='year-on-year change'), size=3) + 
      coord_flip() +
      theme_crea() +
      scale_fill_crea_d('change', col.index = c(5,2), name='', guide=guide_to_use) +
      scale_shape_discrete(guide=guide_to_use, name='') +
      scale_y_continuous(labels=scales::percent) + 
      labs(subtitle=group$pollutant_name, x='', y='') -> p_yoy
    
    return(list(plot_means=p_means, plot_yoy=p_yoy, data_means=plotdata_means, data_yoy=yoy))
  }) -> plots

make_pollution_plot <- function(plotlist, plot_title, rel_widths=1) {
  plot_grid(plotlist=plotlist,nrow=1, rel_widths=rel_widths) -> g
  title <- ggdraw() + draw_label(plot_title, size=22, fontface='bold', color=unname(pal_crea['Dark.blue']))
  plot_grid(title, g, ncol=1, rel_heights=c(0.1, 1)) -> p
  quicksave(file.path(output_dir, paste0(plot_title, '.png')), plot=p, footer_height=.03)
}

plot_title="Monthly average pollutant concentrations in province capitals"
plots %>% lapply('[[', 'plot_means') %>% rev %>% make_pollution_plot(plot_title=plot_title)

plots %>% lapply('[[', 'data_means') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, '.csv')))


plot_title="Year-on-year changes in pollutant concentrations in province capitals"
plots %>% lapply('[[', 'plot_yoy') %>% rev() %>% 
  make_pollution_plot(plot_title=plot_title, rel_widths = c(.25,.25,.5))
  
plots %>% lapply('[[', 'data_yoy') %>% bind_rows() %>% write_csv(file.path(output_dir, paste0(plot_title, '.csv')))


#worst episodes (PM2.5, non-sandstorm PM2.5, O3)
aq %>% filter(year(date)==year(focus_month), source=='mee') %>%
  mutate(violation=value>=case_when(pollutant_name=='PM2.5'~75,pollutant_name=='O3'~100,pollutant_name=='NO2'~200)) %>%
  group_by(city_name, NAME_1, location_id, date) %>% 
  mutate(PM_ratio=value[pollutant_name=='PM2.5']/value[pollutant_name=='PM10'],
         sand_storm=value[pollutant_name=='PM10']>150 & PM_ratio<.7 & value[pollutant_name=='NO2']<20 & value[pollutant_name=='SO2']<20) ->
  aq_episodes
  
bind_rows(aq_episodes %>% filter(pollutant_name=='PM2.5') %>% mutate(value = ifelse(sand_storm, value, 0), pollutant_name='sandstorms (PM2.5)'),
          aq_episodes %>% filter(pollutant_name=='PM2.5') %>% mutate(value = ifelse(!sand_storm, value, 0), pollutant_name='PM2.5 (excluding sandstorms)'),
          aq_episodes %>% filter(pollutant_name!='PM2.5')) ->
  aq_episodes_w_sandstorms

aq_episodes_w_sandstorms %>% filter(grepl('PM2.5|O3|NO2', pollutant_name), !is.na(value)) %>%
  group_by(city_name, NAME_1, location_id, pollutant_name) %>% arrange(date) %>%
  mutate(value_rollmean = rollapplyr(value, width=7, FUN=mean, fill=NA),
         worst_day = rollapplyr(value, width=7, FUN=max, fill=NA)) %>%
  filter(date>=focus_month, date<=focus_month+days_in_month(focus_month)+5) %>%
  group_by(city_name, NAME_1, location_id, pollutant_name) %>%
  slice_max(value_rollmean, n=1, with_ties = F) %>%
  group_by(pollutant_name) %>% 
  slice_max(value_rollmean, n=5) %>% 
  select(pollutant_name, city_name, NAME_1, date, value_rollmean, worst_day) %>%
  mutate(date = paste0(format(date-6, "%b %d"), " – ", format(date, "%b %d"))) %T>%
  write_excel_csv(file.path(output_dir, 'worst_episodes.csv'))


