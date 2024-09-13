source('load_package.R')


focus_month <- today() %>% subtract(30) %>% "day<-"(1)

output_dir <- paste0("outputs/monthly_snapshot_", format(as.Date(focus_month), "%Y_%m"))

dir.create(output_dir, showWarnings = F, recursive = T)

# preload air quality data
aq <- get_aq(start_date = ymd("2022-01-01"), update_data = T)
aq_dw <- get_deweathered_aq(china_admin_capitals, start_date = ymd("2022-01-01"), update_data = T)

target_lang = 'EN'
#target_lang = 'ZH'

# set lang in global environment
assign("lang", target_lang, envir = .GlobalEnv)

capacity_plots(focus_month = focus_month,
               output_dir=output_dir,
               lang=target_lang)

industry_output_plots(plots=NULL, #list of plots to make, NULL to use default defined within the function
                      include_yoy_labels=F, #include labels with year-on-year growth in plots?
                      skip_yoy_adjustment = 'Copper|Glass|Chemical Fibers|Solar$', #these products aren't retroactively adjusted to fit reported yoy numbers because there are anomalies
                      focus_month = focus_month,
                      output_dir=output_dir,
                      lang=target_lang)

steel_indicator_plots(start_year=year(today())-6, #first year shown in plots
                      output_dir=output_dir,
                      lang=target_lang)

fuel_supply_plots(focus_month = focus_month,
                  output_dir=output_dir,
                  lang=target_lang)

air_quality_plots(focus_month = focus_month, update_data = F, aq = aq, aq_dw = aq_dw,
                  output_dir=output_dir,
                  lang=target_lang)

aq_compliance_plots(cities = china_admin_capitals,
                    one_month_plots=T,
                    update_data = T,
                    output_dir=output_dir,
                    lang=target_lang)


last_month <- focus_month %>% 'day<-'(days_in_month(.))
source('project_workflows/power generation plots 2024.R')

shared_dir=file.path("G:/Shared drives/CREA-China/monthly snapshot",basename(output_dir))

dir.create(shared_dir)

list.files(path=output_dir, full.names=T) %>%
  file.copy(., file.path(shared_dir, basename(.)), overwrite=T)
