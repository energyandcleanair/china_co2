get_data_file <- function(filename, data_folder="inst/extdata", inst_data_folder=gsub("inst/", "", data_folder)){

  # First check locally if the file exists in data folder
  if(file.exists(file.path(data_folder, filename))){
    path <- file.path(data_folder, filename)
  }else{
    # look into installed package
    path <- system.file(inst_data_folder, filename, package = "chinatracker")
  }

  if(!file.exists(path) | (path=="")){
    stop(paste0("File ", filename, " not found in ", data_folder, " folder or in installed package"))
  }

  return(path)
}

fuel_cols = rcrea::crea_palettes$CREA[c(1, 4, 2, 6, 5)]
names(fuel_cols) = c('Hydro', 'Solar', 'Wind', 'Thermal', 'Nuclear')

read_bp <- function(file_path,
                    sheet_name=NULL,
                    year=2022,
                    startRow=3, ignore.case=T, read_multiple=F) {
  require(readxl)
  excel_sheets(file_path) -> sheets

  if(is.null(sheet_name)) {
    message(paste('no sheet given. options:', paste(sheets, collapse='; ')))
    return(sheets)
  }

  if(sheet_name %notin% sheets) sheet_name = grep(sheet_name, sheets, value=T, ignore.case=ignore.case)
  if(length(sheet_name)>1 & !read_multiple) stop(paste('sheet_name corresponds to more than one sheet: ', paste(sheet_name, collapse='; ')))
  if(length(sheet_name)==0) stop(paste('sheet_name does not match a sheet. options: ', paste(sheets, collapse='; ')))

  lapply(sheet_name,
         function(sn) {
           read_xlsx(file_path,
                     sheet=sn, #rowIndex=NULL,
                     skip=startRow-1,
                     .name_repair = function(x) x %>% make.names %>% make.unique) -> d

           names(d)[1] <- 'country'
           d$is.country <- with(d,!(is.na(country) | grepl("Other|Total|Africa", country)))
           d$is.country[d$country=="South Africa"] <- T
           d$is.country[d$country=="Central America"] <- F
           world.total.index <- match("Total World", d$country)
           if(is.na(world.total.index))
             world.total.index <- grep("Total", d$country) %>% tail(1)
           d$is.country[world.total.index:nrow(d)] <- F
           last.data.row = d[[paste0('X', year-1)]] %>% is.na() %>% not %>% which %>% max
           d <- d[1:last.data.row,]
           EU.index <- grep("European Union", d$country)
           d$country[EU.index] <- "EU"
           d$is.country[EU.index] <- T
           d$country[d$country=="United Kingdom"] <- "UK"
           d$country[grep("Russia", d$country)] <- "Russia"

           d %>% select(-matches("X[0-9]*\\.")) %>%
             filter(!is.na(country)) %>%
             pivot_longer(starts_with('X'), names_to='year', values_transform=list(value=as.numeric)) %>%
             mutate(across(value, as.numeric), across(year, force_numeric),
                    variable=sn)
         }) -> d.out

  if(!read_multiple) d.out <- d.out[[1]]
  return(d.out)
}

#fix dumb English province names used by GADM
fix_province_names <- function(x) {
  x %>% recode('Nei Mongol'='Inner Mongolia', 'Ningxia Hui'='Ningxia', 'Xinjiang Uygur'='Xinjiang', 'Xizang'='Tibet')
}

check_dates <- function(data, obv_date_threshold = Sys.Date() - 30, file_name,
                        check_dates_stop = F){
  problem_data <- NA
  max_update_date <- NA
  min_update_date <- NA

  max_date <- max(data$date)
  if(max_date < Sys.Date() - 60){
    warning(glue('Data in file "{file_name}" is not up to date. Last row date is {max_date}'))
  }

  if('Update' %in% colnames(data)){
    min_update_date <- min(data$Update)
    max_update_date <- max(data$Update)

    if(max_update_date < Sys.Date() - 60 | min_update_date < Sys.Date() - 60){
      problem_data <- data %>% filter(Update < Sys.Date() - 60,
                                      !Name %in% ignore_names) %>%
        distinct(Name) %>%
        pull

      if(length(problem_data != 0)){
        warning(paste('Problematic data:', paste(problem_data, collapse = ', ')))

        if(check_dates_stop){
          stop(glue(paste('Data in file "{file_name}" is not up to date.',
                          'Lastest update date is {max_update_date}.',
                          'Earliest update date is {min_update_date}.')))
        } else {
          warning(glue(paste('Data in file "{file_name}" is not up to date.',
                             'Lastest update date is {max_update_date}.',
                             'Earliest update date is {min_update_date}.')))
        }
      } else {
        problem_data <- NA
      }
    }
  }

  return(tibble(file = file_name, latest_data = max_date, latest_update = max_update_date,
                earliest_update = min_update_date,
                problem_data = paste(problem_data, collapse = ', ')))
}

#download Ember data from: https://ember-climate.org/data-catalogue/monthly-electricity-data/
get_ember_monthly_data <- function(cached = get_data_file("monthly_full_release_long_format-4.csv"),
                                   file_url="https://ember-climate.org/app/uploads/2022/07/monthly_full_release_long_format-4.csv") {
  ember <- NULL

  try(ember <- read_csv(file_url))
  if(!is.null(ember)) ember %>% write_csv(cached)
  if(is.null(ember)) ember <- cached %>% read_csv

  return(ember)
}

ignore_names <- c(
  'China: Output: Power and Energy Storage Batteries (Lithium Manganate): YTD',
  'China: Output: Power and Energy Storage Batteries (Lithium Titanate): YTD',
  'China: Output: Power and Energy Storage Batteries (Lithium Manganate): YTD: YoY',
  'China: Output: Power and Energy Storage Batteries (Lithium Titanate): YTD: YoY'
)
