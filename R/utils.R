get_data_file <- function(filename, data_folder="data"){

    # First check locally if the file exists in data folder
    if(file.exists(file.path(data_folder, filename))){
        path <- file.path(data_folder, filename)
    }else{
        # look into installed package
        path <- system.file(data_folder, filename, package = "chinatracker")
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

#fix English province names
fix_province_names <- function(x) {
  x %>% recode('Nei Mongol'='Inner Mongolia', 'Ningxia Hui'='Ningxia', 'Xinjiang Uygur'='Xinjiang', 'Xizang'='Tibet')
}
