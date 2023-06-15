trans <- function(x, 
                  lang=get('lang', envir=.GlobalEnv), 
                  trans_file = 'data/label translations.xlsx',
                  wrap_chars=NULL,
                  ignore.case=T,
                  when_missing='warn') {
  if(lang=='EN') return(x)
  
  read_xlsx(trans_file) -> dict
  
  if(!is.null(wrap_chars)) dict$ZH %<>% strsplit_lang(width=wrap_chars)
  
  if(ignore.case) {
    x %<>% tolower
    dict$EN %<>% tolower
  }
  
  dictvect <- dict$ZH
  names(dictvect) <- dict$EN
  
  #identify values not translated
  missing <- x %whichnotin% dict$EN
  if(length(missing)>0) {
    msg = warning(paste('these values were not matched:', paste(missing, collapse='; ')))
    if(when_missing=='warn') warning(msg)
    if(when_missing=='stop') stop(msg)
  }
  
  if(is.character(x)) x %<>% recode(!!!dictvect)
  if(is.factor(x)) x %<>% recode_factor(!!!dictvect, ordered=T)
  
  x
}

translateSources <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='ZH')
    x <- case_when(grepl('Hydro', x)~'水电',
                   grepl('Nuclear', x)~'核电',
                   grepl('Solar', x)~'太阳能发电',
                   grepl('Thermal', x)~'火电',
                   grepl('Wind', x)~'风电')
  x
}

translateProvinces <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  read_xlsx('data/provincesZH.xlsx') -> provdict
  
  if(lang=='ZH')
    x <- provdict$ProvinceZH[match(x, provdict$Province)]
  
  return(x)
}

translateFuels <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  recode_factor(x,
                'Steam Coal'='动力煤',
                'Natural Gas'='天然气',
                'Crude Oil'='石油',
                'Coking Coal'='焦煤',
                'Cement'='水泥',
                'Oil Products'='成品油')
}

monthlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') x %<>% format.Date('%b')
  if(lang=='ZH') x %<>% month() %>% paste0('月')
  return(x)
}

yearlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(is.Date(x) | is.POSIXt(x)) x %<>% year
  if(lang=='ZH') x %<>% paste0('年')
  x
}

convert_value <- function(x, original_unit, lang=get('lang', envir=.GlobalEnv)) {
  x * case_when(lang=='ZH'~1,
                original_unit %in% c("10MW", "10000 tons", '10000 kw', '10000 units')~1/100,
                original_unit %in% c('100M cu.m', '100 million kwh', "100Mt")~1/10)
}

unit_label <- function(original_unit, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='ZH') {
    new_unit <- case_when(original_unit == "10MW"~'万千瓦',
                          original_unit == "10000 tons"~'万吨',
                          original_unit == "100Mt"~'亿吨',
                          original_unit=='10000 kw'~'万千瓦',
                          original_unit=='100 million kwh'~'亿千瓦时',
                          original_unit=='100M cu.m'~'亿立方米',
                          original_unit=='10000 units'~'万辆')
  }
  
  if(lang=='EN') {
    new_unit <- case_when(original_unit == "10MW"~'GW',
                          original_unit == "10000 tons"~'Mt',
                          original_unit=='10000 kw'~'GW',
                          original_unit == "100Mt"~'Gt',
                          original_unit=='100 million kwh'~'TWh',
                          original_unit=='100M cu.m'~'bcm',
                          original_unit=='10000 units'~'million units')
  }
  new_unit
}

lang_theme <- function(lang=get('lang', envir=.GlobalEnv)) {
  case_when(lang=='ZH'~list(theme(text=element_text(family='Source Sans'),
                                  plot.title = element_text(size=rel(2), margin=margin(c(20,12,16,12))))),
            T~list(theme()))
}

strsplit_lang <- function(x, width, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') return(stringr::str_wrap(x, width=width))
  
  starts <- seq(1, nchar(x), by=width) %>% c(nchar(x)) %>% unique
  
  # chop it up
  out <- character()
  for(i in 1:(length(starts)-1)) {
    if(substr(x, starts[i], starts[i]) %in% c("，", "。","：")) starts[i] %<>% add(1)
    out[i] <- substr(x, starts[i], starts[i+1])
  }
  out %>% paste(collapse='\n')
}