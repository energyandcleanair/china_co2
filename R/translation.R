#the trans function in this repo has been replaced by creahelpers::trans
trans_old <- function(x,
                  lang=get("lang", .GlobalEnv),
                  trans_file = get_data_file('label_translations.xlsx'),
                  wrap_chars=NULL,
                  ignore.case=T,
                  when_missing='warn') {

  if(lang=='EN') return(x)

  read_xlsx(trans_file) -> dict

  if(!is.null(wrap_chars)) dict$ZH %<>% strsplit_lang(width=wrap_chars, lang=lang)

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

translateSources <- function(x, lang=get("lang", .GlobalEnv)) {
  if(lang=='ZH')
    x <- case_when(grepl('Hydro', x)~'\u6c34\u7535',
                   grepl('Nuclear', x)~"\u6838\u7535",
                   grepl('Solar', x)~'\u592a\u9633\u80fd\u53d1\u7535',
                   grepl('Thermal', x)~"\u706b\u7535",
                   grepl('Wind', x)~"\u98ce\u7535")
  x
}

translateProvinces <- function(x, lang=get("lang", .GlobalEnv)) {
  read_xlsx(get_data_file('provincesZH.xlsx')) -> provdict

  if(lang=='ZH')
    x <- provdict$ProvinceZH[match(x, provdict$Province)]

  return(x)
}

translateFuels <- function(x, lang=get("lang", .GlobalEnv)) {
  recode_factor(x,
                'Steam Coal'='\u52a8\u529b\u7164',
                'Natural Gas'='\u5929\u7136\u6c14',
                'Crude Oil'='\u77f3\u6cb9',
                'Coking Coal'='\u7126\u7164',
                'Cement'='\u6c34\u6ce5',
                'Oil Products'='\u6210\u54c1\u6cb9')
}

monthlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') x %<>% format.Date('%b')
  if(lang=='ZH') x %<>% month() %>% paste0('\u6708')
  return(x)
}

yearlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lubridate::is.Date(x) | lubridate::is.POSIXt(x)) x %<>% year
  if(lang=='ZH') x %<>% paste0('\u5e74')
  x %>% as.character()
}

monthyearlab <- function(x, lang=get('lang', envir=.GlobalEnv), english_format='%b %Y') {
  if(lang=='EN') x %<>% format.Date(english_format)
  if(lang=='ZH') x <- glue("{year(as.Date(x))}\u5e74{month(as.Date(x))}\u6708")
  return(x)
}

convert_value <- function(x, original_unit, lang=get('lang', envir=.GlobalEnv)) {
  x * case_when(original_unit %in% c('mwh')~1/1000,
                lang=='ZH'~1,
                original_unit %in% c("10MW", "10000 tons", '10000 kw', '10000 units')~1/100,
                original_unit %in% c('100M cu.m', '100 million kwh', "100Mt")~1/10)
}

unit_label <- function(original_unit, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='ZH') {
    new_unit <- case_when(original_unit == "10MW"~"\u4e07\u5343\u74e6",
                          original_unit == "mwh"~"\u5409\u74e6\u65f6",
                          original_unit == "10000 tons"~"\u4e07\u5428",
                          original_unit == "100Mt"~"\u4ebf\u5428",
                          original_unit=='10000 kw'~"\u4e07\u5343\u74e6",
                          original_unit=='100 million kwh'~"\u4ebf\u5343\u74e6\u65f6",
                          original_unit=='100M cu.m'~"\u4ebf\u7acb\u65b9\u7c73",
                          original_unit=='10000 units'~"\u4e07\u8f86")
  }

  if(lang=='EN') {
    new_unit <- case_when(original_unit == "10MW"~'GW',
                          original_unit == "mwh"~'GWh',
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
  if(lang == 'ZH'){
    theme(text = element_text(face = 'bold', family = 'Noto Sans'))
  } else {
    theme()
  }


  # case_when(lang=='ZH'~list(theme(text=element_text(family='PingFang SC'),
  #                                 plot.title = element_text(size=rel(2), margin=margin(c(20,12,16,12))))),
  #           T~list(theme()))
}

strsplit_lang <- function(x, width, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') return(stringr::str_wrap(x, width=width))

  starts <- seq(1, nchar(x), by=width) %>% c(nchar(x)) %>% unique

  # chop it up
  out <- character()
  for(i in 1:(length(starts)-1)) {
    if(substr(x, starts[i], starts[i]) %in% c("，", "\u3002", "：")) starts[i] %<>% add(1)
    out[i] <- substr(x, starts[i], starts[i+1])
  }
  out %>% paste(collapse='\n')
}
