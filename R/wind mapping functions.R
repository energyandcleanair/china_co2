require(plyr)
require(zoo)
require(tidyverse)
require(lubridate)
require(magrittr)
require(readxl)
require(rcrea)
require(creahelpers)

readwind <- function(infile,
                     colNames = c('var', 'prov', 'type'),
                     lang='ZH') {
  read_xls(infile, skip=2, col_names = F) -> d
  read_xls(infile, n_max=1, col_names = F) %>% unlist -> h

  ncols <- h[-1] %>%
    sapply(function(x) gregexpr(":",x) %>% unlist %>% length) %>%
    max %>% add(1)
  h[-1] %>% gsub(": ", ":", .) %>%
    colsplit(":", paste0("V", 1:ncols)) -> md
  names(md)[1:length(colNames)] <- colNames

  read_xlsx("provincedict.xlsx") -> provdict
  read_xlsx("productdict.xlsx") -> productdict



  if(lang=='ZH') {
    md$prov %>% substr(1,2) -> md$prov
    provdict$provZH <- provdict$prov
    provdict$prov %>% substr(1,2) -> provdict$prov
  }

  left_join(md, provdict) %>%
    left_join(productdict) -> dict

  dict$Col <- paste0("C", 1:(ncol(d)-1))
  names(d) <- c('date', dict$Col)

  d %<>%
    gather(Col, Value, -date) %>%
    left_join(dict) %>% dplyr::select(-Col)

  d$date %>% as.numeric %>% openxlsx::convertToDate() -> d$date
  d %<>% subset(!is.na(date))

  if(lang=='EN') {
    d %<>% sel(-ends_with('EN')) %>%
      rename(provEN=prov, productEN=product)
    if(any(d$provEN %notin% adm1$provEN)) stop("Province names don't match")
  }

  return(d)
}

getwindvars <- function(infile, readfun = readxl::read_excel, skip=1) {
  readfun(infile, n_max=1, col_names = F, skip=skip) %>% unlist %>% '['(-1)
}

readwindEN <- function(infile,
                       colNames = c('var', 'prov'),
                       columnFilter = "",
                       columnExclude = "$^",
                       read_vardata = F,
                       readfun = readxl::read_excel,
                       skip=1,
                       zero_as_NA=F,
                       force_last_of_month=T,
                       drop_china_column=T) {
  readfun(infile, n_max=1, col_names = F, skip=skip) %>% unlist -> h
  
  if(any(grepl('YTD', h))) colNames %<>% c('type') %>% unique
  if(any(grepl('YoY', h))) colNames %<>% c('YoY') %>% unique
  if(drop_china_column) h %<>% gsub('(\\(DC\\) )?China: ', '', .)
  
  if(read_vardata) {
    readfun(infile, n_max=10, col_names = F, skip=skip) -> vardata
    vardata[is.na(as.numeric(vardata[[1]])), ] -> vardata
  }
  
  readfun(infile, skip=2+skip, col_names = F) -> d
  
  ind <- c(1, which(grepl(columnFilter, h) & !grepl(columnExclude, h))) %>% unique
  h[ind] -> h
  d[, ind] -> d
  if(read_vardata) vardata[, ind] -> vardata
  
  ncols <- h[-1] %>% sapply(function(x) gregexpr(":",x) %>% unlist %>% length) %>% max %>% add(1)
  h[-1] %>% tibble(col=.) %>% separate(col, paste0("V", 1:ncols), ":") -> md
  md %>% lapply(function(x) x %>% gsub("^ ", "", .) %>% gsub(" $", "", .)) -> md[,]
  names(md)[1:length(colNames)] <- colNames
  
  names(d) <- c('date', make.names(h[-1]))
  md$col <- names(d[-1])
  
  if(read_vardata)
    vardata[,-1] %>% t %>% data.frame %>% set_names(vardata[,1, drop=T]) %>% 
    bind_cols(md, .) -> md
  
  if(is.POSIXt(d$date)) d$date %<>% date
  if(is.character(d$date) & !all(is.na(as.numeric(d$date)))) d$date %<>% as.numeric %>% openxlsx::convertToDate()
  dups <- d %>% names %>% duplicated %>% which
  if(length(dups)>0) d[, -dups] -> d
  d %<>% subset(!is.na(date)) %>%
    gather(col, Value, -date) %>%
    left_join(md) %>% dplyr::select(-col) %>% 
    mutate(across(Value, as.numeric))
  
  if(all(c('type', 'YoY') %in% names(d))) {
    d %<>% mutate(YoY = ifelse(type=='YoY', 'YoY', YoY),
                  type = ifelse(type=='YoY', '', type)) 
  }
  
  if(force_last_of_month) day(d$date)<-days_in_month(d$date)
  if(zero_as_NA) d$Value[d$Value==0] <- NA
  
  return(d)
}

addmonths <- function(df) {
  alldates <- seq.Date(df$date[1]+1, max(df$date)+1, by='month')-1
  data.frame(date=alldates) %>% left_join(df) -> df
  df %>% dplyr::select_if(is.character) %>%
    select_if((function(x) !all(is.na(x)))) %>% 
    na.omit %>% distinct()-> id.vars
  if(complete.cases(df[, names(id.vars)]) %>% not %>% any)
    df[!complete.cases(df[, names(id.vars)]), names(id.vars)] <- id.vars
  
  return(df)
}

unYTD <- function(df) {
  df %>% 
    group_modify(function(df, group=NULL) {
      addmonths(df) -> df
      
      do.ytd=T
      if(!is.null(group$type)) {
        if(class(group$type) %in% c('character', 'factor') && all(group$type!='YTD'))  do.ytd=F
        if(class(group$type) == 'logical' && all(!df$type)) do.ytd=F
        
        if(length(unique(group$type))>1) stop('passed data of mixed YTD/non-YTD type')
      }
      
      #fix NAs in February
      ind <- which(month(df$date)==3 & is.na(lag(df$Value, 1)))
      
      if(length(ind)>0) {
        df$Value[ind-1] <- df$Value[ind]*(mean(df$Value[month(df$date)==2], na.rm=T) /
                                            mean(df$Value[month(df$date)==3], na.rm=T))
        
      }
      
      if(do.ytd) {
        df$prevdate <- lag(df$date, 1)
        df$prevval <- lag(df$Value, 1)
        df$Value1m <- df$Value - df$prevval
        df$Value1m[month(df$date)==1] <- df$Value[month(df$date)==1]
        
        ind <- which(month(df$date)==2 & is.na(df$prevval))
        df$Value1m[ind] <- df$Value[ind]/2
        ind %<>% subset(.>1)
        df$Value1m[ind-1] <- df$Value[ind]/2
      } else df$Value1m <- df$Value
      
      df %>% dplyr::select(-starts_with("prev"))
    })
}

seasonal <- function(df, year_range=0:9999) {
  df %>% 
    group_modify(function(df, ...) {
      df$month <- month(df$date)
      df %>% group_by(month) %>% 
        summarise(monthmean = mean(Value1m[year(date) %in% year_range], na.rm=T)) %>%
        mutate(monthadj = monthmean / mean(monthmean)) %>%
        right_join(df) %>%
        mutate(Value.seasonadj = Value1m / monthadj) %>%
        dplyr::select(-starts_with('month')) %>% 
        ungroup %>% arrange(date)
    })
}

roll12m <- function(df, months=12, outcol=paste0('Value', months, 'm'), incol='Value1m') {
  df %>% 
    group_modify(function(df, ...) {
      addmonths(df) -> df
      df[[outcol]] <- zoo::rollapplyr(df[[incol]], width=months, mean, na.rm=T, fill=NA)
      return(df)
    })
}

YoY <- function(df, daterange) {
  df$j <- format.Date(df$date, "%j")
  rangeyear <- daterange %>% last %>% year
  data.frame(rangedate = daterange,
             j = format.Date(daterange, "%j")) %>%
    right_join(df) %>%
    mutate(period = paste0("Y", rangeyear + year(date) - year(rangedate))) %>%
    dplyr::select(-j, -rangedate) -> df.per
  df.per %>%
    group_by_at(names(df.per)[!grepl("Value|date", names(df.per))]) %>%
    summarise(Value1m = mean(Value1m, na.rm=T)) %>%
    filter(period != "YNA")
}

get.yoy <- function(values, dates,
                    type='relative',
                    fun=ifelse(type=='relative', (function(x1, x0) {x1/x0-1}),(function(x1, x0) {x1-x0}))) {
  dates -> targetdates
  
  day(targetdates)[day(targetdates)==29 & month(targetdates)==2] <- 28
  if(any(duplicated(targetdates))) dates -> targetdates
  
  targetdates -> basedates
  year(basedates) <- year(targetdates) - 1
  basevalues <- values[match(basedates, targetdates)]
  fun(values, basevalues)
}

get.yoy.df <- function(df, col='Value', datecol='date',
                       fun=function(x1, x0) {x1/x0-1}) {
  df[[datecol]] -> targetdates
  day(targetdates)[day(targetdates)==29 & month(targetdates)==2] <- 28
  if(any(duplicated(targetdates))) df[[datecol]] -> targetdates
    
  targetdates-> basedates
  year(basedates) <- year(basedates) - 1
  baseconcs <- df[[col]][match(basedates, targetdates)]
  df$YoY <- fun(df[[col]], baseconcs)
  return(df)
}

#back-calculate revised values based on next year's values and YoY numbers
unYoY <- function(df, col='Value', outcol='Value',
                  datecol='date') {
  df %>% 
    group_modify(function(df, group) {
      if(is.null(df[[outcol]])) df[[outcol]] <- NA
      
      process_dates <- df[[datecol]] %>% unique %>% sort(decreasing = T)
      
      for(d in as.character(process_dates)) {
        d %<>% as.Date()
        next.dt = d
        year(next.dt) = year(d) + 1
        next.val = df[[col]][df$date == next.dt & !df$YoY]
        next.yoy = df[[col]][df$date == next.dt & df$YoY]
        cur.val.ind = which(df$date == d & !df$YoY)
        checkval = next.val + next.yoy + cur.val.ind
        
        if(length(checkval) == 1) {
          if(!is.na(checkval))
            df[[outcol]][cur.val.ind] <- next.val / (1+next.yoy/100)
          message('adjusting ', df[cur.val.ind, ] %>% select(c(is.character, is.Date)))
        }
      }
      return(df)
    })
}

translateSources <- function(x) {
  if(lang=='ZH')
    x <- case_when(grepl('Hydro', x)~'水电',
                   grepl('Nuclear', x)~'核电',
                   grepl('Solar', x)~'太阳能',
                   grepl('Thermal', x)~'火电',
                   grepl('Wind', x)~'风电')
  x
}

translateProvinces <- function(x) {
  read_xlsx('data/provincesZH.xlsx') -> provdict
  
  if(lang=='ZH')
    x <- provdict$ProvinceZH[match(x, provdict$Province)]
  
  return(x)
}

translateFuels <- function(x) {
  recode_factor(x,
                'Steam Coal'='动力煤',
                'Natural Gas'='天然气',
                'Crude Oil'='石油',
                'Coking Coal'='焦煤',
                'Cement'='水泥',
                'Oil Products'='成品油')
}

monthlab <- function(x) {
  if(lang=='EN') x %<>% format.Date('%b')
  if(lang=='ZH') x %<>% month() %>% paste0('月')
  return(x)
}

yearlab <- function(x) {
  if(is.Date(x) | is.POSIXt(x)) x %<>% year
  if(lang=='ZH') x %<>% paste0('年')
  x
}

#ENtoZH <- function(x) {
#  recode(x,
#         "Cement"="水泥",
#         "Crude Steel"="粗钢",
#         "Non-ferrous Metals"="有色金属",
#         "Pig Iron"="生铁",
#         "Steel Products"="钢材",
#         "Thermal Power"="火电")
#}
