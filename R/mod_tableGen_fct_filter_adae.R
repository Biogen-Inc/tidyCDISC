filter_adae <- function(recipe, datalist, ADSL) {
  UseMethod("filter_adae", recipe)
}

filter_adae.default <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  list(data = dat, message = msg)
}

filter_adae.stan_25 <- 
  filter_adae.stan_26 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AESEV" %in% colnames(dat)){
    dat <- dat %>% filter(AESEV == 'SEVERE')
    msg <- "AESEV = 'SEVERE'"
  } else {
    msg <- "Variable 'AESEV' doesn't exist in ADAE. STAN table not displayed because filter \"AESEV = 'SEVERE'\" cannot be applied!"
    stop(msg)
  }
  
  if("TRTEMFL" %in% colnames(dat)){
    dat <- dat %>% filter(TRTEMFL == 'Y')
    msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
  } else {
    msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!")
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_29 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AREL" %in% colnames(dat)){
    dat <- dat %>% filter(AREL == 'RELATED')
    msg <- "AREL = 'RELATED'"
  } else {
    msg <- "Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!"
    stop(msg)
  }
  
  if("TRTEMFL" %in% colnames(dat)){
    dat <- dat %>% filter(TRTEMFL == 'Y')
    msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
  } else {
    msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!")
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_30 <- 
  filter_adae.stan_31 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AESER" %in% colnames(dat)){
    dat <- dat %>% filter(AESER == 'Y')
    msg <- "AESER = 'Y'"
  } else {
    msg <- "Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!"
    stop(msg)
  }
  
  if("TRTEMFL" %in% colnames(dat)){
    dat <- dat %>% filter(TRTEMFL == 'Y')
    msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
  } else {
    msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!")
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_33 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""

    if("AREL" %in% colnames(dat) & "AESER" %in% colnames(dat)){
    dat <- dat %>% filter(AREL == 'RELATED' & AESER == 'Y')
    msg <- "AREL = 'RELATED'<br/>AESER = 'Y'"
  } else if("AREL" %in% colnames(dat) & !("AESER" %in% colnames(dat))){
    dat <- dat %>% filter(AREL == 'RELATED')
    msg <- "AREL = 'RELATED'<br/>Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!"
    stop("Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!")
  } else if(!("AREL" %in% colnames(dat)) & "AESER" %in% colnames(dat)){
    dat <- dat %>% filter(AESER == 'Y')
    msg <- "Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!<br/>AESER = 'Y'"
    stop("Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!")
  } else{
    msg <- "Variables 'AREL' & 'AESER' do not exist in ADAE. STAN table not displayed because filters \"AREL = 'RELATED'\" and \"AESER = 'Y'\" cannot be applied!"
    stop(msg)
  }
  
  if("TRTEMFL" %in% colnames(dat)){
    dat <- dat %>% filter(TRTEMFL == 'Y')
    msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
  } else {
    msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!")
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_34 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AEACN" %in% colnames(dat)){
    dat <- dat %>% filter(AEACN == 'DRUG WITHDRAWN')
    msg <- "AEACN = 'DRUG WITHDRAWN'"
  } else{
    msg <- "Variable 'AEACN' doesn't exist in ADAE. STAN table not displayed because filter \"AEACN = 'DRUG WITHDRAWN'\" cannot be applied!"
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_36 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AEACNOTH" %in% colnames(dat)){
    dat <- dat %>%
      filter(stringr::str_detect(tolower(AEACNOTH),"withdrawal") &
               stringr::str_detect(tolower(AEACNOTH),"study"))
    msg <- "AEACNOTH Contains 'withdrawal' and 'study'"
  } else{
    msg <- "Variable 'AEACNOTH' doesn't exist in ADAE. STAN table not displayed because filter \"AEACNOTH Contains 'withdrawal' and 'study'\" cannot be applied!"
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_38 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("AEACN" %in% colnames(dat)){
    dat <- dat %>% filter(AEACN %in% c('DRUG INTERRUPTED', 'DRUG REDUCED', 'DOSE REDUCED', 'DRUG INCREASED', 'DOSE INCREASED'))
    msg <- "AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED')"
  } else{
    msg <- "Variable 'AEACN' doesn't exist in ADAE. STAN table not displayed because filter \"AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED')\" cannot be applied!"
    stop(msg)
  }
  
  list(data = dat, message = msg)
}

filter_adae.stan_39 <- function(recipe, datalist, ADSL) {
  dat <- clean_ADAE(datafile = datalist, ADSL = ADSL)
  msg <- ""
  
  if("TRTEMFL" %in% colnames(dat)){
    dat <- dat %>% filter(TRTEMFL == 'Y')
    msg <- "TRTEMFL = 'Y'"
  }else {
    msg <- "Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!"
    stop(msg)
  }
  
  list(data = dat, message = msg)
}
