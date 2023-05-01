filter_adsl <- function(recipe, ADSL) {
  UseMethod("filter_adsl", recipe)
}

filter_adsl.default <- function(recipe, ADSL) {
  dat <- ADSL
  msg <- ""
  list(data = dat, message = msg)
}

filter_adsl.stan_3 <- function(recipe, ADSL) {
  dat <- ADSL
  msg <- ""
  if("FASFL" %in% colnames(dat)){
    dat <- dat %>% filter(FASFL == 'Y')
    msg <- "Population Set: FASFL = 'Y'"
  } else {
    if("ITTFL" %in% colnames(dat)){
      dat <- dat %>% filter(ITTFL == 'Y')
      msg <- "Population Set: ITTFL = 'Y'"
    } else {
      msg <- "Variable 'FASFL' or 'ITTFL' doesn't exist in ADSL. STAN table not displayed because filter \"FASFL == 'Y'\" or \"ITTFL == 'Y'\"cannot be applied!"
      stop(msg)
    }
  }
  list(data = dat, message = msg)
}

filter_adsl.stan_18 <- 
  filter_adsl.stan_19 <- 
  filter_adsl.stan_20 <- 
  filter_adsl.stan_21 <- 
  filter_adsl.stan_22 <- 
  filter_adsl.stan_23 <- 
  filter_adsl.stan_24 <- 
  filter_adsl.stan_25 <- 
  filter_adsl.stan_26 <- 
  filter_adsl.stan_27 <- 
  filter_adsl.stan_28 <- 
  filter_adsl.stan_29 <- 
  filter_adsl.stan_30 <- 
  filter_adsl.stan_31 <- 
  filter_adsl.stan_32 <- 
  filter_adsl.stan_33 <- 
  filter_adsl.stan_34 <- 
  filter_adsl.stan_35 <- 
  filter_adsl.stan_36 <- 
  filter_adsl.stan_37 <- 
  filter_adsl.stan_38 <- 
  filter_adsl.stan_39 <- 
  filter_adsl.stan_41 <- 
  filter_adsl.stan_42 <- 
  filter_adsl.stan_43 <- 
  filter_adsl.stan_44 <- 
  filter_adsl.stan_45 <- 
  filter_adsl.stan_46 <- 
  filter_adsl.stan_47 <- 
  filter_adsl.stan_51 <- 
  filter_adsl.stan_52 <- 
  filter_adsl.stan_53 <- function(recipe, ADSL) {
  dat <- ADSL
  msg <- ""
  if("SAFFL" %in% colnames(dat)) {
    dat <- dat %>% filter(SAFFL == 'Y')
    msg <- "Population Set: SAFFL = 'Y'"
  } else {
    msg <- "Variable 'SAFFL' doesn't exist in ADSL. STAN table not displayed because filter \"SAFFL == 'Y'\" cannot be applied!"
    stop(msg)
  }
  list(data = dat, message = msg)
}