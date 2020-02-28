library(haven)
library(tidyverse)

allowed_operators <- c(">", ">=", "==", "<=", "<", "!=") %>% 
  set_names() %>% 
  map(match.fun)

# This is a list of two dataframe
# ADSL
# ADVS
# Taken from 
dd <- readRDS("tests/data/test_data.RDS")

ADSL <- dd$data$ADSL
BDS <- dd$data[sapply(dd$data, function(x) "PARAMCD" %in% colnames(x))]

PARAMCD <- map(BDS, ~ if(!"CHG" %in% names(.)) update_list(., CHG = NA) else .)

# Bind all the PARAMCD files 
all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
    arrange(SUBJID, AVISITN, PARAMCD) %>% 
    select(USUBJID, SUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
    #distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE) 
  
# Join ADSL and all_PARAMCD
test_data <- inner_join(ADSL, all_PARAMCD, by = "USUBJID")

#########################################################################
# Filtered Data
#########################################################################

# This function should just live in one place
# But is a little different in what it accepts for testing environment


filtering_expr <- function(input_filtering, input_condition, input_filt_grp) {
  column <- rlang::sym(input_filtering)
  operator <- allowed_operators[[input_condition]]
  if (is.null(operator)) {
    rlang::abort(glue::glue("Can't use operator `{input$condition}`"))
  }
  
  if (grepl("[A-Za-z]", dd$data$ADSL[[input_filtering]][1])) {
    value <- input_filt_grp
  } else {
    value <- as.numeric(input_filt_grp)
  }
  
  call <- rlang::call2(operator, column, value)
  rlang::as_quosure(call, env = emptyenv())
}


test_data_filtered <- test_data %>% dplyr::filter(!!filtering_expr("COUNTRY", "==", "CAN"))

