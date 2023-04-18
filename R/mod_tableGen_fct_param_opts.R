stat_options <- function(block, datalist, ...) {
  UseMethod("stat_options", block)
}

stat_options.default <- function(block, datalist, ...) {
  if (is.null(block$stat_options))
    block$stat_options <- block$stat_selection
  
  block
}

stat_options.avisit <- function(block, datalist, ...) {
  avisits <- 
    datalist[[block$data]] %>%
    dplyr::filter(stringr::str_detect(toupper(AVISIT), "UNSCHEDULED", negate = TRUE),
                  AVISIT != "") %>%
    dplyr::distinct(AVISIT, AVISITN) %>%
    varN_fctr_reorder() %>%
    dplyr::pull(AVISIT) %>%
    get_levels() %>%
    as.list()
  
  block$stat_options <- avisits
  
  block
}

stat_options.avisit_lab <- function(block, datalist, ...) {
  avisits <-
    datalist[sapply(datalist, function(x) "PARAMCD" %in% colnames(x)) & substr(names(datalist), 1, 4) == "ADLB"] %>%
    purrr::map_dfc(dplyr::select, PARAMCD, AVAL, AVISIT, AVISITN) %>%
    dplyr::filter(PARAMCD == block$variable) %>%
    dplyr::filter(!is.na(AVAL),
             !is.na(AVISIT),
             !(AVISIT %in% c(" ", "")),
             stringr::str_detect(toupper(AVISIT),"UNSCHEDULED",negate = TRUE)
    ) %>%
    dplyr::distinct(AVISIT, AVISITN) %>%
    varN_fctr_reorder() %>%
    dplyr::pull(AVISIT) %>%
    get_levels() %>%
    as.list()
    
  block$stat_options <- avisits
  
  block
}

var_options <- function(block, datalist, ...) {
  UseMethod("var_options", block)
}

var_options.default <- function(block, datalist, ...) {
  if (is.null(block$var_options))
    block$var_options <- block$var_selection
  
  block
}

var_options.atpt <- function(block, datalist, ...) {
  atpts <-
    datalist[[block$data]] %>%
    dplyr::filter(PARAMCD == block$variable) %>%
    dplyr::distinct(ATPT, ATPTN) %>%
    varN_fctr_reorder() %>%
    dplyr::pull(ATPT) %>%
    get_levels() %>%
    {list(ATPT = as.list(.))}
  
  block$var_options <- atpts
  
  block
}
