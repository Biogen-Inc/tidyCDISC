#' Function to merge the list of user supplied data frames
#' 
#' @param datalist list of CDISC dataframes 
#' 
#' @export
combineData <- function(filepaths) {
  purrr::map(.x, ~haven::read_sas(filepaths))
}


#' Function to merge the list of user supplied data frames
#' 
#' @param datalist list of CDISC dataframes 
#' 
#' @export
#' 
mergeData <- function(datalist) {
  sasdata0 <- toupper(names(datalist))
  sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
  
  ADSL <- reactive({ datafile$ADSL })
  BDS <- reactive({ datafile[sapply(datafile, function(x) "PARAMCD" %in% colnames(x))] })
  
  PARAMCD <- map(BDS, ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
  
  if (!is_empty(PARAMCD)) {
    # Bind all the PARAMCD files 
    all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
      arrange(USUBJID, AVISITN, PARAMCD) %>% 
      select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
    # Join ADSL and all_PARAMCD
    combined_data <- full_join(ADSL, all_PARAMCD, by = "USUBJID")
  } else {
    combined_data <- ADSL %>%
      mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
  }
  return(combined_data)
}

#' Function to compare the SAS table to the IDEA output table
#' 
#' @param sas_table SAS output 
#' @param tg_table IDEA output
#' 
#' @export
compareTables <- function(sas_table, tg_table) {
  
}
