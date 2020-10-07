#' Function to read the SAS list of user supplied data frames
#' 
#' @param datalist list of CDISC dataframes 
#' 
#' @export
readData <- function(filepaths) {
  purrr::map(filepaths, ~haven::read_sas(.x))
}


#' Function to bind data rows from the list of user supplied data frames
#' 
#' @param datafile list of ADaM-ish dataframes 
#' 
#' @export
#' 
combineData <- function(datafile) {
  # ac: not needed for this function
  # sasdata0 <- toupper(names(datalist))
  # my_loaded_adams <- names(which(sapply(toupper(names(datalist)),function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
  
  ADSL <- datafile$ADSL
  BDS <- datafile[sapply(datafile, function(x) "PARAMCD" %in% colnames(x))]
  
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
