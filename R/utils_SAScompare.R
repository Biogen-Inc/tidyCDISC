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


#' Function to filter rows from combined data
#'
#' @param datafile list of ADaM-ish dataframes
#'
#' @export
#'
filterData <- function(datafile, input_filter_df, filter_code) {
  processed_data <-

    select_dfs <- datafile[input_filter_df]

    # Separate out non BDS and BDS data frames. Note: join may throw some
    # warnings if labels are different between two datasets, which is fine!
    # Ignore
    non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )]
    bds <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]

    # Make CHG var doesn't exist, create the column and populate with NA
    PARAMCD_dat <- purrr::map(bds, ~ if(!"CHG" %in% names(.)) {purrr::update_list(., CHG = NA)} else {.})

    # Combine selected data into a 1 usable data frame
    if (!rlang::is_empty(PARAMCD_dat)) {
      all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from") %>% distinct(.keep_all = T)

      if (!rlang::is_empty(non_bds)){
        processed_data <- inner_join(non_bds %>% purrr::reduce(inner_join), all_PARAMCD)
      } else {
        processed_data <-all_PARAMCD
      }
    } else {
      processed_data <- non_bds %>% reduce(inner_join)
    }

    # evaluate the filtered code
    filtered_data <- eval(filter_code)
    return(filtered_data)
}


#' Function to compare the SAS table to the IDEA output table
#' 
#' @param sas_table SAS output 
#' @param tg_table IDEA output
#' 
#' @export
compareTables <- function(sas_table, tg_table) {
  
}
