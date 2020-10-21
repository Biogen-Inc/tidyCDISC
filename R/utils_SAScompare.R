

#' Function to read the SAS list of user supplied data frames
#' 
#' @param datalist list of CDISC dataframes 
#' 
#' @export
readData <- function(study_directory, file_names) {
  purrr::map(file_names, ~haven::read_sas(file.path(study_directory,.x))) %>%
  setNames(toupper(stringr::str_remove(file_names, ".sas7bdat")))
}


#' Function to bind data rows from the list of user supplied data frames
#' 
#' @param datafile list of ADaM-ish dataframes 
#' 
#' @export
#' 
combineData <- function(datafile) {
  
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
  
  combined_data <- varN_fctr_reorder(combined_data)
  
  return(combined_data)
}





#' Expands a collapsed column into multiple rows
#' 
#' Function that searches for a particular delimiter in data columns that would
#' then separate values on the left of the right into their own row for
#' increased machine readability.
#'
#' @param data either a tg_table or sas_table dataframe
#' @param sep a delimiter to search for when expanding rows
#'   
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr separate_rows
#' 
delim_expand_rows <- function(data, sep){
  
  if(sep == "\\(" & !("Mean (SD" %in% unique(data$Variable))){
    d <- data %>%
      filter(across(
        #-starts_with("id_")
        -c(id_num:Variable), function(col) stringr::str_detect(col, sep))) %>%
      tidyr::separate_rows(-c(id_num:Variable), sep = sep, convert = T)# convert works for sas
    
  } else {
    d <- data %>%
      filter(across(
        #-starts_with("id_")
        -c(id_num:id_rn), function(col) stringr::str_detect(col, sep))) %>%
      tidyr::separate_rows(-c(id_num:id_rn), sep = sep, convert = T) # convert works for sas
  }
  d <- d %>%
    group_by(id_num, id_rn) %>%
    mutate(var_rn = row_number()) %>%
    ungroup() %>%
    mutate(Variable = trimws(Variable, which = "both"))
  
  if(sep == "\\("){
    d <- d %>%
      mutate(Variable = ifelse(var_rn == 2 & Variable != "SD",paste(Variable, "(COL PCT)"), Variable))
  }
  return(d)
}

#' Organize SAS or tg table into a machine readable format for comparing
#'
#' @param data a sas or tg table
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'  
machine_readable <- function(data){
  data %>%
    mutate(var_rn = 1) %>%
    filter(across(-c(id_num:Variable), function(col) {
      !stringr::str_detect(col, "\\(") & !stringr::str_detect(col, "\\,") & !stringr::str_detect(col, "\\|")}
    )) %>%
    mutate(across(-c(id_num:Variable), as.numeric)) %>% # convert fields to numeric
    union(delim_expand_rows(data = data, sep = "\\|")) %>% # no | for sas table, but we'll do it anyway
    union(delim_expand_rows(data = data, sep = "\\,")) %>%
    union(
      delim_expand_rows( sep = "\\(", data = 
                           data %>% 
                           filter(Variable != "Mean (SD)") %>%
                           mutate(across(-starts_with("id_"), function(col) gsub(")", "", col)))
      )
    ) %>%
    union(
      delim_expand_rows( sep = "\\(", data = 
                           data %>% 
                           filter(Variable == "Mean (SD)") %>%
                           mutate(across(-starts_with("id_"), function(col) gsub(")", "", col)))
      )
    ) %>%
    arrange(id_num, id_rn, var_rn) %>%
    select(-id_rn,-var_rn)
}

#'  
#'  
#' Organize SAS table into a format for comparing
#'
#' @param data the sas_table dataframe, output from IDEA
#' @param as_is a logical which will determine if the table should be
#'   minimally prepared for comparison (leaving collapsed columns together) or
#'   prepared for optimal machine readability before comparison.
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' 
#' @export
#'   
prep_sas_table <- function(data, as_is = FALSE){
  sas_prepped <-
    data %>%
    mutate(id_num = as.numeric(factor(cat)),
           descr = trimws(descr, which = "both"))
  
  sas_labelled <-
    sas_prepped %>%
    # grab and move labels into dedicated field
    left_join(
      sas_prepped %>%
        filter(is.na(subcat) | subcat < 0) %>%
        distinct(id_num, descr) %>%
        group_by(id_num) %>%
        top_n(1) %>% # if there happens to be more than 1 na or negative subcat
        ungroup() %>%
        rename(id_descr = descr)
    ) %>%
    mutate(id_desc = ifelse(is.na(id_descr), banner, id_descr)) %>%
    filter(subcat > 0) %>%
    group_by(id_num) %>%
    mutate(id_rn = row_number()) %>% # id_rn = subcat) # has a strange numbering system
    ungroup() %>%
    select(id_num, id_desc, id_rn, Variable = descr, everything()) %>%
    select(-cat, -subcat, -region, -pg , -id_descr, -banner) %>%
    mutate(across(-c(id_num:Variable), function(col) trimws(col, "both")))
  
  if(as_is == F){
    # separate out values that have more than 1 value embedded in cell
    sas_comp_ready <- machine_readable(sas_labelled)
  }
  
  return(if(as_is) sas_labelled else sas_comp_ready)
}

#' Create new Generic Names for Columns with numeric table data
#'
#' @param dat a data frame
#' 
temp_col_rename <- function(dat){
  var_ind <- which(names(dat) == "Variable")
  orig <- names(dat)[(var_ind + 1):ncol(dat)]
  col_nums <- seq_len(ncol(dat) - var_ind)- 1
  col_nums[length(col_nums)] <- 99
  names(dat)[(var_ind + 1):ncol(dat)] <- paste0("col", col_nums)
  return(list(dat = dat, orig_names = orig))
}

#' Revert temporary column names back to the original; use with temp_col_rename
#'
#' @param dat a data frame
#' @param orig_grp_names a character vector of original col names
#'
revert_temp_colnames <- function(dat, orig_grp_names){
  var_ind <- which(names(dat) == "Variable")
  names(dat)[(var_ind + 1):ncol(dat)] <- orig_grp_names
  return(dat)
}


#' Organize Table Generator table into a format for comparing
#'
#' @param data the tg_table dataframe, output from IDEA
#' @param as_is a logical which will determine if the table should be minimally
#'   prepared for comparison (leaving collapsed columns together) or prepared
#'   for optimal machine readability before comparison.
#' @param num_dec the number of desired decimal places on all numeric columns;
#'   value will be passed to the 'digits' argument of the round() function
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' 
#' @export
#'   
prep_tg_table <- function(data, as_is = FALSE, num_dec = 1){
  
  tg00 <- data %>%
    mutate(id_num = as.numeric(factor(ID, levels = unique(data$ID)))) %>%
    filter(Variable != "Missing") %>%
    group_by(id_num) %>%
    mutate(id_rn = row_number()) %>%
    ungroup() %>%
    select(id_num, id_desc = ID, id_rn, everything())
  
  tg_renamed <- temp_col_rename(tg00)
  tg <- tg_renamed$dat
  
  if(as_is == F){
    tg_comp_ready <- machine_readable(tg) %>% revert_temp_colnames(tg_renamed$orig_names)
  }

  return(if(as_is) tg else tg_comp_ready)
}





#' Function to compare the SAS table to the IDEA output table
#' 
#' @param sas_table SAS output 
#' @param tg_table IDEA output
#' 
#' @export
compareTables <- function(sas_table, tg_table) {
  
}
