

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
combineBDS <- function(datafile, ADSL) {
  
  BDS <- datafile[sapply(datafile, function(x) "PARAMCD" %in% colnames(x))]
  
  PARAMCD <- map(BDS, ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
  
  if (!is_empty(PARAMCD)) {
    # Bind all the PARAMCD files 
    all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
      arrange(USUBJID, AVISITN, PARAMCD) %>% 
      select(USUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG, data_from)
    # Join ADSL and all_PARAMCD
    combined_data <- inner_join(ADSL, all_PARAMCD, by = "USUBJID")
  } else {
    combined_data <- ADSL %>%
      mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
  }
  
  combined_data <- varN_fctr_reorder(combined_data) # add this after filter?
  
  return(combined_data)
}

#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param datafile list of ADaM-ish dataframes 
#' 
#' @export
#' 
cleanADAE <- function(datafile, ADSL) {
  if("ADAE" %in% names(datafile)){
    # find columns the ADAE & ADSL have in common (besides Usubjid), remove
    # them from the ADAE, so that the ADSL cols are used instead. Then join
    # on usubjid and re-order the colnames to match the adae
    adae_cols <- colnames(datafile$ADAE)
    common_cols <- dplyr::intersect(adae_cols, colnames(ADSL))
    com_cols_excp_u <- common_cols[common_cols != "USUBJID"]
    adae_adsl <- datafile$ADAE %>% 
      select(-one_of(com_cols_excp_u)) %>%
      inner_join(ADSL, by = "USUBJID")
    preferred_col_order <- c(adae_cols, dplyr::setdiff(colnames(ADSL), adae_cols))
    if(all(sort(colnames(adae_adsl)) == sort(preferred_col_order))){
      varN_fctr_reorder(adae_adsl[,preferred_col_order]) # add this after filter?
    } else {
      varN_fctr_reorder(adae_adsl)
    }
  } else {
    varN_fctr_reorder(ADSL)
  }
}

#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' 
numeric_stan_table <- function(input_recipe){
  ifelse(is.null(input_recipe) | input_recipe == "NONE", 
         0,
         as.numeric(gsub(" ","",gsub(":","",stringr::word(start = 2, substr(input_recipe, 1, 9)))))
  )
}


#' Function to pre-filter the ADSL depending on the stan table selected
#' 
#' @param data an ADSL
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' 
#' @export
#' 
prep_adsl <- function(ADSL, input_recipe) { #, stan_table_num
  stan_table_num <- numeric_stan_table(input_recipe)
  dat <- ADSL
  msg <- ""
  if(!is.null(input_recipe)){ # if recipe has initialized...
    if(stan_table_num == 5){
      if("ITTFL" %in% colnames(dat)){
        dat <- dat %>% filter(ITTFL == 'Y')
        msg <- "ITTFL = 'Y'"
      }else {
        msg <- "Variable 'ITTFL' doesn't exist in ADSL. Filter not applied!"
      }
    } else if(stan_table_num %in% c(18:39)){
      if("SAFFL" %in% colnames(dat)){
        dat <- dat %>% filter(SAFFL == 'Y')
        msg <- "SAFFL = 'Y'"
      } else{
        msg <- "Variable 'SAFFL' doesn't exist in ADSL. Filter not applied!"
      }
    }
  }
  
  return(list(data = dat, message = msg))
}

#' Function to pre-filter the ADAE depending on the stan table selected
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param data an ADSL
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' 
#' @export
#' 
prep_adae <- function(datafile, ADSL, input_recipe) { #, stan_table_num
  stan_table_num <- numeric_stan_table(input_recipe)
  dat <- cleanADAE(datafile = datafile, ADSL = ADSL)
  msg <- ""
  if(!is.null(input_recipe)){ # if recipe has initialized...
    
    if(stan_table_num %in% c(25, 26)){
        if("AESEV" %in% colnames(dat)){
          dat <- dat %>% filter(AESEV == 'SEVERE')
          msg <- "AESEV = 'SEVERE'"
        } else {msg <- "Variable 'AESEV' doesn't exist in ADAE. Filter AESEV = 'SEVERE' not applied!"}
    
    } else if(stan_table_num == 29){
        if("AREL" %in% colnames(dat)){
          dat <- dat %>% filter(AREL == 'RELATED')
          msg <- "AREL = 'RELATED'"
        } else {msg <- "Variable 'AREL' doesn't exist in ADAE. Filter AREL = 'RELATED' not applied!"}
        
    } else if(stan_table_num %in% c(30, 31)){
        if("AESER" %in% colnames(dat)){
          dat <- dat %>% filter(AESER == 'Y')
          msg <- "AESER = 'Y'"
        } else {msg <- "Variable 'AESER' doesn't exist in ADAE. Filter AESER = 'Y' not applied!"}
        
    } else if(stan_table_num == 33){
      if("AREL" %in% colnames(dat) & "AESER" %in% colnames(dat)){
        dat <- dat %>% filter(AREL == 'RELATED' & AESER == 'Y')
        msg <- "AREL = 'RELATED'<br/>AESER = 'Y'"
      } else if("AREL" %in% colnames(dat) & !("AESER" %in% colnames(dat))){
        dat <- dat %>% filter(AREL == 'RELATED')
        msg <- "AREL = 'RELATED'<br/>Variable 'AESER' doesn't exist in ADAE. Filter AESER = 'Y' not applied!"
      } else if(!("AREL" %in% colnames(dat)) & "AESER" %in% colnames(dat)){
        dat <- dat %>% filter(AESER == 'Y')
        msg <- "Variable 'AREL' doesn't exist in ADAE. Filter AREL = 'RELATED' not applied!<br/>AESER = 'Y'"
      } else{
        msg <- "Variables 'AREL' & 'AESER' do not exist in ADAE. Filters AREL = 'RELATED' and AESER = 'Y' not applied!"
      }
    } else if(stan_table_num == 34){
      if("AEACN" %in% colnames(dat)){
        dat <- dat %>% filter(AEACN == 'DRUG WITHDRAWN')
        msg <- "AEACN = 'DRUG WITHDRAWN'"
      } else{
        msg <- "Variable 'AEACN' doesn't exist in ADAE. Filter AEACN = 'DRUG WITHDRAWN' not applied!"
      }
    } else if(stan_table_num == 36){ #AEACNOTH contains 'Withdrawl" and "Study"
      if("AEACNOTH" %in% colnames(dat)){
        dat <- dat %>%
          filter(stringr::str_detect(tolower(AEACNOTH),"withdrawl") &
                   stringr::str_detect(tolower(AEACNOTH),"study"))
        msg <- "AEACNOTH Contains 'withdrawl' and 'study'"
      } else{
        msg <- "Variable 'AEACNOTH' doesn't exist in ADAE. Filter AEACNOTH Contains 'withdrawl' and 'study' not applied!"
      }
    } else if(stan_table_num == 38){
      if("AEACN" %in% colnames(dat)){
        dat <- dat %>% filter(AEACN %in% c('DRUG INTERRUPTED', 'DRUG REDUCED', 'DOSE REDUCED', 'DRUG INCREASED', 'DOSE INCREASED'))
        msg <- "AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED')"
      } else{
        msg <- "Variable 'AEACN' doesn't exist in ADAE. Filter AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED') not applied!"
      }
    } else if(stan_table_num == 39){
      if("TRTEMFL" %in% colnames(dat)){
        dat <- dat %>% filter(TRTEMFL == 'Y')
        msg <- "TRTEMFL = 'Y'"
      }else {
        msg <- "Variable 'TRTEMFL' doesn't exist in ADAE. Filter TRTEMFL = 'Y' not applied!"
      }
    }
    if(stan_table_num %in% c(25:26, 29:33)){
      if("TRTEMFL" %in% colnames(dat)){
        dat <- dat %>% filter(TRTEMFL == 'Y')
        msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
      } else {msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. Filter TRTEMFL = 'Y' not applied!")}
    }
  }
  
  return(list(data = dat, message = msg))
}





#' The smallest possible data set we could filter to semi-join later
#' 
#' @param datafile list of ADaM-ish dataframes 
#' 
#' @export
#' 
data_to_filter <- function(datafile, input_filter_df) {
  select_dfs <- datafile[input_filter_df]
  
  # Separate out non-BDS and BDS data frames. Note: join may throw some warnings
  # if labels are different between two datasets, which is fine! Just Ignore
  non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
  bds <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
  
  # Make CHG var doesn't exist, create the column and populate with NA
  PARAMCD_dat <- purrr::map(bds, ~ if(!"CHG" %in% names(.)) {purrr::update_list(., CHG = NA)} else {.})
  
  # Combine selected data into a 1 usable data frame
  if (!rlang::is_empty(PARAMCD_dat)) {
    all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from") %>% distinct(.keep_all = T)
    
    if (!rlang::is_empty(non_bds)){
      combined_data <- inner_join(non_bds %>% purrr::reduce(inner_join), all_PARAMCD)
    } else {
      combined_data <-all_PARAMCD
    }
  } else {
    combined_data <- non_bds %>% reduce(inner_join)
  }
  
  return(combined_data)
}



#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param datafile list of ADaM-ish dataframes 
#' 
#' @export
#' 
data_to_use_str <- function(x) {
  if (x == "ADAE") { ae_data }
  else bds_data
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
        -c(id_block:Variable), function(col) stringr::str_detect(col, sep))) %>%
      tidyr::separate_rows(-c(id_block:Variable), sep = sep, convert = T)# convert works for sas
    
  } else {
    d <- data %>%
      filter(across(
        #-starts_with("id_")
        -c(id_block:id_rn), function(col) stringr::str_detect(col, sep))) %>%
      tidyr::separate_rows(-c(id_block:id_rn), sep = sep, convert = T) # convert works for sas
  }
  d <- d %>%
    group_by(id_block, id_rn) %>%
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
make_machine_readable <- function(data, keep_orig_ids = FALSE){
  d <- data %>%
    mutate(var_rn = 1) %>%
    filter(across(-c(id_block:Variable), function(col) {
      !stringr::str_detect(col, "\\(") & !stringr::str_detect(col, "\\,") & !stringr::str_detect(col, "\\|")}
    )) %>%
    mutate(across(-c(id_block:Variable), as.numeric)) %>% # convert fields to numeric
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
    arrange(id_block, id_rn, var_rn) %>%
    rename(orig_id_rn = id_rn, orig_var_rn = var_rn) %>%
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>% # id_rn = subcat) # has a strange numbering system
    ungroup() %>%
    select(id_block, id_desc, id_rn, orig_id_rn, orig_var_rn, Variable, everything())
  
  if(keep_orig_ids == FALSE){
    d <- d %>% select(-orig_id_rn, -orig_var_rn)
  }
  return(d)
}

#'
#'
#' Organize SAS table into a format for comparing
#'
#' @param data the sas_table dataframe, output from IDEA
#' @param machine_readable a logical; should the table be prepared for optimal
#'   machine readability; that is, should cells with multiple values be pivoted
#'   to new rows
#' @param keep_orig_ids a logical; if machine_readability is desired, do you
#'   want to keep old id's that keep track of the values original position in
#'   the table
#' @param rm_desc_col a logical; should the description column be removed from
#'   the table (since they never match between base and compare)
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'
#' @export
#' 
prep_sas_table <- function(data,
                           machine_readable = TRUE,
                           keep_orig_ids = FALSE,
                           rm_desc_col = FALSE){
  sas_prepped <-
    data %>%
    mutate(id_block = as.numeric(factor(cat)),
           descr = trimws(descr, which = "both"))
  
  sas_labelled <-
    sas_prepped %>%
    # grab and move labels into dedicated field
    left_join(
      sas_prepped %>%
        filter(is.na(subcat) | subcat < 0) %>%
        distinct(id_block, descr) %>%
        group_by(id_block) %>%
        top_n(1) %>% # if there happens to be more than 1 na or negative subcat
        ungroup() %>%
        rename(id_descr = descr)
    ) %>%
    mutate(id_desc = ifelse(is.na(id_descr), banner, id_descr)) %>%
    filter(subcat > 0) %>%
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>% # id_rn = subcat) # has a strange numbering system
    ungroup() %>%
    select(id_block, id_desc, id_rn, Variable = descr, everything()) %>%
    select(-cat, -subcat, -region, -pg , -id_descr, -banner) %>%
    mutate(across(-c(id_block:Variable), function(col) trimws(col, "both")))
  
  if(machine_readable){
    # separate out values that have more than 1 value embedded in cell
    sas_comp_ready <- make_machine_readable(data = sas_labelled, keep_orig_ids = keep_orig_ids)
  } else {
    sas_comp_ready <- sas_labelled
  }
  if(rm_desc_col){
    sas_comp_ready$id_desc <- NULL
  }
  
  return(sas_comp_ready)
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
#' @param machine_readable a logical; should the table be prepared for optimal
#'   machine readability; that is, should cells with multiple values be pivoted
#'   to new rows
#' @param keep_orig_ids a logical; if machine_readability is desired, do you
#'   want to keep old id's that keep track of the values original position in
#'   the table
#' @param rm_desc_col a logical; should the description column be removed from
#'   the table (since they never match between base and compare)
#' @param generic_colnames a logical; Should the column names of the table
#'   generator output be generalized to be col0 - colX col99, where x is the
#'   number of groups in the 'group by' variable and col99 is the total column
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'
#' @export
#' 
prep_tg_table <- function(data,
                          machine_readable = TRUE,
                          keep_orig_ids = FALSE,
                          rm_desc_col = FALSE,
                          generic_colnames = TRUE
                          ){
  
  tg00 <- data %>%
    mutate(id_block = as.numeric(factor(ID, levels = unique(data$ID)))) %>%
    filter(Variable != "Missing") %>%
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>%
    ungroup() %>%
    select(id_block, id_desc = ID, id_rn, everything())
  
  tg_renamed <- temp_col_rename(tg00)
  tg <- tg_renamed$dat
  
  if(machine_readable){
    tg_comp_ready0 <- make_machine_readable(tg, keep_orig_ids = keep_orig_ids)
  } else {
    tg_comp_ready0 <- tg
  }
  if(generic_colnames){
    tg_comp_ready <- tg_comp_ready0
  } else {
    tg_comp_ready <- revert_temp_colnames(tg_comp_ready0, tg_renamed$orig_names)
  }
  if(rm_desc_col){
    tg_comp_ready$id_desc <- NULL
  }

  return(tg_comp_ready)
}





#' Function to compare the SAS table to the IDEA output table
#' 
#' @param sas_table SAS output 
#' @param tg_table IDEA output
#' 
#' @export
compareTables <- function(sas_table, tg_table) {
  
}
