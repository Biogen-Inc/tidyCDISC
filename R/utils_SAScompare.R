




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
      mutate(Variable = if_else(var_rn == 2 & Variable != "SD",paste(Variable, "(COL PCT)"), Variable))
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
      # a<-
      delim_expand_rows( sep = "\\(", data = 
                           data %>% 
                           filter(Variable != "Mean (SD)") %>%
                           mutate(across(-starts_with("id_"), function(col) gsub(")", "", col)))
      )
    ) %>%
    bind_rows(
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
