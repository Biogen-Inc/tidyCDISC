




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
        #-c(id_block:Variable)
        starts_with("col"), function(col) stringr::str_detect(col, sep))) %>%
      tidyr::separate_rows(#-c(id_block:Variable)
        starts_with("col"), sep = sep, convert = T)# convert works for sas
    
  } else {
    d <- data %>%
      # select(Variable, starts_with("col"))
      filter(across(
        # -c(id_block:id_rn), function(col) stringr::str_detect(col, sep)
        c(Variable, starts_with("col")), function(col) stringr::str_detect(col, sep)
      )) %>%
      tidyr::separate_rows(#-c(id_block:id_rn),
                           c(Variable, starts_with("col")),
                           sep = sep, convert = T) # convert works for sas
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
    filter(across(#-c(id_block:Variable)
                  starts_with("col") , function(col) {
      !stringr::str_detect(col, "\\(") & !stringr::str_detect(col, "\\,") & !stringr::str_detect(col, "\\|")}
    )) %>%
    # mutate(across(-c(id_block:Variable), as.numeric)) %>% # convert fields to numeric
    mutate(across(starts_with("col"), as.numeric)) %>% # convert fields to numeric
    union(delim_expand_rows(data = data, sep = "\\|")) %>% # no | for sas table, but we'll do it anyway
    union(delim_expand_rows(data = data, sep = "\\,")) %>%
    union(
      delim_expand_rows( sep = "\\(", data = 
                           data %>% 
                           filter(Variable != "Mean (SD)") %>%
                           mutate(across(#-starts_with("id_")
                             c(Variable, starts_with("col")),
                             function(col) gsub(")", "", col)))
      )
    ) %>%
    union(
      delim_expand_rows( sep = "\\(", data = 
                           data %>% 
                           filter(Variable == "Mean (SD)") %>%
                           mutate(across(#-starts_with("id_")
                             c(Variable, starts_with("col")), function(col) gsub(")", "", col)))
      )
    ) %>%
    arrange(id_block, id_rn, var_rn) %>%
    rename(orig_id_rn = id_rn, orig_var_rn = var_rn) %>%
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>% # id_rn = subcat) # has a strange numbering system
    ungroup() %>%
    select(id_block, id_desc, id_rn, orig_id_rn, orig_var_rn, Variable,
          # id_block:Variable,
           # everything()
          starts_with("col")
          )
  
  if(keep_orig_ids == FALSE){
    d <- d %>% select(-orig_id_rn, -orig_var_rn)
  }
  return(d)
}



#' Make repeated sequence id
#' 
#' Make an "ID vector" that identifies groups of repeated sequences in another vector.
#'
#' @param x a numeric vector of repeated sequence
#'
#' @examples
#' mk_rep_seq_id(rep(1:5,2))
#' 
mk_rep_seq_id <- function(x){
  cumsum(ifelse(is.na(lag(x)), 1, ifelse(lag(x) > x, 1, 0)))
}


#'
#' Organize SAS into a format for comparing
#'
#' @param sas_data the sas table dataframe, output from stat programmer
#' @param tg_data the table generator table dataframe, output from IDEA
#' @param chk_block_names case sensitive
#' @param chk_stat_names case sensitive
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
#' @import rlang
#' @importFrom stringr str_detect
#'
#' @export
#' 
prep_sas_table <- function(
  sas_data,
  block_names = c("by1lbl","vis"),
  block_ord_names = c("cat", "by1ord"), # not required, but helpful
  stat_names = c("by2lbl","Statlbl","test"),
  stat_ord_names = c("subcat", "by2ord"), # not required, but helpful
  tg_data,
  machine_readable = TRUE,
  keep_orig_ids = FALSE,
  rm_desc_col = FALSE){
  
  # For SAS data: the sas programmers formatting can be pretty unpredictable So
  # in order to determine which variable(s) keeps track of each block and stat,
  # will search the data first for some common/typical variables existence in
  # the data frame. Otherwise, look at tg_data for some help. Here we'll scan
  # through our database and use the first var that exists, but since this is
  # a function argument, the R programmer can overwrite these very easily by
  # looking at the data frame and giving us the true variable name as a string
  # if(rm_desc_col == FALSE){
  blk_names <- c(block_names, block_ord_names)
  blk_var <- blk_names[blk_names %in% colnames(sas_data)][1]
  if(blk_var %in% block_ord_names) message(paste0("Using block_ord_names = `", blk_var, "` instead of block_names, because none exist in data"))
  
  # If that didn't work, try to help by comparing with tg data to infer the
  # blocks
  # - assume stats match, create blocks based off of sequence since we know
  #   what to expect with each stat block output 
  # - assume labels match? ehhhhh could be iffy 
  # - assume columns w/ stats match?
  if(rlang::is_empty(blk_var) | is.na(blk_var)){
    blk_ord_var <- block_ord_names[block_ord_names %in% colnames(sas_data)][1]
    if(rlang::is_empty(blk_ord_var) | is.na(blk_ord_var)){
      # If no blk name or blk ord name, then use tg_data to loosly match and
      # find the variable of interest
      
      # blk_var <- 
    }
  } 
  
  blk_sym <- rlang::sym(blk_var)
  # }
  
  # Block order variable
  blk_ord_names <- c(block_ord_names, block_names)
  blk_ord_var <- blk_ord_names[blk_ord_names %in% colnames(sas_data)][1]
  if(rlang::is_empty(blk_ord_var) | is.na(blk_ord_var)){
    
    if(rlang::is_empty(blk_var) | is.na(blk_var)){
      # If no blk ord name & blk_var found, then use the same process as above to
      # loosly match and find the variable of interest
      
      # blk_ord_var <-
    } else {
      # If no blk ord name, but we found a blk_var above, use that for the order var
      blk_ord_var <- blk_var
    }
  }
  
  blk_ord_sym <- rlang::sym(blk_ord_var)
  
  # determine which variable displays the stats within each block first see if
  # we have any columns in common, if so, it may be a good idea to sort by
  # those?
  
  # ac - is this needed? do something similar to blk_var here?
  st_names <- c(stat_names, stat_ord_names)
  stat_var <- st_names[st_names %in% colnames(sas_data)][1]
  stat_sym <- rlang::sym(stat_var)
  
  # If missing, that's okay
  st_ord_names <- c(stat_ord_names, stat_names)
  stat_ord_var <- st_ord_names[st_ord_names %in% colnames(sas_data)][1]
  stat_ord_sym <- rlang::sym(stat_ord_var)
  
  # is.ordered(sas_data$by1lbl)
  # class(sas_data$by1lbl)
  
  if(is.numeric(sas_prepped[[stat_ord_var]])){
    sas_prepped0 <-
      sas_data %>%
      filter(!!stat_ord_sym > 0 & !is.na(!!stat_ord_sym))
  } else {
    sas_prepped0 <- sas_data
  }
  
  sas_prepped <-
    sas_prepped0 %>%
    mutate(id_block_user = as.numeric(factor(!!blk_ord_sym, levels = unique(sas_data[[blk_ord_var]]))),
           id_stat = as.numeric(factor(!!stat_ord_sym, levels = unique(sas_data[[stat_ord_var]]))),
           descr = trimws(!!stat_sym, which = "both") # get rid of white spaces
    )
  
  
  sas_labelled <-
    sas_prepped %>% # sometimes these rows aren't meant to exist in final table
    mutate(id_block_per_stat = mk_rep_seq_id(id_stat)) %>%
    rowwise() %>%
    mutate(
      id_block = ifelse(all(id_block_per_stat == id_block_user) | 
                              rlang::is_empty(stat_ord_var) | is.na(stat_ord_var), id_block_user, id_block_per_stat)
    ) %>% 
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>% # id_rn = !!stat_ord_sym) # has a strange numbering system
    ungroup() %>%
    select(id_block, id_block_user, id_desc = !!blk_sym, id_stat, id_block_per_stat, id_rn,
           Variable = descr, everything()) %>%
    # select(-cat, -!!stat_ord_sym, -region, -pg , -id_descr, -banner) %>%
    mutate(across(-c(id_block:Variable), function(col) trimws(col, "both")))
  
  if(machine_readable){
    # separate out values that have more than 1 value embedded in cell
    sas_comp_ready <- make_machine_readable(
      data = sas_labelled, 
      keep_orig_ids = keep_orig_ids
    )
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
temp_col_rename <- function(dat, sas_generics){
  # sas_generics <- colx # testing
  if(!all(substr(sas_generics, 1, 3) == "col")) stop("generic_colnames argument must be a vector with prefix 'col'")
  colx_st <- min(as.numeric(gsub("col","",sas_generics)))
  
  
  var_ind <- which(names(dat) == "Variable")
  tot_ind <- which(names(dat) == "Total")
  orig <- names(dat)[(var_ind + 1):tot_ind]
  col_nums <- seq_len(tot_ind - var_ind) - ifelse(colx_st == 1, 0, 1)
  col_nums[length(col_nums)] <- 99
  names(dat)[(var_ind + 1):tot_ind] <- paste0("col", col_nums)
  
  if(length(col_nums) == length(sas_generics) + 1){
    dat$col99 <- NULL
    orig <- orig[orig != "Total"]
  } 
  return(list(dat = dat, orig_names = orig))
}

#' Revert temporary column names back to the original; use with temp_col_rename
#'
#' @param dat a data frame
#' @param orig_grp_names a character vector of original col names
#'
revert_temp_colnames <- function(dat, orig_grp_names){
  var_ind <- which(names(dat) == "Variable")
  if("Total" %in% names(dat)){
    end_ind <- which(names(dat) == "Total")
  } else {
    colx <- names(dat)[stringr::str_detect(names(dat), "^col[0-9]")]
    end_ind <- which(names(dat) == colx[length(colx)])
  }
  names(dat)[(var_ind + 1):end_ind] <- orig_grp_names
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
                          generic_colnames = "NONE"
                          ){
  
  tg00 <- data %>%
    mutate(id_block = as.numeric(factor(ID, levels = unique(data$ID)))) %>%
    filter(Variable != "Missing") %>%
    group_by(id_block) %>%
    mutate(id_rn = row_number()) %>%
    ungroup() %>%
    select(id_block, id_desc = ID, id_rn, everything())
  
  tg_renamed <- temp_col_rename(tg00, sas_generics = generic_colnames)
  tg <- tg_renamed$dat
  
  if(machine_readable){
    tg_comp_ready0 <- make_machine_readable(tg, keep_orig_ids = keep_orig_ids)
  } else {
    tg_comp_ready0 <- tg
  }

  if(!rlang::is_empty(generic_colnames)){
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
