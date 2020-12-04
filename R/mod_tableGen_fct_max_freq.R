#' Generate frequency where each subject is only counted once for the maximum
#' VAR
#'
#' @param column the variable to perform  stats on, this also contains
#'   the class of the column based on the data file the column came from
#' @param group the groups to compare for the ANOVA
#' @param data the data to use
#'
#' @return a frequency table of grouped variables
#'
#' @family tableGen Functions
IDEA_max_freq <- function(column, group, data) {
  UseMethod("IDEA_max_freq", column)
}


#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_max_freq
#' 
#' @family tableGen Functionss
IDEA_max_freq.default <- IDEA_max_freq.OCCDS <- IDEA_max_freq.ADAE <- IDEA_max_freq.ADSL <- 
  function(column, group = NULL, data) {
  # # ########## ######### ######## #########
  # column <- blockData$S3
  # group = "TRT01P"
  # data = ae_data %>% filter(SAFFL == 'Y') %>% filter(TRTEMFL == 'Y')
  # # ########## ######### ######## #########
  
    # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  VARN <- paste0(column,"N")
  if(is.character(data[[column]])) {
    stop(paste("Can't calculate max frequency per patient because ", column, " is of class 'character' and ", VARN," doesn't exist in data"))
  }
  
  # if column is categorical 'VAR', then check to make sure 'VARN' exists. If not, error
  if (is.factor(data[[column]])) {
    if(!(VARN %in% colnames(data))){
      stop(paste("Can't calculate max frequency per patient because ", VARN, " doesn't exist in data"))
    }
  }
  
  # The alternative is that VAR is numeric, so we can use that directly in max freq

  total <- 
    data %>%
    filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
    group_by(USUBJID) %>%
    slice_max(!!column) %>%
    ungroup() %>%
    distinct(USUBJID, !!column) %>%
    group_by(!!column) %>%
    summarize(n = n_distinct(USUBJID)) %>%
    ungroup() %>%
    mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(),
           prop = n / n_tot,
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    )  %>%
    select(!!column, x) 
  
  
  if (is.null(group)) { 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate non missing subject counts for {column} when also set as grouping variable."))
    }
    
    group <- rlang::sym(group)
    
    grp_tot <- data %>%
      filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
      group_by(!!group) %>%
      summarize(n_tot = n_distinct(USUBJID)) %>%
      ungroup()
      
    groups <- grp_tot %>%
      left_join(
        data %>%
        filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
        group_by(USUBJID) %>%
        slice_max(!!column) %>%
        ungroup() %>%
        distinct(USUBJID, !!group, !!column) %>%
        group_by(!!group, !!column) %>%
        summarize(n = n_distinct(USUBJID)) %>%
        ungroup()
      ) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = n / n_tot,
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
      ) %>%
      select(-n, -prop, -n_tot) %>%
      spread(!!column, v) %>%
      transpose_df(num = 1)
    
    cbind(groups, total$x)
  }
}



#' @return NULL
#' @rdname IDEA_max_freq
#' 
#' @family tableGen Functions
IDEA_max_freq.BDS <- function(column, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Max Frequency for for BDS variables"
  ))
}

#' @return NULL
#' @rdname IDEA_max_freq
#' 
#' @family tableGen Functions
IDEA_max_freq.custom <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate Max Frequency for custom class data set."
  ))
}
