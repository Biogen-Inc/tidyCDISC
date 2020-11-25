#' Generate frequency of categorical variables
#' using table generator blocks
#'
#' @param column the variable to perform frequency stats on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param group the groups to compare for the ANOVA
#' @param data the data to use 
#'
#' @return a frequency table of grouped variables
#' 
#' @family tableGen Functions
IDEA_non_missing <- function(column, group, data) {
  UseMethod("IDEA_non_missing", column)
}


#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_non_missing
#' 
#' @family tableGen Functionss
IDEA_non_missing.default <- IDEA_non_missing.OCCDS <- IDEA_non_missing.ADAE <- IDEA_non_missing.ADSL <- 
  function(column, group = NULL, data) {
  # # ########## ######### ######## #########
  # column <- blockData$S3
  # group = "TRT01P"
  # data = ae_data %>% filter(SAFFL == 'Y')
  # # ########## ######### ######## #########
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))

  total <- 
    data %>%
    distinct(USUBJID, !!column) %>%
    filter(!is.na(!!column)) %>%
    summarize(n = n_distinct(USUBJID)) %>%
    mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(),
           prop = n / n_tot,
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')'),
           temp_col = "Non Missing"
    )  %>%
    rename_with(~paste(column), "temp_col") %>%
    select(!!column, x) 
  
  
  if (is.null(group)) { 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate non missing subject counts for {column} when also set as grouping variable."))
    }
    
    group <- rlang::sym(group)
    
    grp_tot <- data %>%
      group_by(!!group) %>%
      summarize(n_tot = n_distinct(USUBJID)) %>%
      ungroup()
      
    groups <- grp_tot %>%
      left_join(
        data %>%
        filter(!is.na(!!column)) %>%
        group_by(!!group) %>%
        summarize(n = n_distinct(USUBJID)) %>%
        ungroup()
      ) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = n / n_tot,
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')'),
             temp_col = "Non Missing"
      ) %>%
      rename_with(~as.character(column), "temp_col") %>%
      select(-n, -prop, -n_tot) %>%
      spread(!!column, v) %>%
      transpose_df(num = 1)
    
    cbind(groups, total$x)
  }
}



#' @return NULL
#' @rdname IDEA_non_missing
#' 
#' @family tableGen Functions
IDEA_non_missing.BDS <- function(column, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Non Missings for BDS yet"
  ))
}

#' @return NULL
#' @rdname IDEA_non_missing
#' 
#' @family tableGen Functions
IDEA_non_missing.custom <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
