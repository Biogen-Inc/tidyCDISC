#' Generate frequency of categorical variables
#' using table generator blocks
#'
#' @param column the variable to perform frequency stats on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param week filter the variable by certain week
#' @param group the groups to compare for the ANOVA
#' @param data the data to use 
#'
#' @return a frequency table of grouped variables
#' 
#' @family tableGen Functions
IDEA_freq <- function(column, week, group, data) {
  UseMethod("IDEA_freq", column)
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.default <- function(column, week, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS"
  ))
}

#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.ADSL <- function(column, week, group = NULL, data) {
  
  column <- rlang::sym(as.character(column))
  
  if (!is.character(data[[column]])) {
    stop(paste("Can't calculate frequency, ", column, " is not categorical"))
  }
  
  if (is.null(group)) {
    data %>%
      distinct(USUBJID, !!column) %>%
      count(!!column) %>%
      group_by(!!column) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      mutate(prop = n/sum(n)) %>%
      mutate(x = paste0(n, " (", round(prop*100, 2), ")")) %>%
      select(!!column, x)
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate frequency for {column} when also set as group."))
    }
    
    group <- rlang::sym(group)
    data %>%
      distinct(USUBJID, !!column, !!group) %>%
      count(!!column, !!group) %>%
      group_by(!!group) %>%
      mutate(prop = prop.table(n)) %>%
      mutate(v1 = paste0(n, ' (', round(prop*100, 2), ')')) %>%
      select(-n, -prop) %>% 
      spread(!!group, v1)
  }
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.BDS <- function(column, week, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.OCCDS <- function(column, week = NULL, group, data) {
  rlang::abort(glue::glue(
    "Currently no method to perform frequency statistics on OCCDS"
  ))
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.custom <- function(column, week = NULL, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}