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
IDEA_y <- function(column, group, data) {
  UseMethod("IDEA_y", column)
}

#' @return NULL
#' @rdname IDEA_y
#' 
#' @family tableGen Functions
IDEA_y.default <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS"
  ))
}

#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_y
#' 
#' @family tableGen Functions
IDEA_y.OCCDS <- IDEA_y.ADAE <- IDEA_y.ADSL <- function(column, group = NULL, data) {
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate frequency, ", column, " is not categorical"))
  }
  
  total <- 
    data %>%
    distinct(USUBJID, !!sym(column)) %>%
    count(!!sym(column)) %>%
    mutate(prop = prop.table(n),
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    ) %>%
    filter(!!sym(column) == "Y") %>%
    select(-n, -prop)
  
  
  if (is.null(group)) { 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate frequency for {column} when also set as group."))
    }
    
    group <- rlang::sym(group)
    
    groups <- data %>%
      distinct(USUBJID, !!sym(group), !!sym(column)) %>%
      count(!!sym(column), !!sym(group)) %>%
      group_by(!!sym(group)) %>%
      mutate(prop = prop.table(n),
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
      )  %>%
      filter(!!sym(column) == "Y") %>%
      select(-n, -prop) %>%
      spread(!!sym(column), v) %>%
      transpose_df(num = 1)
    
    cbind(groups, total$x)
  }
}

#' @return NULL
#' @rdname IDEA_y
#' 
#' @family tableGen Functions
IDEA_y.BDS <- function(column, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Y frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname IDEA_y
#' 
#' @family tableGen Functions
IDEA_y.custom <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}