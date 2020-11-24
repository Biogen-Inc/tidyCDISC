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
IDEA_non_empty <- function(column, group, data) {
  UseMethod("IDEA_non_empty", column)
}

#' @return NULL
#' @rdname IDEA_non_empty
#' 
#' @family tableGen Functions
IDEA_non_empty.default <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Default method not yet configured to handle this unknown class"
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
#' @rdname IDEA_non_empty
#' 
#' @family tableGen Functionss
IDEA_non_empty.OCCDS <- IDEA_non_empty.ADAE <- IDEA_non_empty.ADSL <- function(column, group = NULL, data) {
  # ########## ######### ######## #########
  # column <- blockData$S3
  # group = "TRT01P"
  # data = ae_data %>% filter(SAFFL == 'Y')
  # ########## ######### ######## #########
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate Y frequency, ", column, " is not categorical"))
  }
  if (substr(column, nchar(column) - 1, nchar(column)) != "FL") {
    stop(paste("Can't calculate Y frequency on non-flag, ", column, " does not end with 'FL'"))
  }

  total <- 
    data %>%
    distinct(USUBJID, !!sym(column)) %>%
    filter(!!sym(column) == "Y") %>%
    # count(!!sym(column)) %>%
    summarize(n = n_distinct(USUBJID)) %>%
    mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(),
           prop = n / n_tot,
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    )  %>%
    select(-n, -prop, -n_tot)
  
  
  if (is.null(group)) { 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate frequency for {column} when also set as group."))
    }
    
    group <- rlang::sym(group)
    
    grp_tot <- data %>%
      group_by(!!sym(group)) %>%
      summarize(n_tot = n_distinct(USUBJID))
      
    groups <- data %>%
      # distinct(USUBJID, !!sym(group), !!sym(column)) %>%
      group_by(!!sym(group), !!sym(column)) %>%
      summarize(n = n_distinct(USUBJID)) %>%
      left_join(grp_tot) %>%
      mutate(prop = n / n_tot,
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
      ) %>%
      filter(!!sym(column) == "Y") %>%
      select(-n, -prop, -n_tot) %>%
      spread(!!sym(column), v) %>%
      transpose_df(num = 1)
    
    cbind(groups, total$x)
  }
}



#' @return NULL
#' @rdname IDEA_non_empty
#' 
#' @family tableGen Functions
IDEA_non_empty.BDS <- function(column, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Y frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname IDEA_non_empty
#' 
#' @family tableGen Functions
IDEA_non_empty.custom <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
