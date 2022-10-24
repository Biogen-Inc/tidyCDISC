#' Generate summary statistics 
#' using table generator blocks
#'
#' @param column the variable to perform summary statistics on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param week filter the variable by certain week
#' @param group the groups to compare for the summary statistics
#' @param data the data to use 
#'
#' @return an summary statistic table of grouped variables
#' 
#' @family tableGen Functions
#' @keywords tabGen
#' 
#' @noRd
app_mean <- function(column, week, group, data) {
  UseMethod("app_mean", column)
}


#' @return NULL
#' @rdname app_mean
#' @family tableGen Functions
#' @noRd
app_mean.default <- function(column, week, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean because data is not classified as ADLB, BDS or OCCDS",
  ))
}

#' if ADSL supplied look for the column to take mean of
#' and look for a grouping variable to group_by
#'
#' @import dplyr
#' @importFrom rlang sym !! 
#' 
#' @return an summary statistic table of grouped variables
#' 
#' @param column the variable to perform summary statistics on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param week filter the variable by certain week
#' @param group the groups to compare for the summary statistics
#' @param data the data to use 
#' 
#' @family tableGen Functions
#' @rdname app_mean
#' 
#' @noRd
app_mean.ADAE <- app_mean.ADSL <- function(column, week, group = NULL, data) {
  
  column <- as.character(column)
  
  if (!is.numeric(data[[column]])) {
    stop(paste("Can't calculate mean, ", column, " is not numeric"))
  }
  
  all <- data %>%
    mean_summary(column) %>%
    transpose_df(999)
  
  if (!is.null(group)) {
    group <- sym(group)
    grouped <- data %>%
      group_by(!!group) %>% 
      mean_summary(column) %>%
      transpose_df(1)
    
    cbind(grouped, all[2])
    
  } else {
    all
  }
  
}

#' if BDS filter by paramcd and week
#' We need to calculate the difference in N for this
#' and report missing values from the mean, if any
#' 
#' @import dplyr
#' @importFrom rlang sym !!
#' 
#' @return an summary statistic table of grouped variables
#' 
#' @param column the variable to perform summary statistics on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param week filter the variable by certain week
#' @param group the groups to compare for the summary statistics
#' @param data the data to use 
#' 
#' @family tableGen Functions
#' @rdname app_mean
#' 
#' @noRd
app_mean.BDS <- function(column, week, group = NULL, data) {
  
  ################################
  # column <- "K"
  # week <- "DAY -1"
  # group <- 'TRT01P'
  # data <- bds_data
  ################################
  
  column <- as.character(column)
  
  if (!column %in% data[["PARAMCD"]]) {
    stop(paste("Can't calculate mean, ", column, " has no AVAL"))
  }
  
  all <- data %>%
    filter(AVISIT == week & PARAMCD == column) %>%
    mean_summary("AVAL") %>%
    transpose_df(999)
  
  if (!is.null(group)) {
    
    if (week == "NONE") {
      stop("Please select a week from the MEAN dropdown to calculate mean of ", column)
    }
    
    group <- sym(group)
    grp_lvls <- get_levels(data[[group]])
    xyz <- data.frame(grp_lvls) %>%
      rename_with(~paste(group), grp_lvls)
    
    grouped <- 
      xyz %>%
      left_join(
        data %>%
        filter(AVISIT == week & PARAMCD == column) %>%
        group_by(!!group) %>%
        mean_summary("AVAL")
      )%>%
      mutate( n = tidyr::replace_na(n, ""),
             `Mean (SD)` = tidyr::replace_na(`Mean (SD)`, ""),
              Median = tidyr::replace_na(Median, ""),
             `Q1 | Q3` = tidyr::replace_na(`Q1 | Q3`, ""),
             `Min | Max` = tidyr::replace_na(`Min | Max`, ""),
             ) %>%
      transpose_df(1)
    
    cbind(grouped, all[2])
    
  } else {
    all
  }
}

#' @return NULL
#' @rdname app_mean
#' 
#' @family tableGen Functions
#' @noRd
app_mean.OCCDS <- function(column, week = NULL, group, data) {
  rlang::abort(glue::glue(
    "Currently no method to perform summary statistics on OCCDS"
  ))
}


#' @return NULL
#' @rdname app_mean
#' 
#' @family tableGen Functions
#' @noRd
app_mean.custom <- function(column, week = NULL, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
