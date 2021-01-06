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
IDEA_freq <- function(column, group, data, totals) {
  UseMethod("IDEA_freq", column)
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.default <- function(column, group, data, totals) {
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
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.ADAE <- IDEA_freq.ADSL <- function(column, group = NULL, data, totals) {
  # ########## ######### ######## #########
  # column <- "AOCCFL"
  # group = "TRT01P"
  # # # group <- "NONE"
  # data = ae_data #%>% filter(SAFFL == 'Y')
  # totals <- total_df
  # ########## ######### ######## #########
  
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate frequency, ", column, " is not categorical"))
  }
  
  total <- data %>%
    distinct(USUBJID, !!column) %>%
    count(!!column) %>%
    group_by(!!column) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(prop = n/totals[nrow(totals),"n_tot"]) %>%
    mutate(x = paste0(n, " (", sprintf("%.1f", round(prop*100, 1)), ")")) %>%
    select(!!column, x)
  
  
  if (is.null(group)) { 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate frequency for {column} when also set as group."))
    }
    
    group <- rlang::sym(group)
    
    grp_lvls <- getLevels(data[[group]])
    xyz <- data.frame(grp_lvls) %>%
      rename_with(~paste(group), grp_lvls)
    
    grp_tot <- xyz %>%
      left_join(
        totals %>% filter(!!group != "Total")
        # data %>%
        # group_by(!!group) %>%
        # summarize(n_tot = n_distinct(USUBJID)) %>%
        # ungroup()
      )#%>%
    # mutate(n_tot = tidyr::replace_na(n_tot, 0))
    
    groups <- 
      grp_tot %>%
      left_join(
        data %>%
        distinct(USUBJID, !!column, !!group) %>%
        count(!!column, !!group)
      ) %>%
      # group_by(!!group) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = ifelse(n_tot == 0, 0, n / n_tot),
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')) %>%
      select(-n, -prop) %>%
      pivot_wider(!!column, names_from = !!group, values_from = v)
      # spread(!!group, v1)
    
    cbind(groups, total$x)
  }
}


#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.BDS <- function(column, group = NULL, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.OCCDS <- function(column, group, data, totals) {
  rlang::abort(glue::glue(
    "Currently no method to perform frequency statistics on OCCDS"
  ))
}


#' @return NULL
#' @rdname IDEA_freq
#' 
#' @family tableGen Functions
IDEA_freq.custom <- function(column, group, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}