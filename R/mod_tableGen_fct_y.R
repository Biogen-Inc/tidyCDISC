#' Generate frequency of categorical variables
#' using table generator blocks
#'
#' @param column the variable to perform frequency stats on,
#' this also contains the class of the column
#' based on the data file the column came from
#' @param group the groups to compare for the ANOVA
#' @param data the data to use 
#' @param totals the totals data frame that contains denominator N's use when
#'   calculating column percentages
#'
#' @return a frequency table of grouped variables
#' 
#' @family tableGen Functions
#' @keywords tabGen
#' 
#' @noRd
app_y <- function(column, group, data, totals) {
  UseMethod("app_y", column)
}


#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @importFrom tidyr pivot_wider
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname app_y
#' 
#' @family tableGen Functions
#' 
#' @noRd
app_y.default <- app_y.OCCDS <- app_y.ADAE <- app_y.ADSL <- function(column, group = NULL, data, totals) {
  # ########## ######### ######## #########
  # column <- "AOCCFL"
  # group = "TRT01P"
  # # # group <- "NONE"
  # data = ae_data #%>% filter(SAFFL == 'Y')
  # totals <- total_df
  # ########## ######### ######## #########
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate Y frequency, ", column, " is not categorical"))
  }
  # if (substr(column, nchar(column) - 1, nchar(column)) != "FL") {
  #   stop(paste("Can't calculate Y frequency on non-flag, ", column, " does not end with 'FL'"))
  # }

  # Calculate Total Y count
  total <- 
    data.frame(temp_col = "Y") %>%
    rename_with(~paste(column), "temp_col") %>%
    left_join(
      data %>%
      distinct(USUBJID, !!column) %>%
      filter(!!column == "Y") %>%
      group_by(!!column) %>% # keep variable visible in final table
      summarize(n = n_distinct(USUBJID)) 
    ) %>%
    mutate(n = tidyr::replace_na(n, 0),
           n_tot = as.integer(totals[nrow(totals),"n_tot"]),
           prop = n / n_tot,
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    )  %>%
    select(-n, -prop, -n_tot)
  
  
  if (is.null(group) ) { # no not use `| group == "NONE"` here 
    total
  } else {
    
    if (group == column) {
      stop(glue::glue("Cannot calculate frequency for {column} when also set as group."))
    }
    
    group <- rlang::sym(group)
    
    # Calculate Group totals. Note that sometimes, a certain level of the 
    # grouping var may cease to exist, so precautions were taken below
    # to retain it's value and give it a 0 (0.0)
    grp_lvls <- get_levels(data[[group]])
    xyz <- data.frame(grp_lvls) %>%
      rename_with(~paste(group), grp_lvls)
    
    grp_tot <- xyz %>%
      left_join(
        totals %>% filter(!!group != "Total")
        # data %>%
        # group_by(!!group) %>%
        # summarize(n_tot = n_distinct(USUBJID)) %>%
        # ungroup() 
      ) %>%
      mutate(#n_tot = tidyr::replace_na(n_tot, 0),
             temp_col = "Y") %>% # add in case some grp by level doesn't have 'Y'
      rename_with(~paste(column), "temp_col")
    
    # Calculate Group n's that have 'Y' and format table
    groups <- grp_tot %>%
        left_join(
          data %>%
          distinct(USUBJID, !!column, !!group) %>%
          group_by(!!group, !!column) %>%
          summarize(n = n_distinct(USUBJID)) %>%
          ungroup()
      ) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = ifelse(n_tot == 0, 0,n / n_tot),
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
      ) %>%
      select(-n, -prop, -n_tot) %>%
      tidyr::pivot_wider(id_cols = !!column, names_from = !!group, values_from = v)
    
    cbind(groups, total$x) # combine w/ Total
  }
}



#' @return NULL
#' @rdname app_y
#' 
#' @family tableGen Functions
#' 
#' @noRd
app_y.BDS <- function(column, group = NULL, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate Y frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname app_y
#' 
#' @family tableGen Functions
#' 
#' @noRd
app_y.custom <- function(column, group, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
