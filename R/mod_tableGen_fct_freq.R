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
#' @export
#' @keywords tabGen
#' 
app_freq <- function(column, group, data, totals) {
  UseMethod("app_freq", column)
}

#' @return NULL
#' @rdname app_freq
#' 
#' @family tableGen Functions

app_freq.default <- function(column, group, data, totals) {
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
#' @rdname app_freq
#' 
#' @family tableGen Functions

app_freq.ADAE <- app_freq.ADSL <- function(column, group = NULL, data, totals) {
  # ########## ######### ######## #########
  # column <- "SEX"
  # group = "TRT01P"
  # group <- NULL
  # data = tg_data #bds_data #%>% filter(SAFFL == 'Y')
  # totals <- total_df
  # ########## ######### ######## #########
  
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate frequency, ", column, " is not categorical"))
  }
  
  total00 <- data %>%
    distinct(USUBJID, !!column) %>%
    count(!!column)
  
  # special case: if the result has only one value -
  #  all 'Y' or all 'N', then include the other val
  if(nrow(total00) == 1 & all(total00[[1]] %in% c('Y', 'N'))){
    grp_lvls <- c('Y', 'N')
    xyz <- data.frame(grp_lvls) %>%
      rename_with(~paste(column), grp_lvls)
    total0 <- xyz %>% left_join(total00)
  } else {
    total0 <- total00
  }

  total <- total0 %>%
    group_by(!!column) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(n = tidyr::replace_na(n, 0),
      prop = if_else(n == 0, 0, n/as.integer(totals[nrow(totals),"n_tot"]))) %>%
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
      )
    
    
    grp_innards0 <- data %>%
      distinct(USUBJID, !!column, !!group) %>%
      count(!!column, !!group)
    
    # special case: if the result has only one value -
    #  all 'Y' or all 'N', then include the other val
    if(all(grp_innards0[[1]] %in% c('Y')) | all(grp_innards0[[1]] %in% c('N'))){
      grp_lvls <- c('Y', 'N')
      xyz <- data.frame(grp_lvls) %>%
        rename_with(~paste(column), grp_lvls)
      
      grp_innards <- xyz %>%
        tidyr::crossing(grp_tot %>% select(!!group)) %>%
        left_join(
          data %>%
          distinct(USUBJID, !!column, !!group) %>%
          count(!!column, !!group)
        )
    } else {
      grp_innards <- grp_innards0
    }
    
    groups <- 
      grp_tot %>%
      left_join(grp_innards) %>%
      # group_by(!!group) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = ifelse(n_tot == 0, 0, n / n_tot),
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')) %>%
      select(-n, -prop) %>%
      mutate(!!group := ifelse(!!group == '', '_missing_', !!group)) %>%
      tidyr::pivot_wider(!!column, names_from = !!group, values_from = v)
    
    cbind(groups, total$x)
  }
}


#' @return NULL
#' @rdname app_freq
#' 
#' @family tableGen Functions

app_freq.BDS <- function(column, group = NULL, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate frequency for BDS - {column} is numeric"
  ))
}

#' @return NULL
#' @rdname app_freq
#' 
#' @family tableGen Functions
app_freq.OCCDS <- function(column, group, data, totals) {
  rlang::abort(glue::glue(
    "Currently no method to perform frequency statistics on OCCDS"
  ))
}


#' @return NULL
#' @rdname app_freq
#' 
#' @family tableGen Functions

app_freq.custom <- function(column, group, data, totals) {
  rlang::abort(glue::glue(
    "Can't calculate mean, data is not classified as ADLB, BDS or OCCDS"
  ))
}
