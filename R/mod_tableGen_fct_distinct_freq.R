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
IDEA_distinct_freq <- function(column, group, data) {
  UseMethod("IDEA_distinct_freq", column)
}


#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_distinct_freq
#' 
#' @family tableGen Functionss
IDEA_distinct_freq.default <- IDEA_distinct_freq.OCCDS <- IDEA_distinct_freq.ADAE <- IDEA_distinct_freq.ADSL <- 
  function(column, group = NULL, data) {
  # # ########## ######### ######## #########
  # column <- "AEBODSYS"
  # group = "TRT01P"
  column_var_sort = "desc_tot"
  # data = ae_data #%>% filter(SAFFL == 'Y') %>% filter(TRTEMFL == 'Y')
  # test <- data %>% filter(is.na(AEBODSYS))
  # # ########## ######### ######## #########
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  # First, get the desired order our by_var
  column_lvls <- getLevels(data[[column]])
  abc <- data.frame(column_lvls) %>%
    rename_with(~paste(column), column_lvls)
  
  if(column_var_sort %in% c("desc_tot", "desc_right")){
    if(column_var_sort == "desc_tot"){
      init_dat <- data # do nothing
    } else { # column_var_sort == "desc_right"
      grp_lvls <- getLevels(data[[grp]])
      rightmost <- grp_lvls[length(grp_lvls)]
      init_dat <- data %>%
        filter(!!sym(grp) == rightmost)
    }
    sort_cnts <- abc %>%
      left_join(
        init_dat %>%
          # filter(!!sym(filt_var) == filt_lvl) %>%
          group_by(!!column) %>%
          summarize(sort_n = n_distinct(USUBJID)) %>%
          arrange(desc(sort_n)) %>%
          select(sort_n, everything())
      ) %>%
      mutate(sort_n = replace_na(sort_n, 0)) %>%
      arrange(desc(sort_n))
  }
  else { # alpha
    sort_cnts <- abc %>%
      mutate(sort_n = rev(1:length(column_lvls)))
    # have to reverse because we use desc() later
  }
  
  
  total <- 
    sort_cnts %>%
    mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(),
           prop = sort_n / n_tot,
           x = paste0(sort_n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
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
      # filter(!is.na(!!column)) %>% # don't filter here.
      group_by(!!group) %>%
      summarize(n_tot = n_distinct(USUBJID)) %>%
      ungroup() %>%
      tidyr::crossing(
        data %>%
        # filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
          distinct(!!column)
      )
    
    groups <- grp_tot %>%
      left_join(
        data %>%
        # filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
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
      transpose_df(num = 1) %>%
      mutate(rowname = case_when(rowname == "V1" ~ "",
                                 rowname == "<NA>" ~ NA_character_,
                                 TRUE ~ rowname)) %>%
      left_join(sort_cnts, by = c("rowname" = paste(column))) %>%
      arrange(desc(sort_n)) %>%
      select(-sort_n) 
    
    cbind(groups, total$x)
  }
}



#' @return NULL
#' @rdname IDEA_distinct_freq
#' 
#' @family tableGen Functions
IDEA_distinct_freq.BDS <- function(column, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for for BDS variables"
  ))
}

#' @return NULL
#' @rdname IDEA_distinct_freq
#' 
#' @family tableGen Functions
IDEA_distinct_freq.custom <- function(column, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for custom class data set."
  ))
}
