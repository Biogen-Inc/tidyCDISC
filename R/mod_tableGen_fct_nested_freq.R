#' Generate frequency where each subject is only counted once for the maximum
#' VAR
#'
#' @param column the variable to perform  stats on, this also contains
#'   the class of the column based on the data file the column came from
#' @param nested_var select variable to produce frequencies nested inside column
#' @param group the groups to compare for the ANOVA
#' @param data the data to use
#'
#' @return a frequency table of grouped variables
#'
#' @family tableGen Functions
IDEA_nested_freq <- function(column, nested_var, group, data) {
  UseMethod("IDEA_nested_freq", column)
}


#' if ADSL supplied look for the column to take frequency of
#' and look for a grouping variable to group_by
#' if data is grouped add total column to the grouped data
#' 
#' @importFrom rlang sym !!
#' @import dplyr
#' 
#' @return frequency table of ADSL column
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functionss
IDEA_nested_freq.default <- IDEA_nested_freq.OCCDS <- IDEA_nested_freq.ADAE <- IDEA_nested_freq.ADSL <- 
  function(column, nested_var, group = NULL, data) {
    
  # # ########## ######### ######## #########
  # column <- "AEBODSYS"
  # nested_var <- "AEDECOD"
  # group = "TRT01P"
  column_var_sort = "desc_tot"
  # data = ae_data #%>% filter(SAFFL == 'Y') %>% filter(TRTEMFL == 'Y')
  # # ########## ######### ######## #########
  
  # column is the variable selected on the left-hand side
  column <- rlang::sym(as.character(column))
  
  if (is.numeric(data[[column]])) {
    stop(paste("Can't calculate frequency, ", column, " is not categorical"))
  }
  
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
  
  
  total0 <- 
    sort_cnts %>%
    mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(),
           prop = sort_n / n_tot,
           x = paste0(sort_n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    )  %>%
    select(!!column, x, sort_n) 
  
  if(nested_var == "NONE"){
    total <- total0 %>%
      select(-sort_n)
    
  } else {
    nst_var <- rlang::sym(as.character(nested_var))
    
    total <- 
      total0 %>%
      mutate(pt = 'Overall', sort = 0) %>%
      rename_with(~nested_var, pt) %>%
      bind_rows(
        total0 %>% 
        select(-x) %>%
        left_join(
          data %>%
            # filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
            distinct(USUBJID, !!column, !!nst_var) %>%
            group_by(!!column, !!nst_var) %>%
            summarize(n = n_distinct(USUBJID)) %>%
            ungroup() %>%
            mutate(n_tot = data %>% distinct(USUBJID) %>% nrow(), # do we want to keep zeros?
                   prop = n / n_tot,
                   x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
            ) %>%
            mutate(sort = 1) %>%
            arrange(desc(n))
        )
      ) %>%
      arrange(desc(sort_n), !!column, sort, desc(n)) %>%
      select(-sort_n,-n, -prop, -n_tot, -sort) %>%
      select(!!column, !!nst_var, x) %>%
      mutate(var = case_when(
        !!nst_var == "Overall" ~ !!column,
        !!nst_var == NA_character_ ~ NA_character_,
        TRUE ~ paste0("    ", !!nst_var)
      )) %>%
      select(var, x)
  }
  
  
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
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.BDS <- function(column, nested_var, group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for for BDS variables"
  ))
}

#' @return NULL
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.custom <- function(column, nested_var, group, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for custom class data set."
  ))
}
