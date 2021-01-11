#' Generate frequency where each subject is only counted once for the maximum
#' VAR
#'
#' @param column the variable to perform  stats on, this also contains the class
#'   of the column based on the data file the column came from
#' @param nested_var select variable to produce frequencies nested inside column
#' @param group the groups to compare for the ANOVA
#' @param data the data to use
#' @param totals the totals data frame that contains denominator N's use when
#'   calculating column percentages
#' @param sort text string specifying if table should be sorted by descending
#'   counts using keyword "desc_cnt" or alphabetically
#'
#' @return a frequency table of grouped variables
#'
#' @family tableGen Functions
IDEA_nested_freq <- function(column, nested_var = "NONE", group, data, totals, sort) {
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
  function(column, nested_var = "NONE", group = NULL, data, totals, sort) {
    
  # # ########## ######### ######## #########
  # column <- "ETHNIC"
  # nested_var <- "RACE"
  # group <- "TRT01P"
  # data <- bds_data
  # totals <- total_df
  # sort = "alpha"
  # sort = "desc_tot"
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
  
  if(sort %in% c("desc_tot", "desc_right")){
    if(sort == "desc_tot"){
      init_dat <- data # do nothing
    } else { # sort == "desc_right"
      grp_lvls <- getLevels(data[[grp]])
      rightmost <- grp_lvls[length(grp_lvls)]
      init_dat <- data %>%
        filter(!!sym(grp) == rightmost)
    }
    sort_cnts <- abc %>%
      left_join(
        init_dat %>%
          group_by(!!column) %>%
          summarize(n = n_distinct(USUBJID)) %>%
          ungroup()
      ) %>%
      mutate(n = replace_na(n, 0),
             sort_n = n) %>%
      arrange(desc(sort_n)) %>%
      select(sort_n, everything())
  } else { # alpha

    sort_cnts <- abc %>%
      left_join(
        data %>%
          group_by(!!column) %>%
          summarize(n = n_distinct(USUBJID)) %>%
          ungroup() 
      ) %>%
      arrange(desc(as.character(!!column))) %>% # have to reverse because we use desc() later
      mutate(n = replace_na(n, 0),
             sort_n = 1:length(column_lvls)) %>%
      select(sort_n, everything())
  }
  
  
  total0 <- 
    sort_cnts %>%
    mutate(n_tot = totals[nrow(totals),"n_tot"],
           prop = n / n_tot,
           x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
    )  %>%
    select(!!column, x, sort_n)  %>%
    arrange(desc(sort_n))
  
  if(nested_var == "NONE"){
    total <- total0 %>%
      select(-sort_n)
    
  } else {
    nst_var <- rlang::sym(as.character(nested_var))
    
    inner_lvls0 <- 
      total0 %>% 
      select(-x) %>%
      left_join(
        data %>%
          distinct(USUBJID, !!column, !!nst_var) %>%
          group_by(!!column, !!nst_var) %>%
          summarize(n = n_distinct(USUBJID)) %>%
          ungroup() %>%
          mutate(n_tot = totals[nrow(totals),"n_tot"], # do we want to keep zeros?
                 prop = n / n_tot,
                 x = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
          ) %>%
          mutate(sort = 1) 
      )
    if(sort == "desc_tot") {
      inner_lvls <- inner_lvls0 %>%
        mutate(inner_sort = n) %>%
        arrange(desc(inner_sort))
      
    } else { # alpha
      
      inner_column_lvls <- rev(sort(as.character(getLevels(data[[nst_var]]))))
      inner_abc <- data.frame(inner_column_lvls) %>%
        rename_with(~paste(nst_var), inner_column_lvls) %>%
        mutate(inner_sort = 1:length(inner_column_lvls))
      
      inner_lvls <- inner_lvls0 %>%
        left_join(inner_abc) %>%
        arrange(desc(inner_sort))
    }
    
    
    total_by <- 
      total0 %>%
      mutate(pt = 'Overall', sort = 0) %>%
      rename_with(~nested_var, pt) %>%
      bind_rows(inner_lvls) %>%
      arrange(desc(sort_n), !!column, sort, desc(inner_sort)) %>%
      select(-sort_n,-n, -prop, -n_tot, -sort, -inner_sort) %>%
      select(!!column, !!nst_var, x)
    
    total <-  
      total_by %>%
      mutate(var = case_when(
        !!nst_var == "Overall" ~ paste0("<b>",!!column,"</b>"),
        !!nst_var == NA_character_ ~ NA_character_,
        TRUE ~ paste0("&nbsp;&nbsp;&nbsp;&nbsp;", !!nst_var)
      )) %>%
      select(var, x)
  }
  
  
  if (is.null(group)) { 
    total
    
  } else { # group specified!!!
    
    if (group == column) {
      stop(glue::glue("Cannot calculate non missing subject counts for {column} when also set as grouping variable."))
    }
    
    group <- rlang::sym(group)
    
    # Need this in case dataset rows get filtered to a really small set, and
    # "lose" some levels
    grp_lvls <- getLevels(data[[group]])
    xyz <- data.frame(grp_lvls) %>%
      rename_with(~paste(group), grp_lvls)
    
    col_grp_tot <- xyz %>%
      left_join( # have to do this twice because 'crossing()' messes with order
        totals %>% filter(!!group != "Total") %>%
        tidyr::crossing(
          data %>% distinct(!!column)
        )
      )
    
    col_grp <- col_grp_tot %>%
      left_join(
        data %>%
        # filter(!is.na(!!column)) %>% # don't filter here.
        distinct(USUBJID, !!column, !!group) %>%
        group_by(!!column, !!group) %>%
        summarize(n = n_distinct(USUBJID)) %>%
        ungroup()
      ) %>%
      mutate(n = tidyr::replace_na(n, 0),
             prop = ifelse(n_tot == 0, 0, n / n_tot),
             v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
      ) %>%
      select(-n, -prop, -n_tot) 
    
    
    
    if(nested_var == "NONE"){
      groups <- 
        col_grp  %>% 
        pivot_wider(!!column, names_from = !!group, values_from = v) %>%
        left_join(total0) %>%
        arrange(desc(sort_n)) %>%
        select(-sort_n)
    } else {
      
      grp_tot <- xyz %>% # have to do this twice because 'crossing()' messes with order
        left_join(
          totals %>% filter(!!group != "Total") %>%
          tidyr::crossing(
            data %>%
              distinct(!!column, !!nst_var)
          )
        )
      
      groups <- 
        total_by %>%
        left_join(
          col_grp %>%
            pivot_wider(!!column, names_from = !!group, values_from = v) %>%
            mutate(pt = 'Overall') %>%
            rename_with(~nested_var, pt) %>%
            bind_rows(
              grp_tot %>% 
                left_join(
                  data %>%
                    distinct(USUBJID, !!column, !!nst_var, !!group) %>%
                    group_by(!!column, !!nst_var, !!group) %>%
                    summarize(n = n_distinct(USUBJID)) %>%
                    ungroup()
                ) %>%
                mutate(n = tidyr::replace_na(n, 0),
                       prop = ifelse(n_tot == 0, 0, n / n_tot),
                       v = paste0(n, ' (', sprintf("%.1f", round(prop*100, 1)), ')')
                ) %>%
                select(-n, -prop, -n_tot) %>%
                spread(!!group, v) # can use spread since we are using bind rows and the prev df is in order
            )
        )%>%
        select(-x, x) %>%
        mutate(var = case_when(
          !!nst_var == "Overall" ~ paste0("<b>",!!column,"</b>"),
          !!nst_var == NA_character_ ~ NA_character_,
          TRUE ~ paste0("&nbsp;&nbsp;&nbsp;&nbsp;", !!nst_var)
        )) %>%
        select(var, everything(), x, -!!column, -!!nst_var)
      
    }
    
    groups

  }
}



#' @return NULL
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.BDS <- function(column, nested_var = "NONE", group = NULL, data, totals, sort) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for for BDS variables"
  ))
}

#' @return NULL
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.custom <- function(column, nested_var = "NONE", group, data, totals, sort) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for custom class data set."
  ))
}
