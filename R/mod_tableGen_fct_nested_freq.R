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
IDEA_nested_freq <- function(column, nested_var = "NONE", group, data) {
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
  function(column, nested_var = "NONE", group = NULL, data) {
    
  # # ########## ######### ######## #########
  # column <- "AEBODSYS"
  # nested_var <- "AEDECOD"
  # group <- "TRT01P"
  # data <- ae_data
  column_var_sort = "desc_tot"
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
    
    inner_lvls <- 
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
    
    total_by <- 
      total0 %>%
      mutate(pt = 'Overall', sort = 0) %>%
      rename_with(~nested_var, pt) %>%
      bind_rows(inner_lvls) %>%
      arrange(desc(sort_n), !!column, sort, desc(n)) %>%
      select(-sort_n,-n, -prop, -n_tot, -sort) %>%
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
        xyz %>%
        left_join(
          data %>%
          # filter(!is.na(!!column)) %>% # don't filter here.
          group_by(!!group) %>%
          summarize(n_tot = n_distinct(USUBJID)) %>%
          ungroup() 
        )%>%
        mutate(n_tot = tidyr::replace_na(n_tot, 0)) %>%
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
      groups0 <- col_grp %>%
        spread(!!column, v) %>%
        transpose_df(num = 1)
      groups <- cbind(groups0, total$x)
    } else {
      
      grp_tot <- xyz %>% # have to do this twice because 'crossing()' messes with order
        left_join(
          xyz %>%
          left_join(
            data %>%
            # filter(!is.na(!!column)) %>% # don't filter here.
            group_by(!!group) %>%
            summarize(n_tot = n_distinct(USUBJID)) %>%
            ungroup()
          ) %>%
          mutate(n_tot = tidyr::replace_na(n_tot, 0)) %>%
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
            # spread(!!group, v)%>% # swapped for pivot_wider because spread doesn't retain order when zero vals exist for lvl
            mutate(pt = 'Overall') %>%
            rename_with(~nested_var, pt) %>%
            bind_rows(
              grp_tot %>% 
                left_join(
                  data %>%
                    # filter(!is.na(!!column)) %>% # how to incorporate filter on AOCCIFL?
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
                spread(!!group, v)
            )
        )%>%
        select(-x, x) %>%
        mutate(var = case_when(
          !!nst_var == "Overall" ~ paste0("<b>",!!column,"</b>"),
          !!nst_var == NA_character_ ~ NA_character_,
          TRUE ~ paste0("&nbsp;&nbsp;&nbsp;&nbsp;", !!nst_var)
        )) %>%
        select(var, everything(), x, -!!column, -!!nst_var)
      
      # wasn't able to use transpose_df, so we need to make column names generic here
      # colnames(groups) <- c("rowname", paste(1:(ncol(groups)-1)))
    }
    
    groups

  }
}



#' @return NULL
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.BDS <- function(column, nested_var = "NONE", group = NULL, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for for BDS variables"
  ))
}

#' @return NULL
#' @rdname IDEA_nested_freq
#' 
#' @family tableGen Functions
IDEA_nested_freq.custom <- function(column, nested_var = "NONE", group, data) {
  rlang::abort(glue::glue(
    "Can't calculate Distinct Frequency for custom class data set."
  ))
}
