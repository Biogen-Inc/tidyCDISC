

#' Organize Data Frame Events
#'
#' Gather various shiny inputs from an OCCD dataset and return a single
#' standardized events dataset
#'
#' @param df_name A character string containing the name of the OCCD ADaM data
#'   set to organize. Ex: "ADAE"
#' @param df_domain_abbr A character string of the shorthand abbreviation for
#'   the same dataset, usually whatever comes after "AD". Ex: "AE"
#' @param df_desc A character string that provides the full name/description of
#'   the dataset. Ex: "Adverse Events"
#' @param df_st_date_vars A character vector containing possible date variables
#'   to summarize the events of interest. If more than 1 date variable is
#'   provided in the vector, then the first element that exists in the data will
#'   be used. Ex: if df_st_date_vars = c("AESTDT","ASTDT"), then AESTDT will be
#'   used if it exists in the data set provided above. However, if AESTDT is
#'   missing, then ASTDT will be used if it exists.
#' @param event_desc_vars A character vector containing the names of multiple
#'   variables needed from the data set above for event_desc.
#' @param event_desc A character string containing the name of a variable or
#'   even an expression to compute the event description to be displayed and
#'   contains the variables in event_desc_vars
#' @param mi_input_checkbox A character vector of event names selected. Some
#'   possible event names include "Milestones", "Adverse Events", "Con Meds",
#'   "Labs", "Medical History", etc...
#' @param mi_input_apply_filter Logical; If \code{TRUE} then toggle data frame used
#'   to build events to "mi_filtered_dat", which is pre-filtered data frame from
#'   IDEAFilter
#' @param mi_loaded_adams A character vector of available dataframes in
#'   mi_datafile
#' @param mi_datafile A list of data frames
#' @param mi_filtered_dat The output from IDEAFilter, a pre-filtered dataset
#' @param mi_usubjid Character patient number, in the standard USUBJID format
#'
#' @import dplyr
#' @importFrom shinyjs alert
#' @importFrom purrr map_chr
#'
#' @return Data frame standardized from various OCCDs class files
#'
#' @family indvExp Functions
#'   
org_df_events <- function(
    df_name
  , df_domain_abbr
  , df_desc
  , df_st_date_vars
  , event_desc_vars
  , event_desc
  , mi_input_checkbox
  , mi_input_apply_filter
  , mi_loaded_adams
  , mi_datafile
  , mi_filtered_dat
  , mi_usubjid
){
  if (df_name %in% mi_loaded_adams & df_domain_abbr %in% c(mi_input_checkbox)) {
    
    # do any of the df_st_date_vars exist?
    if(any(df_st_date_vars %in% colnames(mi_datafile[[df_name]]))){
      
      # If so, which df_st_date_var should we use? They should be ordered
      # left-to-right from most-preferred to least-preferred
      st_date_var_str <- df_st_date_vars[df_st_date_vars %in% colnames(mi_datafile[[df_name]])][1]
      st_date_var <- sym(st_date_var_str)
      
      dat <- 
        # conditionally toggle which dataset is used
        (if(mi_input_apply_filter == T) mi_datafile[[df_name]] %>% semi_join(mi_filtered_dat) else mi_datafile[[df_name]]) %>% 
        filter(USUBJID == mi_usubjid) %>%
        filter(!is.na(!!st_date_var) ) %>%
        mutate(EVENTTYP = df_desc, DOMAIN = df_domain_abbr) %>%
        select(USUBJID, EVENTTYP, !!st_date_var, one_of(event_desc_vars), DOMAIN) %>%
        distinct() %>%
        mutate(
          START = !!st_date_var,
          END = NA,
          tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)), # disp chr in DT
          tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END))      # disp chr in DT
          ) %>% 
        mutate_(
          DECODE = event_desc
        ) %>%
        select(-starts_with(df_domain_abbr)) %>%
        distinct(.keep_all = TRUE)
    } else{
      if(df_domain_abbr %in% c(mi_input_checkbox)){
        shinyjs::alert(paste0("Cannot add Adverse Events: no ", st_date_var_str, " variable exists in the loaded ", df_name, "."))
      }
      dat <- NULL
    }
  } else {
    dat <- NULL
  }
  return(dat)
}
