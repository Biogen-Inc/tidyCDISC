
#' Build Events Data Frame
#'
#' Gather various shiny inputs from OCCDs datasets (if they exist) and return a
#' single standardized events dataset
#'
#' @param input_checkbox A character vector of event names selected. Some
#'   possible event names include "Milestones", "Adverse Events", "Con Meds",
#'   "Labs", "Medical History", etc...
#' @param input_apply_filter Logical; If \code{TRUE} then toggle data frame used to build
#'   events to "my_filtered_dat", which is pre-filtered data frame from IDEAFilter
#' @param my_loaded_adams A character vector of available dataframes in
#'   my_datafile
#' @param my_datafile A list of data frames
#' @param my_filtered_dat The output from IDEAFilter, a pre-filtered dataset
#' @param my_usubjid Character patient number, in the standard USUBJID format 
#'
#' @import dplyr
#' @importFrom shinyjs alert
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#'
#' @return Data frame standardized to include events from various OCCDs class
#'   files
#'
#' @family indvExp Functions
#' @noRd
#'   
build_events <- function(
  input_checkbox
  , input_apply_filter
  , my_loaded_adams
  , my_datafile
  , my_filtered_dat
  , my_usubjid
){

  
  # If Adverse Events (adae) selected & loaded, and If correct date variable exists
  # then convert the adae data frame to a standard format.
  ae_rec <- org_df_events(
    df_name = "ADAE", df_domain_abbr = "AE", df_desc = "Adverse Events"
    , df_st_date_vars = c("AESTDT","ASTDT") # from left to right, use first var that exists
    , event_desc_vars = c("AEDECOD","AESEV","AESER")
    , event_desc = 'paste0(AEDECOD, ", AESEV: ", AESEV, ", AESER: ", AESER)'
    , mi_input_checkbox = input_checkbox
    , mi_input_apply_filter = input_apply_filter
    , mi_usubjid = my_usubjid
    , mi_loaded_adams = my_loaded_adams
    , mi_datafile = my_datafile
    , mi_filtered_dat = my_filtered_dat
  )
  
  ########
  # ADSL #
  ########
  
  # If "Milestones" is selected, grab any date variables (var name ends with
  # "DT") then convert the data frame to a standard format.
  
  if ("ADSL" %in% my_loaded_adams & "DS" %in% c(input_checkbox)) {
    
    # organizing our ADSL labels for merging below
    adsl <- data.frame(my_datafile[["ADSL"]])
    n <- ncol(adsl)
    # "label table" for all adsl columns
    labs <- 
      data.frame(event_var = colnames(adsl),
                 DECODE = purrr::map_chr(1:n, function(x) attr(adsl[[x]], "label") )
      ) %>%
      mutate(event_var = as.character(event_var))
    
    # date columns we are going to select below
    adsl_date_cols <- adsl %>%
      filter(USUBJID == my_usubjid) %>%
      select(USUBJID,ends_with("DT")) %>%
      colnames()
    
    ds_rec <- (if(input_apply_filter == TRUE) adsl %>% semi_join(my_filtered_dat) else adsl) %>%
      filter(USUBJID == my_usubjid) %>%
      select(all_of(adsl_date_cols)) %>%
      distinct() %>%
      tidyr::pivot_longer(-USUBJID, names_to = "event_var", values_to = "START") %>%
      subset(!is.na(START)) %>%
      left_join(labs, by = "event_var") %>% # DECODE variable exists in "labs"
      arrange(START)%>%
      mutate(EVENTTYP = "Milestones", DOMAIN = "DS",
             END = NA,
             tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)), # disp chr in DT
             tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END))      # disp chr in DT
      ) %>%
      distinct(USUBJID, EVENTTYP, START, END,
               tab_st,
               tab_en,
               DECODE, DOMAIN)%>%
      select(-starts_with("DS"))
    
    
  } else {
    ds_rec <- NULL
  }
  
  # If con meds (adcm) selected & loaded, and If correct date variable exists
  # then convert the adae data frame to a standard format.
  cm_rec <- org_df_events(
    df_name = "ADCM", df_domain_abbr = "CM", df_desc = "Concomitant Meds"
    , df_st_date_vars = c("CMSTDT", "CMSTDTC", "ASTDT")# from left to right, use first var that exists
    , event_desc_vars = "CMDECOD"
    , event_desc = 'CMDECOD'
    , mi_input_checkbox = input_checkbox
    , mi_input_apply_filter = input_apply_filter
    , mi_usubjid = my_usubjid
    , mi_loaded_adams = my_loaded_adams
    , mi_datafile = my_datafile
    , mi_filtered_dat = my_filtered_dat
  )
  
  # If Lab (adlb) selected & loaded, and If correct date variable exists
  # then convert the adae data frame to a standard format.
  lb_rec <- org_df_events(
    df_name = "ADLB", df_domain_abbr = "LB", df_desc = "Lab Results"
    , df_st_date_vars = c("LBDT") # from left to right, use first var that exists
    , event_desc_vars = ""
    , event_desc = "'Labs Drawn'"
    , mi_input_checkbox = input_checkbox
    , mi_input_apply_filter = input_apply_filter
    , mi_usubjid = my_usubjid
    , mi_loaded_adams = my_loaded_adams
    , mi_datafile = my_datafile
    , mi_filtered_dat = my_filtered_dat
  )
  
  # If Chem Lab (adlbc) selected & loaded, and If correct date variable exists
  # then convert the adae data frame to a standard format.
  lc_rec <- org_df_events(
    df_name = "ADLBC", df_domain_abbr = "LC", df_desc = "Chem Lab Results"
    , df_st_date_vars = c("ADT") # from left to right, use first var that exists
    , event_desc_vars = ""
    , event_desc = "'Chem Labs Drawn'"
    , mi_input_checkbox = input_checkbox
    , mi_input_apply_filter = input_apply_filter
    , mi_usubjid = my_usubjid
    , mi_loaded_adams = my_loaded_adams
    , mi_datafile = my_datafile
    , mi_filtered_dat = my_filtered_dat
  )
  
  # Medical history (which contains several categories that get treated as their own group)
  if ("ADMH" %in% my_loaded_adams & "MH_" %in% substring(input_checkbox, 1, 3)) {
    # if the date column exists in the data set, build the data
    if("MHSTDTC" %in% colnames(my_datafile[["ADMH"]])){
      mh_rec <- 
        # conditionally toggle which dataset is used
        (if(input_apply_filter == TRUE) my_datafile[["ADMH"]] %>% semi_join(my_filtered_dat) else my_datafile[["ADMH"]]) %>%
        filter(USUBJID == my_usubjid) %>%
        mutate(EVENTTYP = str_to_title(MHCAT), #used to be "Medical History",
               
               # Create a domain name based on the initials of the med hist category, appending "MH_" prefix
               DOMAIN = paste0("MH_",sapply(strsplit(MHCAT, " "), function(x){
                 toupper(paste(substring(x, 1, 1), collapse = ""))})),
               
               # Some date imputation when missing: default to maximum time period as possible when date is vague
               has_end = ifelse(MHENDTC == "" | is.na(MHENDTC), FALSE, TRUE),
               START = as.Date(case_when(
                 nchar(MHSTDTC) == 10 ~ MHSTDTC,
                 nchar(MHSTDTC) == 7 ~ paste0(MHSTDTC,"-01"),
                 nchar(MHSTDTC) == 4 ~ paste0(MHSTDTC,"-01-01"),
                 TRUE ~ NA_character_)),
               END = as.Date(case_when(
                 nchar(MHENDTC) == 10 ~ MHENDTC,
                 has_end & nchar(MHENDTC) == 7 ~ paste0(MHENDTC,"-28"),
                 has_end & nchar(MHENDTC) == 4 ~ paste0(MHENDTC,"-12-31"),
                 has_end == FALSE & nchar(MHSTDTC) == 7  ~ paste0(MHSTDTC,"-28"),
                 has_end == FALSE & nchar(MHSTDTC) == 4 ~ paste0(MHSTDTC,"-12-31"),
                 TRUE ~ NA_character_)),
               tab_st = ifelse(MHSTDTC == "", NA_character_, MHSTDTC), # disp chr in DT
               tab_en = ifelse(MHENDTC == "", NA_character_, MHENDTC), # disp chr in DT
               DECODE = ifelse(is.na(MHDECOD) | MHDECOD == "", MHTERM, MHDECOD),
               sort_start = ifelse(is.na(START), as.Date("1900-01-01"), START) # if missing, order those first
        ) %>%
        arrange(sort_start) %>%
        distinct(USUBJID, EVENTTYP, START, END, tab_st, tab_en, DECODE, DOMAIN) %>%
        distinct(.keep_all = TRUE)
    } else{
      if("MH_" %in% substring(input_checkbox, 1, 3)){
        shinyjs::alert(paste("Cannot add Medical History: no MHSTDTC variable exists in the loaded ADMH"))
      }
      mh_rec <- NULL
    }
  } else {
    mh_rec <- NULL
  }
  strng <- input_checkbox
  
  # Remove NULLs from the list
  uni_list <- list(ds_rec, ae_rec, cm_rec, lb_rec, mh_rec, lc_rec)
  uni_list <- uni_list[!sapply(uni_list,is.null)]
  
  
  uni_rec <- 
    do.call("rbind", uni_list) %>%
    mutate(ord = ifelse(EVENTTYP == "DS", 1, 0),
           sort_start = if_else(is.na(START), as.Date("1900-01-01"), START), # If start is null, show at beginning of table
           END = as.Date(END, origin="1970-01-01")
    ) %>% # for ties, show DS last
    arrange(sort_start, ord, EVENTTYP) %>%
    filter(DOMAIN %in% c(strng)) %>%
    select(-USUBJID, -ord, -sort_start)
  
  return(uni_rec)
}
