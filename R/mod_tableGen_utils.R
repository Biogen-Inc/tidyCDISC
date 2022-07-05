#' Function to bind data rows from the list of user supplied data frames
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param ADSL A dataframe which contains the ADSL data
#' 
#' @export
#' @keywords tabGen_repro
#' 
prep_bds <- function(datafile, ADSL) {
  init <- sapply(datafile, function(x) "PARAMCD" %in% colnames(x) & !("CNSR" %in% colnames(x)))
  BDS <- datafile[init[substr(names(init),1,4) != "ADTT"]] # remove TTE class df's because `AVISIT` col doesn't exist in that class of df
  
  PARAMCD <- map(BDS, ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
  
  if (!is_empty(PARAMCD)) {
    # Bind all the PARAMCD files 
    all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
      arrange(USUBJID, AVISITN, PARAMCD) %>% 
      select(USUBJID, AVISITN, AVISIT, PARAMCD, PARAM, AVAL, CHG, data_from)
    # Join ADSL and all_PARAMCD
    combined_data <- inner_join(ADSL, all_PARAMCD, by = "USUBJID")
  } else {
    combined_data <- ADSL %>%
      mutate(data_from = "ADSL", PARAMCD = NA, AVAL = NA, CHG = NA)
  }
  
  combined_data <- varN_fctr_reorder(combined_data) # add this after filter?
  
  return(combined_data)
}



#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' @noRd
#' 
numeric_stan_table <- function(input_recipe){
  ifelse(is.null(input_recipe) | input_recipe == "NONE", 
         0,
         as.numeric(gsub(" ","",gsub(":","",stringr::word(start = 2, substr(input_recipe, 1, 9)))))
  )
}


#' Function to pre-filter the ADSL depending on the stan table selected
#' 
#' @param ADSL an ADSL data.frame
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' 
#' @export
#' @keywords tabGen_repro
#' 
prep_adsl <- function(ADSL, input_recipe) { #, stan_table_num
  stan_table_num <- numeric_stan_table(input_recipe)
  dat <- ADSL
  msg <- ""
  if(!is.null(input_recipe)){ # if recipe has initialized...
    if(stan_table_num %in% c(3)){ 
      if("FASFL" %in% colnames(dat)){
        dat <- dat %>% filter(FASFL == 'Y')
        msg <- "Population Set: FASFL = 'Y'"
      } else{
        if("ITTFL" %in% colnames(dat)){
          dat <- dat %>% filter(ITTFL == 'Y')
          msg <- "Population Set: ITTFL = 'Y'"
        } else{
          msg <- "Variable 'FASFL' or 'ITTFL' doesn't exist in ADSL. STAN table not displayed because filter \"FASFL == 'Y'\" or \"ITTFL == 'Y'\"cannot be applied!"
          stop(msg)
        }
      }
    }
    if(stan_table_num %in% c(18:39, 41:47, 51:53)){ # 5, nate requested 5 auto-filter be removed
      if("SAFFL" %in% colnames(dat)){
        dat <- dat %>% filter(SAFFL == 'Y')
        msg <- "Population Set: SAFFL = 'Y'"
      } else{
        msg <- "Variable 'SAFFL' doesn't exist in ADSL. STAN table not displayed because filter \"SAFFL == 'Y'\" cannot be applied!"
        stop(msg)
      }
    }
  }
  
  return(list(data = dat, message = msg))
}

#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param ADSL A dataframe which contains the ADSL data
#' 
#' @noRd
#' @keywords tabGen_repro
#' 
clean_ADAE <- function(datafile, ADSL) {
  if("ADAE" %in% names(datafile)){
    # find columns the ADAE & ADSL have in common (besides Usubjid), remove
    # them from the ADAE, so that the ADSL cols are used instead. Then join
    # on usubjid and re-order the colnames to match the adae
    adae_cols <- colnames(datafile$ADAE)
    common_cols <- dplyr::intersect(adae_cols, colnames(ADSL))
    com_cols_excp_u <- common_cols[common_cols != "USUBJID"]
    adae_adsl <- datafile$ADAE %>% 
      select(-one_of(com_cols_excp_u)) %>%
      inner_join(ADSL, by = "USUBJID")
    preferred_col_order <- c(adae_cols, dplyr::setdiff(colnames(ADSL), adae_cols))
    if(all(sort(colnames(adae_adsl)) == sort(preferred_col_order))){
      varN_fctr_reorder(adae_adsl[,preferred_col_order]) # add this after filter?
    } else {
      varN_fctr_reorder(adae_adsl)
    }
  } else {
    varN_fctr_reorder(ADSL)
  }
}

#' Function to pre-filter the ADAE depending on the stan table selected
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param ADSL an ADSL data.frame
#' @param input_recipe The shiny input that keeps track of the recipe selected
#' 
#' @export
#' @keywords tabGen_repro
#' 
prep_adae <- function(datafile, ADSL, input_recipe) { #, stan_table_num
  stan_table_num <- numeric_stan_table(input_recipe)
  dat <- clean_ADAE(datafile = datafile, ADSL = ADSL)
  msg <- ""
  if(!is.null(input_recipe) & "ADAE" %in% names(datafile)){ # if recipe has initialized...
    
    if(stan_table_num %in% c(25, 26)){
      if("AESEV" %in% colnames(dat)){
        dat <- dat %>% filter(AESEV == 'SEVERE')
        msg <- "AESEV = 'SEVERE'"
      } else {
        msg <- "Variable 'AESEV' doesn't exist in ADAE. STAN table not displayed because filter \"AESEV = 'SEVERE'\" cannot be applied!"
        stop(msg)
      }
      
    } else if(stan_table_num == 29){
      if("AREL" %in% colnames(dat)){
        dat <- dat %>% filter(AREL == 'RELATED')
        msg <- "AREL = 'RELATED'"
      } else {
        msg <- "Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!"
        stop(msg)
      }
      
    } else if(stan_table_num %in% c(30, 31)){
      if("AESER" %in% colnames(dat)){
        dat <- dat %>% filter(AESER == 'Y')
        msg <- "AESER = 'Y'"
      } else {
        msg <- "Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!"
        stop(msg)
      }
      
    } else if(stan_table_num == 33){
      if("AREL" %in% colnames(dat) & "AESER" %in% colnames(dat)){
        dat <- dat %>% filter(AREL == 'RELATED' & AESER == 'Y')
        msg <- "AREL = 'RELATED'<br/>AESER = 'Y'"
      } else if("AREL" %in% colnames(dat) & !("AESER" %in% colnames(dat))){
        dat <- dat %>% filter(AREL == 'RELATED')
        msg <- "AREL = 'RELATED'<br/>Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!"
        stop("Variable 'AESER' doesn't exist in ADAE. STAN table not displayed because filter \"AESER = 'Y'\" cannot be applied!")
      } else if(!("AREL" %in% colnames(dat)) & "AESER" %in% colnames(dat)){
        dat <- dat %>% filter(AESER == 'Y')
        msg <- "Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!<br/>AESER = 'Y'"
        stop("Variable 'AREL' doesn't exist in ADAE. STAN table not displayed because filter \"AREL = 'RELATED'\" cannot be applied!")
      } else{
        msg <- "Variables 'AREL' & 'AESER' do not exist in ADAE. STAN table not displayed because filters \"AREL = 'RELATED'\" and \"AESER = 'Y'\" cannot be applied!"
        stop(msg)
      }
    } else if(stan_table_num == 34){
      if("AEACN" %in% colnames(dat)){
        dat <- dat %>% filter(AEACN == 'DRUG WITHDRAWN')
        msg <- "AEACN = 'DRUG WITHDRAWN'"
      } else{
        msg <- "Variable 'AEACN' doesn't exist in ADAE. STAN table not displayed because filter \"AEACN = 'DRUG WITHDRAWN'\" cannot be applied!"
        stop(msg)
      }
    } else if(stan_table_num == 36){ #AEACNOTH contains 'Withdrawal" and "Study"
      if("AEACNOTH" %in% colnames(dat)){
        dat <- dat %>%
          filter(stringr::str_detect(tolower(AEACNOTH),"withdrawal") &
                   stringr::str_detect(tolower(AEACNOTH),"study"))
        msg <- "AEACNOTH Contains 'withdrawal' and 'study'"
      } else{
        msg <- "Variable 'AEACNOTH' doesn't exist in ADAE. STAN table not displayed because filter \"AEACNOTH Contains 'withdrawal' and 'study'\" cannot be applied!"
        stop(msg)
      }
    } else if(stan_table_num == 38){
      if("AEACN" %in% colnames(dat)){
        dat <- dat %>% filter(AEACN %in% c('DRUG INTERRUPTED', 'DRUG REDUCED', 'DOSE REDUCED', 'DRUG INCREASED', 'DOSE INCREASED'))
        msg <- "AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED')"
      } else{
        msg <- "Variable 'AEACN' doesn't exist in ADAE. STAN table not displayed because filter \"AEACN IN ('DRUG INTERRUPTED', 'DOSE REDUCED', 'DOSE INCREASED')\" cannot be applied!"
        stop(msg)
      }
    } else if(stan_table_num == 39){
      if("TRTEMFL" %in% colnames(dat)){
        dat <- dat %>% filter(TRTEMFL == 'Y')
        msg <- "TRTEMFL = 'Y'"
      }else {
        msg <- "Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!"
        stop(msg)
      }
    }
    if(stan_table_num %in% c(25:26, 29:33)){
      if("TRTEMFL" %in% colnames(dat)){
        dat <- dat %>% filter(TRTEMFL == 'Y')
        msg <- paste0(msg, "<br/>TRTEMFL = 'Y'")
      } else {
        msg <- paste0(msg, "<br/>Variable 'TRTEMFL' doesn't exist in ADAE. STAN table not displayed because filter \"TRTEMFL = 'Y'\" cannot be applied!")
        stop(msg)
      }
    }
  }
  
  return(list(data = dat, message = msg))
}

#' Blood Chemistry PARAMCDs used to build STAN Table 41
#' @noRd
chem <- c(
  "ALT", "AST", "ALP", "BILI", "GGT", # Liver
  "BUN", "CREAT", # Renal
  "SODIUM", "K", "CL", "BICARB", # Electrolytes
  "GLUC", "CA", "PHOS", "ALB", "CHOL", "MG", "TRIG", "URATE" # Other
)

#' Hematology PARAMCDs used to build STAN Table 41
#' @noRd
hema <- c(
  # white blood cells 
  "LYM", "NEUT", "MONO", "EOS", "BASO", #"LYMLE", "NEUTLE", "MONOLE", "EOSLE", "BASOLE",
  "RBC", "HGB", "HCT", "PLAT"
)

#' Urinalysis PARAMCDs used to build STAN Table 41
#' @noRd
urin <- c(
  "SPGRAV", "PH", "COLOR", "OCCBLD",  "GLUCU",  "KETONES", #"GLUCQU", "KETONESQ",
  "PROTU", "MWBCQU", "MWBCU", "MRBCQU", "MRBCU"
)

#' A function that checks if certain parameters exist in any dataframe within a list of dataframes
#' @noRd
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param param_vector character vector of params to search the list of dataframes for
#' 
check_params <- function(datafile, param_vector) {
  # datafile <- datalist # for testing
  # param_vector <- chem # for testing
  
  param_dat <- datafile[sapply(datafile, function(x) "PARAMCD" %in% colnames(x)) & substr(names(datafile), 1, 4) == "ADLB"]
  
  # apply following code to data that contains paramcd
  if(!rlang::is_empty(param_dat)){
    param_lst <- purrr::map(names(param_dat), ~ 
                              param_dat[[.x]] %>%
                              filter(PARAMCD %in% param_vector) %>%
                              filter(!is.na(AVAL) & # only display if non-missing!
                                     !is.na(AVISIT) & 
                                     !(AVISIT %in% c(" ", "")) &
                                     stringr::str_detect(toupper(AVISIT),"UNSCHEDULED",negate = TRUE) #&
                                     # stringr::str_detect(toupper(AVISIT),"EARLY TERMINATION",negate = TRUE)
                                     ) %>%
                              distinct(PARAMCD, AVISIT, AVISITN) %>%
                              varN_fctr_reorder() %>%
                              mutate(PARAMCD = factor(PARAMCD, levels = param_vector)) %>%
                              arrange(PARAMCD, AVISIT)
    )
    param_vctr <- param_lst[[1]]$PARAMCD # Will this work if two ADLB's are uploaded?
    
    if(!rlang::is_empty(param_vctr)){
      visit_vctr <- param_lst[[1]]$AVISIT
      dat_lgls <- purrr::map_lgl(param_lst, ~length(.x) > 0)
      param_lgl <- any(dat_lgls)
      dat_names <- purrr::map_chr(dat_lgls, ~names(param_dat[.x]))
    } else {
      param_lgl <- FALSE
      dat_names <- NA_character_
      param_vctr <- NA_character_
      visit_vctr <- c("fake_weeky","fake_weeky2")
    }
  } else {
    param_lgl <- FALSE
    dat_names <- NA_character_
    param_vctr <- NA_character_
    visit_vctr <- c("fake_weeky","fake_weeky2")
  }
  return(
    list(
      exist = param_lgl,
      dat_name = dat_names,
      vctr = param_vctr,
      tp = visit_vctr
    )
  )
}

#' The smallest possible data set we could filter to semi-join later
#' 
#' @param datafile list of ADaM-ish dataframes 
#' @param input_filter_df The name of a dataset stored in `datafile`
#' 
#' @export
#' @keywords tabGen_repro
#' 
data_to_filter <- function(datafile, input_filter_df) {
  select_dfs <- datafile[input_filter_df]
  
  # Separate out non-BDS and BDS data frames. Note: join may throw some warnings
  # if labels are different between two datasets, which is fine! Just Ignore
  non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
  bds <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
  
  # Make CHG var doesn't exist, create the column and populate with NA
  PARAMCD_dat <- purrr::map(bds, ~ if(!"CHG" %in% names(.)) {purrr::update_list(., CHG = NA)} else {.})
  
  # Combine selected data into a 1 usable data frame
  if (!rlang::is_empty(PARAMCD_dat)) {
    all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from") %>% distinct(.keep_all = T)
    
    if (!rlang::is_empty(non_bds)){
      combined_data <- inner_join(non_bds %>% purrr::reduce(inner_join), all_PARAMCD)
    } else {
      combined_data <-all_PARAMCD
    }
  } else {
    combined_data <- non_bds %>% reduce(inner_join)
  }
  
  return(combined_data)
}



#' Function to clean and combine ADAE dataset with ADSL
#' 
#' @param x string, naming a data.frame.
#' @param ae_data data.frame, of the AE variety
#' @param bds_data data.frame, of the BDS variety
#' 
#' @export
#' @keywords tabGen_repro
#' 
data_to_use_str <- function(x, ae_data, bds_data) {
  if (x == "ADAE") { ae_data }
  else bds_data
}


#' Table Generator Pretty Block lookup table
#' 
#' A dataset used within the table generator module
#' to add pretty names for each stat block when displayed in the table
#' 
#' @format A data farm with 8 rows and 2 variables:
#' \describe{
#'   \item{Pattern}{standard statistical name}
#'   \item{Replacement}{"pretty" name for table output}
#' }
#' 
#' @keywords tabGen_repro
#' 
# pretty_blocks <- tidyr::tibble(
#   Pattern = c("MEAN", "FREQ", "CHG", "Y_FREQ", "MAX_FREQ", "NON_MISSING",
#               "NESTED_FREQ_DSC", "NESTED_FREQ_ABC"),
#   Replacement = c("Descriptive Statistics", 
#                   "Summary Counts", 
#                   "Descriptive Statistics of Change from Baseline",
#                   "Subject Count for those with 'Y' values",
#                   "Subject Count for maximum",
#                   "Subject Count for those with Non Missing values",
#                   "Subject Count at each variable level, sorted descending by total counts",
#                   "Subject Count at each variable level, sorted alphabetically by name")
# )
# usethis::use_data(pretty_blocks)
"pretty_blocks"

#' Table Generator Cicerone R6 Object 
#' 
#' This object is used within the table generator module
#' to add help text to the various fields using 
#' a help buttom
#' 
#' @importFrom cicerone Cicerone
#' 
#' @family tableGen Functions
#' @noRd
#' 
tg_guide <- cicerone::Cicerone$
  new()$
  step(
    "all-column-blocks",
    "Variables from Uploaded Data",
    "Variables are grouped by dataset name. From here, you can drag any variable bubble into the Variable drop zone, including PARAMCDs from BDS files"
  )$
  step(
    "sortable_agg",
    "Statistic Blocks",
    "Drag one of these blocks into the Stats drop zone to be paired with a corresponding Variable block on the left hand side.
    Additional input fields may appear on some stat blocks once dragged & dropped to allow for further customization, like choosing an AVIST for the 'MEAN' block."
  )$
  step(
    "all-output-blocks",
    "Drop Zones",
    "Blocks are analyzed by row, meaning the statistic block on the right will be applied to the variable block on the left."
  )$
  step(
    el = "COLUMN-wrapper",
    title = "Grouping Data",
    description = "Break down the table's statistics into levels of the chosen grouping variable"
  )$
  step(
    "filter-accordion",
    "Filter your Data",
    "To filter,  select the dataset(s) that contain the variable(s) you want to filter on.
    Select the variables and the corresponding values to include (or exclude)."
  )$
  step(
    "RECIPE",
    "Auto-Generated Tables",
    "Instead of dragging and dropping from scratch, create tables using STAN 2.0 presets. If you select NONE you can clear your workspace."
  )$
  step(
    "table_title",
    "Customize Title",
    "The sky is the limit! Add a table title that suits your needs."
  )$
  step(
    "download_table",
    "Keep a copy for your records",
    "Download a table in CSV or HTML format as seen in the app. RTF support coming soon!"
  )$
  step(
    "tableGen_ui_1-tblcode",
    "Produce this table outside of the app",
    "Have more data coming? Don't download the table, download the R script needed to produce the table. Then there's no need to even open up the app periodically to re-create your outputs!"
  )




