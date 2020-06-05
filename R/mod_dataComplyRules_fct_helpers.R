################################################################################
# Inputs for Data Compliance Module
#
# Module found in "R/mod_dataComply.R" Module description: a module that
# will interface with a list of data frames and either (I) display an error if
# needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense,
# but they can continue if they wish.
###############################################################################

# RULES for all dfs
all_df_rules <- list(
  error = c("USUBJID"),
  warn = c("")
)

# Rules for explicit dfs
expl_rules <- 
  list(
    ADLB = list(error = c(""),
                warn = c("LBDT", "LBSTNRLO", "LBSTNRHI")),
    ADMH = list(error = c("","MHCAT"),
                warn = c("MHSTDTC", "MHENDTC", "MHDECOD", "MHTERM")),
    ADCM = list(error = c(""),
                warn = c("CMSTDT", "CMDECOD")),
    ADAE = list(error = c(""),
                warn = c("AESTDT", "AEDECOD", "AESEV", "AESER"))
  )

# Rules for dfs that include certain variables
df_incl_rules <- 
  list(
    PARAMCD = list(error = c(""),
                   warn = c("AVISITN", "VISIT", "AVISIT", "PARAMCD", "PARAM", "AVAL", "CHG", "BASE"))
  )





#' Gather Rules For Help UI
#'
#' Gather Rules into a shiny tagList to be included in the UI for "help"
#'   purposes, conditionally containing different required and/or recommended rule
#'   sets (if they exist).
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param all_df_rules A named list of variables names that should result in
#'   \code{error} or \code{warn} if variables do not exist or are missing for
#'   ANY DATAFRAME uploaded.
#' @param expl_rules A named list dataframes containing named lists of variables
#'   names that should result in \code{error} or \code{warn} if variables do not
#'   exist or are missing for SPECIFIED DATAFRAMES uploaded
#' @param df_incl_rules A named list of data frame variables containing a named list of
#'   variables names that should result in \code{error} or \code{warn} if
#'   variables do not exist or are missing
#'
#'   DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom rlang is_empty
#' @importFrom data.table rbindlist
#' @importFrom purrr map2 pmap
#' @noRd
#' 
gather_rules <- function(input, output, session,
                         all_df_rules = list( list(error = c(""), warn = c("")) ),
                         expl_rules = list( list(error = c(""), warn = c("")) ),
                         df_incl_rules = list( list(error = c(""), warn = c("")) )
) {
  
  if((is.null(expl_rules) | is.null(names(expl_rules))) & 
     (is.null(df_incl_rules) | is.null(names(df_incl_rules))) &
     (is.null(all_df_rules) | is.null(names(all_df_rules))) 
  ) {
    stop("No Rules Supplied. Without rules, the data compliance module is useless. Please remove the Module.")
  }
  
  # rules that apply to all df's loaded
  if(!is.null(all_df_rules) & !is.null(names(all_df_rules))) {
    err0 <- unique(unlist(all_df_rules$error)) 
    err <- if(err0 == "") character(0) else err0
    wrn0 <- unique(unlist(all_df_rules$warn))
    wrn <- if(wrn0 == "") character(0) else wrn0
  } else {
    err <- character(0) # ""
    wrn <- character(0) # ""
  }
  
  #  explicit rules for specific df's
  if(!is.null(expl_rules) & !is.null(names(expl_rules))) {
    hdf <- 
      lapply(expl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df")
    
    hdf_err <- 
      hdf %>%
      distinct(df,error) %>%
      subset(error != '') %>%
      group_by(df) %>%
      summarize(p = paste(error, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df, p, sep = ": ")) %>%
      distinct%>%
      pull(f)
    
    hdf_wrn <-
      hdf %>%
      distinct(df,warn) %>%
      subset(warn != "") %>%
      group_by(df) %>%
      summarize(p = paste(warn, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df, p, sep = ": ")) %>%
      pull(f)
    
  } else {
    hdf_err <- character(0) # "" # data.frame(df = character(), error = character(), warn = character())
    hdf_wrn <- character(0) # ""
  }
  
  # Rules for data frames containing certain vars (df_vars)
  if(!is.null(df_incl_rules) & !is.null(names(df_incl_rules))) {
    dfw <-
      lapply(df_incl_rules, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df_var")
    
    dfw_err <-
      dfw %>%
      distinct(df_var,error) %>%
      subset(error != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(error, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
    dfw_wrn <-
      dfw %>%
      distinct(df_var,warn) %>%
      subset(warn != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(warn, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
  } else {
    # dfw <- data.frame(df_var = character(), error = character(), warn = character())
    dfw_err <- character(0) # ""
    dfw_wrn <- character(0) # ""
  }
  
  ui<- tagList( # start UI of modal
    
    # Notes... can't add footer to bsModal
    # HTML("Note: The user will be alerted if files are uploaded and these variables (1) don't exist or (2) are completely empty / missing"),
    
    # Rules for All Data Sets
    tagList(
      if(!is_empty(err) | !is_empty(wrn)){
        tagList(
          br(),
          h5(strong("Rules for All Data Sets")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required: ",paste(err, collapse = ", "))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended: ",paste(wrn, collapse = ", "))),
                br(),br(),
              )
            } else {""}
          ))
        )
      }),
    
    # Specific Rules for Specific Data Sets
    tagList(
      if(!is_empty(hdf_err) | !is_empty(hdf_wrn)){
        tagList(
          br(),
          h5(strong("Specific Rules for Specific Data Sets")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(hdf_err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(hdf_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(hdf_wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(hdf_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""}
          )) # end div
        )
      }),
    
    # Rules for Data Sets That Contain Certain Variables
    tagList(
      if(!is_empty(dfw_err) | !is_empty(dfw_wrn)){
        tagList(
          br(),
          h5(strong("Rules for Data Sets That Contain Certain Variables")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(dfw_err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(dfw_wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""}
          )) # end div
        )
      })
  ) # end of tagList UI
  
  return(ui)
  
  
}
###########################################################
# Run it help module UI once, before loading the app
###########################################################
rulesUI <- gather_rules(all_df_rules = all_df_rules,
                        expl_rules = expl_rules,
                        df_incl_rules = df_incl_rules)


