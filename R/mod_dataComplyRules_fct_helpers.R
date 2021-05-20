################################################################################
# Arguments / Inputs for Data Compliance Modules
# Used by Modules: mod_dataComplyRules.R & mod_dataComply.R
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
df_incl_rules_except_tte <- 
  list(
    PARAMCD = list(error = c("AVISITN", "AVISIT", "PARAMCD", "PARAM", "AVAL", "CHG", "BASE"),
                   warn = c(""))
  )
df_incl_rules <- 
  list(
    CNSR = list(error = c("PARAMCD", "CNSR", "AVAL"),
                   warn = c(""))
  )





#' Gather Rules For Help UI
#'
#' Gather Rules into a shiny tagList to be included in the UI for "help"
#' purposes, conditionally containing different required and/or recommended rule
#' sets (if they exist).
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param all_df_rules A double-nested list: inner list is named list of
#'   variables names that should result in \code{error} or \code{warn} if
#'   variables do not exist or are missing for ANY data frame. Outer list is
#'   unnamed.
#' @param expl_rules A double-nested list: outer list is a named list
#'   dataframes. Inner list (for each data frame) contains named lists of
#'   variables names that should result in \code{error} or \code{warn} if
#'   variables do not exist or are missing for the specified parent data frame
#' @param df_incl_rules A double-nested list: outer list is a named list
#'   variable names used to identify a particular class of data frame. For
#'   example, the variable name PARAMCD would id BDS class data frames, and the
#'   inner list would contain a named lists of variables names that should
#'   result in \code{error} or \code{warn} if variables do not exist or are
#'   missing for the implied parent data frame.
#' @param df_incl_rules_except_tte A double-nested list: outer list is a named
#'   list variable names used to identify a particular class of data frame. For
#'   example, the variable name PARAMCD would id BDS class data frames WHERE
#'   Time to event (TTE) is an exception, and the inner list would contain a
#'   named lists of variables names that should result in \code{error} or
#'   \code{warn} if variables do not exist or are missing for the implied parent
#'   data frame.
#'
#'
#' @import shiny
#' @import dplyr
#' @importFrom dplyr %>%
#' @importFrom rlang is_empty
#' @importFrom data.table rbindlist
#' @importFrom purrr map2 pmap
#'
#' @return An shiny tagList
#'
#' @family dataComply Functions
#'   
gather_rules <- function(input, output, session,
                         all_df_rules = list(error = c(""), warn = c("") ),
                         expl_rules = list( list(error = c(""), warn = c("")) ),
                         df_incl_rules = list( list(error = c(""), warn = c("")) ),
                         df_incl_rules_except_tte = list( list(error = c(""), warn = c("")) )
) {
  
  # If there are no rules supplied, alert R developer and suggest removing the
  # module from app
  if((is.null(expl_rules) | is.null(names(expl_rules))) & 
     (is.null(df_incl_rules) | is.null(names(df_incl_rules))) &
     (is.null(df_incl_rules_except_tte) | is.null(names(df_incl_rules_except_tte))) &
     (is.null(all_df_rules) | is.null(names(all_df_rules))) 
  ) {
    stop("No Rules Supplied. Without rules, the data compliance module is useless. Please remove the Module.")
  }
  
  # unlist rules that apply to all df's loaded and organize them into a vector
  # of easy to read strings
  if(!is.null(all_df_rules) & !is.null(names(all_df_rules))) {
    err0 <- unique(unlist(all_df_rules$error)) 
    err <- if(err0 == "") character(0) else err0
    wrn0 <- unique(unlist(all_df_rules$warn))
    wrn <- if(wrn0 == "") character(0) else wrn0
  } else {
    err <- character(0) # ""
    wrn <- character(0) # ""
  }
  
  # unlist explicit rules for specific df's, and organize them into a vector of
  # easy to read strings
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
    hdf_err <- character(0)
    hdf_wrn <- character(0)
  }
  
  # Unlist Rules for data frames containing certain vars (df_vars), and
  # organize them into a vector of easy to read strings
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
  
  # Unlist Rules for data frames containing certain vars (df_vars), and
  # organize them into a vector of easy to read strings
  if(!is.null(df_incl_rules_except_tte) & !is.null(names(df_incl_rules_except_tte))) {
    dfw_ette <-
      lapply(df_incl_rules_except_tte, data.frame, stringsAsFactors = FALSE) %>%
      data.table::rbindlist(fill=TRUE, idcol = "df_var")
    
    dfw_ette_err <-
      dfw_ette %>%
      distinct(df_var,error) %>%
      subset(error != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(error, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
    dfw_ette_wrn <-
      dfw_ette %>%
      distinct(df_var,warn) %>%
      subset(warn != "") %>%
      group_by(df_var) %>%
      summarize(p = paste(warn, collapse = ", ")) %>%
      ungroup() %>%
      mutate(f = paste(df_var, p, sep = ": ")) %>%
      pull(f)
    
  } else {
    # dfw_ette <- data.frame(df_var = character(), error = character(), warn = character())
    dfw_ette_err <- character(0) # ""
    dfw_ette_wrn <- character(0) # ""
  }
  
  ui<- tagList( 
    # start UI of modal
    
    # If they exist, format Rules for All Data Sets into a collapsed HTML string
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
    
    # If they exist, format Specific Rules for Specific Data Sets into a
    # collapsed HTML string
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
    
    # If they exist, format Rules for Data Sets That Contain Certain Variables
    # into a collapsed HTML string
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
      }),
    
    # If they exist, format Rules for Data Sets That Contain Certain Variables
    # into a collapsed HTML string
    tagList(
      if(!is_empty(dfw_ette_err) | !is_empty(dfw_ette_wrn)){
        tagList(
          br(),
          h5(strong("Rules for all Data Sets That Contain Certain Variables, except 'Time to Event'")),
          div(style = "font-size: 12px;", tagList(
            if(!is_empty(dfw_ette_err)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Required:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_ette_err, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""},
            if(!is_empty(dfw_ette_wrn)){
              tagList(
                HTML(paste0("&nbsp;&nbsp;&nbsp;Recommended:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                            paste(dfw_ette_wrn, collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))),
                br(),br(),
              )
            } else {""}
          )) # end div
        )
      })
    
  ) # end of tagList UI
  
  return(ui)
  
  
}

###############################################################
#
# Run gather_rules help module UI once, before loading the app
#
###############################################################
rulesUI <- gather_rules(all_df_rules = all_df_rules,
                        expl_rules = expl_rules,
                        df_incl_rules = df_incl_rules,
                        df_incl_rules_except_tte = df_incl_rules_except_tte)


