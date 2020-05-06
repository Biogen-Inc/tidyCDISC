######################################################################################
# a module that will interface with the data import module and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################

# User Interface (Modals)
dataComplyUI <- function(id, label = "Chec if Data Complies with Rules") {
  
  ns <- NS(id)
  
}

# Datafile is a list of data frames
# rules is a DF with 3 columns: 
# (1) data frame name
# (2) Variable names needed for #3 or #4 below
# (3) Variables that should be in there for the app to make sense, but sure, if you're
#     desperate, they can make the column blank and it won't break the app
# (4) variables and every one of them has to be populated or things are going to break hard


idea_hard_rules <- list(
  ADSL = list(need = c("USUBJID"),
              warn = c("USUBJID")),
  ADLB = list(need = c("USUBJID"),
              warn = c("USUBJID", "LBDT", "LBSTNRLO", "LBSTNRHI")),
  ADMH = list(need = c("USUBJID", "MHCAT"),
              warn = c("USUBJID", "MHCAT", "MHSTDTC", "MHENDTC", "MHDECOD", "MHTERM")),
  ADCM = list(need = c("USUBJID"),
              warn = c("USUBJID", "CMSTDT", "CMDECOD")),
  ADAE = list(need = c("USUBJID"),
              warn = c("USUBJID", "AESTDT", "AEDECOD", "AESEV", "AESER"))
)
idea_hard_rules



idea_dfWith_rules <- list(
  PARAMCD = list(need = c("USUBJID"),
                 warn = c("USUBJID", "AVISITN", "VISIT", "AVISIT", "PARAMCD", "PARAM", "AVAL", "CHG", "BASE"))
)
idea_dfWith_rules

dataComply <- function(input, output, session, datafile = reactive(NULL), hard_rules, dfWith_rules ) {
  
  # any time the reactive datafiles changes, run this code
  observeEvent(datafile(), {
    
    # check to see if hard_rules are met for each DF in the list
    
    
    
    # check to see if dfWith_rules are met for applicable DFs in the list
    if(!is.null(dfWith_rules)){
      bds <- reactive({ datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))] })
    }
    
    
    
    if(str_detect(input$myBrowser, "IE")){
      showModal( 
         modalDialog(footer = NULL, glue("This web app doesn't function with Internet Explorer. Please use a modern browser such as Google Chrome.")
      ))
    }    
  })
  
}
















