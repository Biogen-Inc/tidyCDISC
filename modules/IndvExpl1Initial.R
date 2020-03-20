IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  
  
  # Hide widgets until the input file has been selected
  shinyjs::hide(id = "selPatNo")
  shinyjs::hide(id = "hr2")
  shinyjs::hide(id = "events_header")
  shinyjs::hide(id = "checkGroup")
  shinyjs::hide(id = "eventsTable")
  shinyjs::hide(id = "hr3")
  shinyjs::hide(id = "plot_header")
  shinyjs::hide(id = "plot_adam")
  shinyjs::hide(id = "plot_param")
  shinyjs::hide(id = "visit_var")
  shinyjs::hide(id = "plot_hor")
  
  
  
  my_loaded_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  
  observe({
    # make sure selectData has been run
    req(!is.null(datafile())) #74
    
    
    # The rest of the widgets will be shown after the USUBJID has been selected
    subj <- unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
    
    updateSelectInput(
      session = session,
      inputId = "selPatNo",
      choices = c(" ",subj),
      selected = " "
    )
    shinyjs::show(id = "selPatNo") # ac: display the patient number dropdown
  })
  
  # return(dataselected) # #74
  return(my_loaded_adams) #74
  
} # IndvExpl1Initial
