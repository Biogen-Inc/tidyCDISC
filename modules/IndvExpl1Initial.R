IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  
  
  # # Only select data that starts with AD followed by one or more alphanumerics or underscore
  # req(!is.null(datafile()))
  # sasdata <- toupper(names(datafile()))
  # sasdata <- names(which(sapply(sasdata,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
  
  # Removed - Issue #74
  # dataselected <- callModule(selectData, id = NULL, datafile)

  

  # Hide widgets until the input file has been selected
  shinyjs::hide(id = "selPatNo")
  shinyjs::hide(id = "selType")
  shinyjs::hide(id = "selLabCode")
  shinyjs::hide(id = "checkGroup")
  shinyjs::hide(id = "eventsTable")
  shinyjs::hide(id = "hr2")
  shinyjs::hide(id = "hr3")
  
  observe({
  
  # make sure selectData has been run
  # req(!is.null(dataselected())) #74
  req(!is.null(datafile())) #74
    
  # datakeep <- reactive({ datafile()[dataselected()] }) #74
  # datakeep <- reactive({ datafile() }) #74
  
  # Guard against user forgetting to select an ADSL dataset
  # if (!"ADSL" %in% names(datakeep())) {
  #     shinyjs::alert("An ADSL dataset is required.")
  #     return()
  # } #74 redundant
       
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
  # return(datakeep()) #74
  
} # IndvExpl1Initial
