IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  
  dataselected <- callModule(selectData, id = NULL, datafile)
  # observe(print(dataselected()))
  
  # seltypes <- c(" ")  # initialize seltypes

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
  req(!is.null(dataselected()))
    
  datakeep <- reactive({ datafile()[dataselected()] })
  
  # Guard against user forgetting to select an ADSL dataset
  if (!"ADSL" %in% names(datakeep())) {
      shinyjs::alert("An ADSL dataset is required.")
      return()
  } 
       
  # The rest of the widgets will be shown after the USUBJID has been selected
  subj <- unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
  
  updateSelectInput(
    session = session,
    inputId = "selPatNo",
    choices = c(" ",subj),
    selected = " "
  )
  shinyjs::show(id = "selPatNo")
  })
  return(dataselected)
  
} # IndvExpl1Initial
