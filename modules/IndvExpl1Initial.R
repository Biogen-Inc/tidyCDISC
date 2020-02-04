IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  
  # observe(print(dataselected()))
  
  seltypes <- c(" ")  # initialize seltypes

  # Hide widgets until the input file has been selected
  shinyjs::hide(id = "selPatNo")
  shinyjs::hide(id = "selType")
  shinyjs::hide(id = "selLabCode")
  shinyjs::hide(id = "checkGroup")
  shinyjs::hide(id = "eventsTable")
  shinyjs::hide(id = "hr2")
  shinyjs::hide(id = "hr3")
  
  observe({
    
    req(!is.null(datafile()))
    
    # if ADCM or ADLB were selected, add to the seltypes selectInput list
    
    if ("ADCM" %in% dataselected()) {
      seltypes <- c(seltypes,"MEDS")
    }
    if ("ADLB" %in% dataselected()) {
      seltypes <- c(seltypes,"LABS")
    }

    # print(paste("Indv#1 seltypes is",paste(seltypes,collapse = " ")))
    
    updateSelectInput(
      session = session,
      inputId = "selType",
      choices = seltypes,
      selected = " "
    )
    
  })
  
  observe({
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
  return(seltypes)
  
} # IndvExpl1Initial
