IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  
  seltypes <- c(" ","MEDS","LABS")  # initialize seltypes
  
  # Hide widgets until the input file has been selected
  # shinyjs::hide(id = "selPatNo")
  shinyjs::hide(id = "selType")
  shinyjs::hide(id = "selLabCode")
  shinyjs::hide(id = "checkGroup")
  shinyjs::hide(id = "eventsTable")
  shinyjs::hide(id = "hr2")
  shinyjs::hide(id = "hr3")
  
  observe({
    
    if (!"ADCM" %in% dataselected()) {
      seltypes <- seltypes[!seltypes == "MEDS"]
    }
    if (!"ADLB" %in% dataselected()) {
      seltypes <- seltypes[!seltypes == "LABS"]
    }

    updateSelectInput(
      session = session,
      inputId = "selType",
      choices = seltypes,
      selected = " "
    )
  })
  
  observe({
  subj <- unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
  
  # The rest of the widgets will be shown after the USUBJID has been selected
  shinyjs::show(id = "selPatNo")
  
  updateSelectInput(
    session = session,
    inputId = "selPatNo",
    choices = c(" ",subj),
    selected = " "
  )
  })
  return(seltypes)
  
} # IndvExpl1Initial
