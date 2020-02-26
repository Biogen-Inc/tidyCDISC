IndvExpl2SelPatno <- function(input, output, session, datafile, dataselected){
  
  ns <- session$ns

# observeEvent for inputselPatno 
observeEvent(input$selPatNo, {
  
  req(input$selPatNo != " ") # selPatNo cannot be blank
  
  shinyjs::hide(id = "selType")
  shinyjs::hide(id = "selLabCode")
  
  # Clear eventsTable
  output$eventsTable <- DT::renderDataTable({
    NULL
  })
  
  output$demogInfo <- DT::renderDataTable({
    
    adsl_rec <- datafile()[["ADSL"]] %>%
      filter(USUBJID == input$selPatNo) %>%
      select(USUBJID, COUNTRYC, AGE, AGEGR, SEX, RACE, SITEID, TRT01P, RANDDT, TR01SDT, LAST2SDT)
    
    adsl_rec <- as.data.frame((adsl_rec)) # 'data' must be 2-dimensional (e.g. data frame or matrix)
    
    # Assuming we are only getting one record returned
    DT::datatable(adsl_rec, options = list(dom = 't'), rownames = FALSE, 
                  colnames = c('Planned Treatment Group' = 8),
                  caption = "Selected Demographic variables from ADSL")
    
  })
  
  # Now show the rest of the widgets
  shinyjs::show(id = "hr2")
  shinyjs::show(id = "hr3")
  shinyjs::show(id = "checkGroup")
  shinyjs::show(id = "eventsTable")
  shinyjs::show(id = "selType")
  
  # Clear datatable
  output$DataTable<- DT::renderDataTable({
    NULL
  })
  # Clear plotoutput
  output$PlotChart <- renderPlotly({
    NULL
  })
  
  checked1 <- NULL
  checked2 <- NULL
  checked3 <- NULL
  checked4 <- NULL
  
  # check for "adsl", "adae", "adcm", and "adlb"
  if ("ADSL" %in% dataselected()) {
    checked1 <- "DS"
  }
  if ("ADAE" %in% dataselected()) {
    checked2 <- "AE"
  }
  if ("ADCM" %in% dataselected())  {
    checked3 <- "CM"
  }
  if ("ADLB" %in% dataselected()) {
    checked4 <- "LB"
  }
  
  choices <- list(checked1,checked2,checked3,checked4)
  names <- c("Disposition","Adverse Events","Concomitant Meds","Labs")
  # build a named list
  choices <- setNames(choices,names)
  # Remove NULLs from the list
  choices <- choices[!sapply(choices,is.null)]
  
  # update the checkbox group
  updateCheckboxGroupInput(
    session = session,
    inputId = "checkGroup",
    choices = unlist(choices), # optionally convert list to array
    selected = NULL,
    inline = TRUE)
  
}) # observeEvent
return(reactive({ input$selPatNo }))

} # IndvExpl2SelPatno