IndvExpl2SelPatno <- function(input, output, session, datafile, loaded_adams){ #, dataselected
  
  ns <- session$ns

  
  
    
  
  
  
  
# observeEvent for inputselPatno 
observeEvent(input$selPatNo, {
  
  req(input$selPatNo != " ") # selPatNo cannot be blank
  
  # shinyjs::hide(id = "selType") # ac: I don't think this code will ever be realized because it is tracking when selPatNo is changes, 
  # shinyjs::hide(id = "selLabCode") # and then later, shows the selType Anyway. Same is not true for selLabCode. I think this should
                                   # be in a separate observer with a validate(need())
  
  # Clear eventsTable - ac: do we really want to do this?
  output$eventsTable <- DT::renderDataTable({
    NULL
  })
  
  
  
  output$demogInfo <- DT::renderDataTable({
    
    # grab demographic variables of interest. Notice if COUNTRYC doesn't exist, we'll grab country. If neither exists, one_of() will throw a
    # warning, but it the select() will still execute
    adsl <- datafile()[["ADSL"]]
    adsl_rec <- datafile()[["ADSL"]] %>%
      filter(USUBJID == input$selPatNo) %>%
      select(one_of(ifelse("COUNTRYC" %in% colnames(adsl),"COUNTRYC","COUNTRY"))
             , AGE
             , one_of(ifelse("AGEGR" %in% colnames(adsl),"AGEGR","AGEGR1"))
             , SEX, RACE, SITEID, TRT01P) #79 removed dates due to redundancy, RANDDT, TR01SDT, LAST2SDT) #74 Removed USUBJID
    
    adsl_rec <- as.data.frame((adsl_rec)) # 'data' must be 2-dimensional (e.g. data frame or matrix)
    
    # Assuming we are only getting one record returned
    # col position below depends on if country exists in ADSL (it is not required to exist), but it is always the last column
    DT::datatable(adsl_rec, options = list(dom = 't'), rownames = FALSE,
                  colnames = c('Planned Treatment Group' = ncol(adsl_rec)),
                  caption = tags$caption(style = "font-weight:bold;font-size:20px;color:black;", paste0("Patient '", input$selPatNo, "' Demographic Info" ))
                  )
    
  })
  
  # Show the rest of the widgets once a patient number was selected
  shinyjs::show(id = "hr2")
  shinyjs::show(id = "events_header")
  shinyjs::show(id = "checkGroup")
  shinyjs::show(id = "eventsTable")
  shinyjs::show(id = "hr3")
  shinyjs::show(id = "plot_header")
  shinyjs::show(id = "selType")
  
  # Clear datatable - ac: do we need to do this?
  output$DataTable<- DT::renderDataTable({
    NULL
  })
  # Clear plotoutput- ac: do we need to do this?
  output$PlotChart <- renderPlotly({
    NULL
  })
  
  checked1 <- NULL
  checked2 <- NULL
  checked3 <- NULL
  checked4 <- NULL
  
  # Am I supposed to add more to this list?
  # check for "adsl" (required), "adae" (adds to Events), "adcm" (adds to Events & Value), and "adlb" (adds to Events & Value)
  if ("ADSL" %in% loaded_adams()) {
    checked1 <- "DS"
  }
  if ("ADAE" %in% loaded_adams()) {
    checked2 <- "AE"
  }
  if ("ADCM" %in% loaded_adams())  {
    checked3 <- "CM"
  }
  if ("ADLB" %in% loaded_adams()) {
    checked4 <- "LB"
  }
  
  choices <- list(checked1,checked2,checked3,checked4)
  names <- c("Milestones","Adverse Events","Concomitant Meds","Labs") # ac: labels
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