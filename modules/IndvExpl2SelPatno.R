IndvExpl2SelPatno <- function(input, output, session, datafile){ #, dataselected
  
  ns <- session$ns

  # Hide widgets until the input file has been selected
  shinyjs::hide(id = "selPatNo")
  shinyjs::hide(id = "selType")
  shinyjs::hide(id = "selLabCode")
  shinyjs::hide(id = "checkGroup")
  shinyjs::hide(id = "eventsTable")
  shinyjs::hide(id = "hr2")
  shinyjs::hide(id = "hr3")
  
  
  
  loaded_adams<- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  
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
  
  
    
  
  
  
  
# observeEvent for inputselPatno 
observeEvent(input$selPatNo, {
  
  req(input$selPatNo != " ") # selPatNo cannot be blank
  
  shinyjs::hide(id = "selType") # ac: I don't think this code will ever be realized because it is tracking when selPatNo is changes, 
  shinyjs::hide(id = "selLabCode") # and then later, shows the selType Anyway. Same is not true for selLabCode. I think this should
                                   # be in a separate observer with a validate(need())
  
  # Clear eventsTable - ac: do we really want to do this?
  output$eventsTable <- DT::renderDataTable({
    NULL
  })
  

  
  output$demogInfo <- DT::renderDataTable({
    
    adsl_rec <- datafile()[["ADSL"]] %>%
      filter(USUBJID == input$selPatNo) %>%
      select(COUNTRYC, AGE, AGEGR, SEX, RACE, SITEID, TRT01P, RANDDT, TR01SDT, LAST2SDT) #74 Removed USUBJID
    
    adsl_rec <- as.data.frame((adsl_rec)) # 'data' must be 2-dimensional (e.g. data frame or matrix)
    
    # Assuming we are only getting one record returned
    DT::datatable(adsl_rec, options = list(dom = 't'), rownames = FALSE, 
                  colnames = c('Planned Treatment Group' = 8),
                  caption = tags$caption(style = "font-size:20px;color:black;", paste0(input$selPatNo, ": Demographic Info from ADSL" ))
                  )
    
  })
  
  # Show the rest of the widgets once a patient number was selected
  shinyjs::show(id = "hr2")
  shinyjs::show(id = "hr3")
  shinyjs::show(id = "checkGroup")
  shinyjs::show(id = "eventsTable")
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
  names <- c("Disposition","Adverse Events","Concomitant Meds","Labs") # ac: labels
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