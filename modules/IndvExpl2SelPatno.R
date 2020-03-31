IndvExpl2SelPatno <- function(input, output, session, datafile, loaded_adams){ #, dataselected
  
  ns <- session$ns

  output$demog_header <- renderText({
    req(!is.null(datafile()) & input$selPatNo != " ")
    paste0("Patient Demographic Info")
  })
  
  output$subjid_subtitle1 <- output$subjid_subtitle2 <- output$subjid_subtitle3 <- renderText({
    req(!is.null(datafile()) & input$selPatNo != " ")
    paste0("USUBJID: '",input$selPatNo,"'")
  })
  
  # observeEvent for inputselPatno 
  observeEvent(input$selPatNo, {
    
    req(input$selPatNo != " ") # selPatNo cannot be blank
    
    # Show the rest of the widgets once a patient number was selected
    shinyjs::show(id = "demog_header")
    shinyjs::show(id = "subjid_subtitle1")
    shinyjs::show(id = "demogInfo")
    shinyjs::show(id = "hr2")
    shinyjs::show(id = "events_header")
    shinyjs::show(id = "subjid_subtitle2")
    shinyjs::show(id = "checkGroup")
    # shinyjs::show(id = "eventsPlot")
    # shinyjs::show(id = "eventsTable")
    shinyjs::show(id = "hr3")
    shinyjs::show(id = "plot_header")
    shinyjs::show(id = "subjid_subtitle3")
    shinyjs::show(id = "plot_adam")
    
    # Clear datatables abd plots 
    output$DataTable<- DT::renderDataTable({NULL})
    output$PlotChart <- renderPlotly({NULL})
    output$eventsTable <- renderDataTable({NULL})
    output$eventsPlot <- renderTimevis({NULL})
    output$events_tv_caption1 <- renderText({NULL})
    output$events_tv_caption2 <- renderText({NULL})
    shinyjs::hide(id = "events_tv_caption1")
    shinyjs::hide(id = "events_tv_caption2")
    shinyjs::hide(id = "eventsPlot")
    shinyjs::hide(id = "eventsTable")
    
    
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
      DT::datatable(adsl_rec,
                    style="default",
                    class="compact",
                    options = list(bSort = FALSE,dom = 't'),
                    rownames = FALSE,
                    colnames = c('Planned Treatment Group' = ncol(adsl_rec))#,
                    # caption = tags$caption(style = "font-weight:bold;font-size:20px;color:black;", paste0("Patient Demographic Info\n USUBJID: '", input$selPatNo, "'" ))
                    )
    })
    
    
    # update checkboxes
    checked1 <- NULL
    checked2 <- NULL
    checked3 <- NULL
    checked4 <- NULL
    checked5 <- NULL
    
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
    if ("ADMH" %in% loaded_adams()) {
      checked5 <- "MH"
    }
    
    choices <- list(checked1,checked2,checked3,checked4,checked5)
    names <- c("Milestones","Adverse Events","Concomitant Meds","Labs","Medical History") # ac: labels
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

  # return selected patient USUBJID from module
  return(reactive({ input$selPatNo }))

} # IndvExpl2SelPatno