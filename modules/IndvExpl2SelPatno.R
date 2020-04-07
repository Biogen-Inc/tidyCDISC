IndvExpl2SelPatno <- function(input, output, session, datafile, loaded_adams, filtered_dat){ #, dataselected
  
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
    # if any filter is selected in IDEAFilter, then we should show the "events_apply_filters" checkbox,
    # which defaults to TRUE everytime a new patient is selected
    if(any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0)){
      updateCheckboxInput(session = session, inputId = "events_remove_filter", value = F)
      shinyjs::show(id = "events_remove_filter")
      updateCheckboxInput(session = session, inputId = "bds_remove_filter", value = F)
      shinyjs::show(id = "bds_remove_filter")
    } else {
      updateCheckboxInput(session = session, inputId = "events_remove_filter", value = T)
      shinyjs::hide(id = "events_remove_filter")
      updateCheckboxInput(session = session, inputId = "bds_remove_filter", value = T)
      shinyjs::hide(id = "bds_remove_filter")
    }
    shinyjs::show(id = "checkGroup")  
    # shinyjs::show(id = "eventsPlot")
    # shinyjs::show(id = "eventsTable")
    shinyjs::show(id = "hr3")
    shinyjs::show(id = "plot_header")
    shinyjs::show(id = "subjid_subtitle3")
    # see if-then-else up near subjid_subtitle2 ^^^ for bds_remove_filter checkbox toggling
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
    checked1 <- NA
    checked2 <- NA
    checked3 <- NA
    checked4 <- NA
    checked5 <- NA
    mh_names <- NA
    
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
      # For ADMH, we want to create separate checkboxes for each type of 
      # Medical History Category that exist in the ADMH for the selected patient.
      mh_names <-
        datafile()[["ADMH"]] %>%
        filter(USUBJID == input$selPatNo) %>%
        distinct(MHCAT) %>%
        pull()%>%
        str_to_title()
      checked5 <- paste0("MH_",sapply(strsplit(mh_names, " "), function(x){
        toupper(paste(substring(x, 1, 1), collapse = ""))}))
    }
    
    choices <- as.list(unlist(c(list(checked1,checked2,checked3,checked4,as.list(checked5)))))
    names <- c("Milestones","Adverse Events","Concomitant Meds","Labs",mh_names) # ac: labels
    
    # build a named list
    choices <- setNames(choices,names)
    # Remove NULLs from the list
    choices <- choices[!sapply(choices,is.na)]
    
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