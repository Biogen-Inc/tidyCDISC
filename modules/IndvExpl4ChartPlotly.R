IndvExpl4ChartPlotly <- function(input, output, session, datafile, loaded_adams, usubjid, filtered_dat){ #, dataselected , occr_choices
  
  ns <- session$ns
  
  # We can only plot ADaM datasets that have the variables we need for plotting. Those include:
  #   req: "PARAM", 
  #   req at least 1: "AVISIT", "AVISITN", "VISITDY"
  #   req: "AVAL"
  plotable_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    needed_cols_exists <- names(which(sapply(datafile(), FUN = function(x) all(c("PARAMCD","AVAL") %in% colnames(x)))) > 0)
    one_visit_exists <- names(which(sapply(datafile(), FUN = function(x) any(c("AVISIT","AVISITN","VISIT") %in% colnames(x)))) > 0)
    return(intersect(needed_cols_exists,one_visit_exists))
  }) # do I also need to filter these datasets by subjid? For example, if there is no data for 1 subj in a certain data set, then
     # it shouldn't be an available option to select.
  
  output$plot_header <- renderText({
    req(!is.null(datafile()))
    paste0("Patient Metrics by Visit") #'", usubjid, "' 
  })
  
  # Need to refresh these every time a new subject is selected
  observeEvent(usubjid(), {
  
    # If a plotable adam is loaded, include it in the dropdown choices
    # & set default selection back to blank
    updateSelectInput(
      session = session,
      inputId = "plot_adam",
      choices = plotable_adams(), #  plot_adams ................. does this need to update/ depend on usubjid selected? Because RK was backing out datasets
                                  #                             that didn't have any PARAMs for a patient
      # selected =  " " # keep commented out if you want visit plot to update when ubsubjid changes
    )
    
    # update array of params
    updateSelectInput (
      session = session,
      inputId = "plot_param",
      # choices = c(" "),
      # selected = " " # keep commented out if you want visit plot to update when ubsubjid changes
    )
    
    # cat(paste("\n",input$overlay_events))
    # cat(paste("\n",length(input$overlay_events) > 0))
    
  })

  



  
  # upon selecting a plottable adam data set from dropdown
  # observeEvent(list(input$plot_adam), { # ,input$bds_remove_filter # add this back in if we want to enable total tab filtering
  vv_dy_name <- eventReactive(list(input$plot_adam), {
    # make sure a subject has been selected
    req(usubjid() != "" & input$plot_adam != " ") # selPatNo cannot be blank
    
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    })
    # Clear plotoutput
    output$PlotChart <- renderPlotly({
      NULL
    })
    
    bds_cols <- datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid()) %>%
      colnames()
    
    # cat(paste('\n0: ',input$plot_adam))
    # cat(paste('\n2:',paste(bds_cols, collapse = ", "),'\n'))
  
     lb_data <- 
       # (if(input$bds_remove_filter == F) filtered_dat() %>% subset(data_from == input$plot_adam) else datafile()[[input$plot_adam]]) %>%  #ac: "ADLB" #lb_rec
       datafile()[[input$plot_adam]] %>%
       filter(USUBJID == usubjid()) %>%
       select(all_of(bds_cols)) %>%
       distinct()
       
     
     lbcodes <- lb_data %>%
                filter(!is.na(AVAL) & AVAL != "") %>% # if all the numeric AVAL exists
                distinct(PARAMCD) %>% #ac: PARAM and PARAMCD are both req fields. PARAM cd would be better for a dropdown
                pull()
     
     # cat(paste("\nlbcodes:", lbcodes))
     
     if ((length(lbcodes) == 0)) {
       shinyjs::alert(paste("No PARAMs exist for this ADaM data set & subject!"))  
       
       shinyjs::hide(id = "plot_param")
       shinyjs::hide(id = "visit_var")
       shinyjs::hide(id = "plot_hor")
       shinyjs::hide(id = "overlay_events")
       shinyjs::hide(id = "overlay_event_vals")
       sel_vst_var <- ""
     } else { 
       
       shinyjs::show(id = "plot_param")
       shinyjs::show(id = "visit_var")
       shinyjs::show(id = "DataTable")
       shinyjs::show(id = "PlotChart")
       shinyjs::show(id = "overlay_events")
       
       
       # update params list
       updateSelectInput (
         session = session,
         inputId = "plot_param",
         choices = c(lbcodes), #" ",
         # selected = " "
       )
       
       # update visit variable to display by
       my_vst_vars <- lb_data %>% select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>% colnames()
       sel_vst_var <- lb_data %>% select(ends_with("DY")) %>% colnames()
       
       updateSelectInput (
         session = session,
         inputId = "visit_var",
         choices = my_vst_vars ,
         selected = ifelse(length(sel_vst_var) > 0, sel_vst_var, character(0))
       )
     }
     # cat(paste("\nsel_vst_var:", sel_vst_var))
     return(sel_vst_var)
  }) # eventReactive



  
  

  observe({
  # observeEvent(list(input$visit_var), {
    req(input$plot_adam)

    # cat(paste("\nloaded ADLB:","ADLB" %in% loaded_adams()))


    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY" & "ADLB" %in% loaded_adams()){
      shinyjs::hide(id = "display_dy")
      shinyjs::show(id = "overlay_events")

    } else {

      # olay_note <- ifelse("ADLB" %in% loaded_adams()
      #        ,paste0("when Visit Variable ends in 'DY'- ", vv_dy_name())
      #        ,"by uploading a ADLB")
      # cat(paste("\n",olay_note))
      output$display_dy <- renderUI({
        HTML(paste0("<br/>Note: You can overlay events<br/>when an ADLB is loaded on data<br/>tab and Visit Variable displayed<br/>ends in 'DY' like ", vv_dy_name()))
        # HTML(paste0("<br/><br/>Note: You can overlay events<br/>",olay_note))
      })
      shinyjs::hide(id = "overlay_events")
      shinyjs::show(id = "display_dy")
    }

    # display Event Vals if an 1 overlay_events is selected an visit_var == ends_with("DY")
    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY" & length(input$overlay_events) > 0){
      shinyjs::show(id = "event_type_filter")
      shinyjs::show(id = "overlay_event_vals")
      if(any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0) & !is.null(input$plot_adam)){
        updateRadioButtons(session, "event_type_filter", # inline = T,
                           choices = as.list(c("All", "Pre-Filters", "Manually Filter")),
                           selected = isolate(input$event_type_filter)
        )
      } else {
        updateRadioButtons(session, "event_type_filter", # inline = T,
                           choices = as.list(c("All", "Manually Filter")),
                           selected = isolate(input$event_type_filter)
        )
      }
      
    } else { # NO event selected or non - DY selected
      shinyjs::hide(id = "event_type_filter")
      shinyjs::hide(id = "overlay_event_vals")
    }
  })
  
  
  
  
  

  ###
  # Recreate the pre-vline data to populate the dropdown overlay_events_vals 
  #list(input$plot_param,input$visit_var,input$overlay_events,input$event_type_filter)
  olay_events <- eventReactive(list(length(input$overlay_events) > 0, input$event_type_filter, input$overlay_event_vals) , {
    
    # don't run until a patient and ADAM are selected
    req(usubjid() != "" & input$plot_adam != " ")
    
    
    # create data to plot vlines using events dataset
    if(length(input$overlay_events) > 0 & input$visit_var == vv_dy_name()){ #& "ADLB" %in% loaded_adams() # overlay checkbox won't appear unless this is true
      
      v_events_apply_filter <- reactive({
        ifelse( input$event_type_filter == "Pre-Filters", TRUE, FALSE)
      })
      
      # See build_events_df.R
      olay_events0 <-
        build_events(
          input_checkbox = input$overlay_events
          , input_apply_filter = v_events_apply_filter() # input$events_apply_filter
          , my_usubjid = usubjid(), my_loaded_adams = loaded_adams(), my_datafile = datafile(), my_filtered_dat = filtered_dat()
        )
      
      # If df is not null or empty, then let's
      if (!is.null(olay_events0) && nrow(olay_events0) > 0){
        
        # if AE exists, create a new column that only contains aedecod
        olay_events <-
          olay_events0 %>%
          mutate(filter_code = ifelse(EVENTTYP == "Adverse Events", substr(DECODE, 1, regexpr("AESEV:",DECODE)-2), as.character(DECODE)))
        return(olay_events)
      }
    }
  })
  
  
  observeEvent(list(input$overlay_events, input$event_type_filter), {
    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY" & length(input$overlay_events) > 0){
      
      # update displayed overlay_event_vals
      if(input$event_type_filter != "Manually Filter"){ #"All" or "Inherit Pre-Filters"
        updateSelectizeInput(session, "overlay_event_vals",
                             choices = c("All"),
                             selected = "All"
        )
        shinyjs::disable(id = "overlay_event_vals")
        
      } else { 
        # "Manually filter" is selected
        # cat(paste("\n1. choices:",as.character(olay_events()$filter_code)))
        # cat(paste("\n2. choices:",as.character(olay_events()$EVENTTYP)))
        # cat(paste("\n3. choices:",paste(split(setNames(as.character(olay_events()$filter_code),olay_events()$filter_code),
        #                                    as.character(olay_events()$EVENTTYP)), collapse = ", ")))
        shinyjs::enable(id = "overlay_event_vals")
        my_choices <- split(setNames(as.character(olay_events()$filter_code),olay_events()$filter_code),as.character(olay_events()$EVENTTYP))
        curr_event_vals <- isolate(input$overlay_event_vals)
        keep_vals <- ifelse(curr_event_vals == "All","",curr_event_vals[curr_event_vals %in% unlist(my_choices)])
        
        updateSelectizeInput(session, "overlay_event_vals",
                             choices = my_choices,
                             selected = keep_vals
        )
      }
    }
  })



output$v_applied_filters <- renderUI({
  req(
    usubjid() != ""
    # & !is.null(filtered_dat())
    & any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0)
    & input$event_type_filter == "Pre-Filters"
  )

  filters_in_english(filtered_dat())

})
  
  
  ###
  # update horizontal line choices
  observeEvent(list(input$plot_param), { # ,input$bds_remove_filter # add this back in if we want to enable total tab filtering
    req(usubjid() != "" & input$plot_adam != " " & input$plot_param != " ")
    
    INPUT_visit_var <- sym(input$visit_var)
    
    bds_cols <- datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid()) %>%
      colnames()
    
    # ac: changed from above. Note this is slightly different from table data because it get's rid of NA values for visit var
    plot_dat <- 
      # (if(input$bds_remove_filter == F) filtered_dat() %>% subset(data_from == input$plot_adam) else datafile()[[input$plot_adam]]) %>%  #ac: "ADLB" #lb_rec
      datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid() & !(is.na(!!INPUT_visit_var)) & PARAMCD == input$plot_param) %>% # make sure AVISITN is not missing
      select(all_of(bds_cols)) %>%
      distinct()
    
    # update plot_horizontal variable to display
    scr <- plot_dat %>% select(one_of("VISIT"))%>% distinct()%>% pull()
    base <- plot_dat %>% select(one_of("AVISIT"))%>% distinct()%>% pull()
    hor_choices0 <- c(ifelse("SCREENING" %in% toupper(scr),"Screening",NA), ifelse("BASELINE" %in% toupper(base),"Baseline",NA))
    hor_choices <- hor_choices0[which(!is.na(hor_choices0))]
    
    if(length(hor_choices) > 0){
      shinyjs::show(id = "plot_hor")
      updateCheckboxGroupInput (
        session = session,
        inputId = "plot_hor",
        choices = hor_choices
      )
    }
    else{shinyjs::hide(id = "plot_hor")}
  })
  
  
  
  ###
  # Create the vline data to populate the graph, if applicable
  vline_dat <- eventReactive(list(length(input$overlay_events) > 0, input$event_type_filter, input$overlay_event_vals) , {
    
    # create data to plot vlines using events dataset
    if(length(input$overlay_events) > 0 & input$visit_var == vv_dy_name()){ #& "ADLB" %in% loaded_adams() # overlay checkbox won't appear unless this is true
      
       INPUT_visit_var <- sym(input$visit_var)
       
       day1 <-
         datafile()[["ADLB"]] %>%
         filter(USUBJID == usubjid() & LBDY == 1) %>%
         summarize(min_lbdt = min(LBDT)) %>% # lbdt does not vary for a patient's 1st lbdy, but use min just to grab val
         pull(min_lbdt)
       
       if(!is.null(day1) & !is.null(olay_events())){
         
         vline_dat0 <-
           olay_events() %>%
           mutate(!!INPUT_visit_var := ifelse(START - day1 < 0, START - day1, START - day1 + 1)) %>%
           rename("Event" = "EVENTTYP")
         
         if(input$event_type_filter == "Manually Filter"){
           vline_dat <-
             vline_dat0 %>%
             filter(filter_code %in% input$overlay_event_vals)
         } else {
           vline_dat <- vline_dat0
         }
         return(vline_dat)
       }
    }
  })
    
  ############ 
  # Plotting!
  ############
  # If any *param* or *visit var* are updated, run code below
  observeEvent(list(usubjid(),
                    input$plot_param,
                    input$visit_var,
                    input$overlay_events,
                    input$overlay_event_vals,
                    input$event_type_filter), {
    
    # don't run until a patient and ADAM are selected
    req(usubjid() != "" & input$plot_adam != " ") # selPatNo cannot be blank
    
    
    # create data
    bds_cols <- datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid()) %>%
      colnames()
    
    
    lb_data <- 
      # (if(input$bds_remove_filter == F) filtered_dat() %>% subset(data_from == input$plot_adam) else datafile()[[input$plot_adam]]) %>%  #ac: "ADLB" #lb_rec
      datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid()) %>%
      select(all_of(bds_cols)) %>%
      distinct()
    
    INPUT_visit_var <- sym(input$visit_var)
    
    
    
    output$PlotChart <- renderPlotly({
      req(input$plot_param != " ")
      
      fnIndvExplVisits(
        data = lb_data,
        usubjid = usubjid(),
        input_plot_hor = input$plot_hor,
        input_visit_var = input$visit_var,
        input_plot_param = input$plot_param,
        input_plot_adam = input$plot_adam,
        input_overlay_events = input$overlay_events,
        vline_dat = vline_dat(),
        vv_dy_name = vv_dy_name()
      )$plotly
       
     })
       
    
    
    
       
     output$DataTable <- DT::renderDataTable(server = FALSE, { # ALLOWS downloading all rows, and not just displayed rows
       
       # make sure a LabCode has been selected
       req(input$plot_param != " ")
       
       lb_tab <- lb_data %>%
         filter(PARAMCD == input$plot_param) %>%
         mutate(avisit_sort = ifelse(is.na(AVISITN), -9000000000, AVISITN)) %>%
         arrange_(ifelse(input$visit_var == "AVISITN", "avisit_sort", input$visit_var)) %>% #ac: LBDY
         select(ends_with("DY"), one_of(
               "VISITNUM",
               "AVISITN",
               "VISIT",
               "AVISIT"),
                PARAMCD,
                PARAM,
                AVAL 
         )
       
       if (nrow(lb_tab) > 0) {
         DT::datatable(lb_tab,
                       style="default",
                       extensions = "Buttons",
                       # class="compact",
                       options = list(dom = 'Bftp', pageLength = 20,
                                      buttons = list(list(
                                        extend = "excel",
                                        filename = paste("Pat", usubjid(), "Param", input$plot_param, "dwnd",str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"), sep = "_")
                                      ))
                      ))
         # DT::datatable(vline_dat())
       }
       
     }) #renderDataTable
  
  }) # observe
} # IndvExpl4ChartPlotly