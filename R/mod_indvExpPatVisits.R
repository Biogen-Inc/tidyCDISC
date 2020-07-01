#' indvExpPatVisits Server Function
#'
#' Prepare Individual Explorer Tab's Visits subtab with content. Specifically,
#' creating some headers, build events dataset, Output a DT & plotly Object.
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param datafile A list of dataframes
#' @param loaded_adams A character vector of loaded adam datasets
#' @param usubjid A Character string containing a USUBJID
#' @param filtered_dat A IDEAFilter output data frame containing USUBJID
#'
#' @import shiny
#' @import dplyr
#' @importFrom shinyjs show hide enable disable
#' @importFrom plotly renderPlotly
#' @importFrom stringr str_replace_all str_replace
#' @importFrom rmarkdown render
#'
#' @family indvExp Functions
#'   
mod_indvExpPatVisits_server <- function(input, output, session, datafile, loaded_adams, usubjid, filtered_dat){
  ns <- session$ns
  
  # We can only plot ADaM datasets that have the variables below for plotting.
  # Therefore, we verify which of the df's from datafile are plottable. Required
  # "PARAM & AVAL", req at least 1: "AVISIT", "AVISITN", "VISITDY"
  plotable_adams <- reactive({
    req(!is.null(datafile()))
    needed_cols_exists <- names(which(sapply(datafile(), FUN = function(x) all(c("PARAMCD","AVAL") %in% colnames(x)))) > 0)
    one_visit_exists <- names(which(sapply(datafile(), FUN = function(x) any(c("AVISIT","AVISITN","VISIT") %in% colnames(x)))) > 0)
    return(intersect(needed_cols_exists,one_visit_exists))
  }) 
  # do I also need to filter these datasets by usubjid? For example, if there is
  # no data for 1 subj in a certain data set, then it shouldn't be an available
  # option to select.
  
  # When the user asks for help, guide them through the UI
  observeEvent( input$help_visits, {
    if(length(plotable_adams()) == 0){
      guide_ind_exp_visits_blank$init()$start()
    } else {
      # if no adlb, then
      if(!("ADLB" %in% plotable_adams()) | !(input$visit_var %in% vv_dy_name())){
        guide_ind_exp_visits$init()$start()
      } else {
        if(length(input$overlay_events) == 0){
          guide_ind_exp_visits_adlb$init()$start()
        } else{
          guide_ind_exp_visits_adlb_olay$init()$start()
        }
      }
      

      # else, adlb but no overlay
      # 
      # else adlb & overlay
      # guide_ind_exp_visits_adlb_olay$init()$start()
      
    #   if(any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0)){
    #     guide_ind_exp_events_adv$init()$start()
    #   } else {
    #     guide_ind_exp_events$init()$start()
    #   }
    }
  })
  
  
  # Header that depends on a few items existing
  output$plot_header <- renderText({
    req(!is.null(datafile()))
    paste0("Patient Metrics by Visit")
  })
  
  
  # Need to refresh plottable ADaMs every time a new df added (uploaded) to datafile 
  observeEvent(list(loaded_adams()), { #
  
    updateSelectInput(
      session = session,
      inputId = "plot_adam",
      choices = plotable_adams()
    )
  })
  
  # upon selecting a plottable adam data set from dropdown
  vv_dy_name <- eventReactive(list(input$plot_adam), {
    req(usubjid() != "" & input$plot_adam != " ") # make sure a subject has been selected
    
    # Clear some outputs
    output$DataTable <- DT::renderDataTable({ NULL })
    output$PlotChart <- renderPlotly({ NULL })
    
    # Initial data set filtered to the usubjid
    lb_data <- 
      datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid()) %>%
      distinct()

    # Grab PARAMCD's from data
    lbcodes <- lb_data %>%
      filter(!is.na(AVAL) & AVAL != "") %>% # if AVAL exists
      distinct(PARAMCD) %>% 
      pull()
    
    # Update PARAMCD list if values exist & show more widgets
    pcd_sel_widgets <- c("plot_param", "visit_var", "plot_hor", "overlay_events", "overlay_event_vals")
    if ((length(lbcodes) == 0)) {
      
      # warn user and hide widgets
      shinyjs::alert(paste("No PARAMs exist for this ADaM data set & subject!"))  
      purrr::map(pcd_sel_widgets, ~ shinyjs::hide(.x))
      sel_vst_var <- ""
      
    } else { 
      # show widgets
      purrr::map(pcd_sel_widgets, ~ shinyjs::show(.x))
      
      # update params list
      updateSelectInput (
        session = session,
        inputId = "plot_param",
        choices = c(lbcodes)
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
    return(sel_vst_var) # return the variable(s) ending in dy. It can be more than 1.
  }) # eventReactive
  
  
  
  
  
  
  observe({
    req(input$plot_adam) # only run if plot_adam is chosen
    
    # If a dy variable is vhose and an ADLB is loaded, show overlay_events widget and hide ADLB & DY reminder.
    # Else, do the opposite, and create those outputs
    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY" & "ADLB" %in% loaded_adams()){
      shinyjs::hide(id = "display_dy")
      shinyjs::show(id = "overlay_events")
    } else {
      output$display_dy <- renderUI({
        HTML(paste0("<br/>Note: You can overlay events<br/>when an ADLB is loaded on data<br/>tab and Visit Variable displayed<br/>ends in 'DY' like ", paste(vv_dy_name(),collapse = ", ")))
      })
      shinyjs::hide(id = "overlay_events")
      shinyjs::show(id = "display_dy")
    }
    
    
    # display Event Vals if an 1 overlay_events is selected an visit_var == ends_with("DY")
    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY" & length(input$overlay_events) > 0){
      shinyjs::show(id = "event_type_filter")
      shinyjs::show(id = "overlay_event_vals")
      if(any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0) & !is.null(input$plot_adam)){
        updateRadioButtons(session, "event_type_filter",
                           choices = as.list(c("All", "Pre-Filters", "Manually Filter")),
                           selected = isolate(input$event_type_filter)
        )
      } else {
        updateRadioButtons(session, "event_type_filter",
                           choices = as.list(c("All", "Manually Filter")),
                           selected = isolate(input$event_type_filter)
        )
      }
    } else { # NO event selected or non - DY selected
      shinyjs::hide(id = "event_type_filter")
      shinyjs::hide(id = "overlay_event_vals")
    }
  })
  
  
  # Recreate some 'pre-vline' data to populate the dropdown overlay_events_vals 
  olay_events <- eventReactive(list(length(input$overlay_events) > 0,
                                    input$event_type_filter,
                                    input$overlay_event_vals) , {
    req(usubjid() != "" & input$plot_adam != " ") # don't run until a patient and ADAM are selected
    
    # create data to plot vlines using events dataset
    if(length(input$overlay_events) > 0 & input$visit_var %in% vv_dy_name()){ #& "ADLB" %in% loaded_adams() # overlay checkbox won't appear unless this is true
      v_events_apply_filter <- reactive({
        ifelse( input$event_type_filter == "Pre-Filters", TRUE, FALSE)
      })
      
      # See mod_indvExp_fct_buildEvents.R, but this function will make an events
      # dataframe based on what's been selected in input$overlay_events
      olay_events0 <-
        build_events(
          input_checkbox = input$overlay_events,
          input_apply_filter = v_events_apply_filter(),
          my_usubjid = usubjid(),
          my_loaded_adams = loaded_adams(),
          my_datafile = datafile(),
          my_filtered_dat = filtered_dat()
        )
      
      # If df is not null or empty, then let's
      if (!is.null(olay_events0) && nrow(olay_events0) > 0){
        
        # if AE exists, create a new column that only contains aedecod fore more
        # manual filtering
        olay_events <-
          olay_events0 %>%
          mutate(filter_code = ifelse(EVENTTYP == "Adverse Events", substr(DECODE, 1, regexpr("AESEV:",DECODE)-2), as.character(DECODE)))
        return(olay_events)
      }
    }
  })
  
  # If an overlay_events val or event_type_filter changes and the user wants to
  # manually filter through the event values, then we'll update that select
  # input
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
  
  # Output text string of what was filtered in IDEAFilter widget/ module
  # this will be displayed in the user's well panel for convenience
  output$v_applied_filters <- renderUI({
    req(
      usubjid() != ""
      & any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0)
      & input$event_type_filter == "Pre-Filters"
      & length(input$overlay_events) > 0
    )
    filters_in_english(filtered_dat())
  })
  
  # Output text string of what was filtered manually by user in wellPanel this
  # will be displayed below the graph and in any batch downloads. This manually creates
  # a new format to display these types of items, which is different (better?) than the
  # IDEAFilter method because we have slightly more access/ control over the inputs
  v_applied_filters_HTML_on_graph <- reactive({
    req(usubjid() != "" & input$plot_adam != " ")
    
    HTML(
      if(length(input$overlay_events) == 0 | 
         (length(input$overlay_events) > 0 & input$event_type_filter == "All")) {
        ""
      } else if(length(input$overlay_events) > 0 & input$event_type_filter == "Pre-Filters") {
        as.character(filters_in_english(filtered_dat(), filter_header = "Events Lines Filtered to Include:"))
        
      } else if (length(input$overlay_events) > 0 & input$event_type_filter == "Manually Filter") {
        
        paste0("<b>Event Lines Filtered to Include:</b><br/>&nbsp;&nbsp;&nbsp;&nbsp;"
               ,paste(
                 olay_events() %>%
                   filter(filter_code %in% input$overlay_event_vals) %>%
                   distinct(EVENTTYP, filter_code) %>%
                   subset(filter_code != '') %>%
                   group_by(EVENTTYP) %>%
                   summarize(p = paste(filter_code, collapse = ", ")) %>%
                   ungroup() %>%
                   mutate(f = paste(EVENTTYP, p, sep = ": ")) %>%
                   distinct%>%
                   pull(f)
                 , collapse = "<br/>&nbsp;&nbsp;&nbsp;&nbsp;"))
      }
    )
  })
  
  # Add HTML directly above to a ui object
  output$v_applied_filters_grphDisp <- renderUI({
    req(
      usubjid() != ""
      & length(input$overlay_events) > 0
      & nrow(olay_events()) > 0
    )
    v_applied_filters_HTML_on_graph()
  })
  
  
  # Update horizontal line choices
  observeEvent(list(input$plot_param), {
    req(usubjid() != "" & input$plot_adam != " " & input$plot_param != " ")
    
    INPUT_visit_var <- sym(input$visit_var)

    # Note this is slightly different from table data because it get's rid of NA
    # values for visit var
    plot_dat <- 
      datafile()[[input$plot_adam]] %>%
      filter(USUBJID == usubjid() & !(is.na(!!INPUT_visit_var)) & PARAMCD == input$plot_param) %>% # make sure AVISITN is not missing
      distinct()
    
    # update plot_horizontal variable to display
    scr <- plot_dat %>% select(one_of("VISIT"))%>% distinct()%>% pull()
    base <- plot_dat %>% select(one_of("AVISIT"))%>% distinct()%>% pull()
    hor_choices0 <- c(ifelse(any(regexpr("SCREENING", toupper(scr)) > 0),"Screening",NA),
                      ifelse(any(regexpr("BASELINE", toupper(base)) > 0),"Baseline",NA))
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
  

  # Create the vline data to populate the graph, if applicable
  vline_dat <- eventReactive(list(length(input$overlay_events) > 0,
                                  input$event_type_filter,
                                  input$overlay_event_vals,
                                  input$plot_adam) , {
                                    
      # create data to plot vlines using events dataset
      if(length(input$overlay_events) > 0 & input$visit_var %in% vv_dy_name()){ 
        
        INPUT_visit_var <- sym(input$visit_var)
        
        # calbrating new "DY" imputation for vlines based on ADLB since it
        # contains both dates and "DY" var LBDY
        day1 <-
          datafile()[["ADLB"]] %>%
          filter(USUBJID == usubjid() & LBDY == 1) %>%
          summarize(min_lbdt = min(LBDT)) %>% # lbdt does not vary for a patient's 1st lbdy, but use min just to grab val
          pull(min_lbdt)
        
        # if overlay events data frame exists and day1 exists, build vlines data frame for plotting
        if(!is.null(day1) & !is.null(olay_events())){
          
          vline_dat0 <-
            olay_events() %>%
            mutate(!!INPUT_visit_var := ifelse(START - day1 < 0, START - day1, START - day1 + 1) +
                     case_when(EVENTTYP == "Adverse Events" ~ .3, # add a small jitter so vlines don't overplot
                               EVENTTYP == "Concomitant Meds" ~ .7,
                               TRUE ~ 0)                
            ) %>%
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
  
  
  
  
  ##################
  # Visit Plotting!
  ##################
  # If any of the following inputs are updated, run code below
  observeEvent(list(usubjid(),
                    input$plot_param,
                    input$visit_var,
                    input$overlay_events,
                    input$overlay_event_vals,
                    input$event_type_filter), {
                      
    # don't run until a patient and ADAM are selected
    req(usubjid() != "" & input$plot_adam != " ") # selPatNo cannot be blank
    
    # prepare plotting data
    lb_data <- 
      datafile()[[input$plot_adam]] %>%
      filter(!is.na(AVAL) & AVAL != "") %>%
      filter(USUBJID == usubjid()) %>%
      distinct()
    
    INPUT_visit_var <- sym(input$visit_var)

    output$PlotChart <- renderPlotly({
      req(input$plot_param != " ")
      
      # See mod_indvExpPatVisits_fct_plot.R
      fnIndvExplVisits(
        watermark = FALSE,
        graph_output = "plotly",
        bds_data = lb_data,
        usubjid = usubjid(),
        input_plot_hor = input$plot_hor,
        input_visit_var = input$visit_var,
        input_plot_param = input$plot_param,
        input_plot_adam = input$plot_adam,
        input_overlay_events = input$overlay_events,
        vline_dat = vline_dat(),
        vv_dy_name = vv_dy_name()
      )
    })
    
    ################################
    #
    # For Batch Download wellPanel
    #
    ################################
    np <- length(unique(lb_data$PARAMCD))
    output$dwnld_params_header <- renderText({
      s <- ifelse(np > 1,
                  paste("Download Report with Plots for all",np,"Params")
                  ,"Download Report with Plot Above")
    })
    
    output$batchDownReport <- downloadHandler(
      filename = function() {
        paste(paste(input$plot_adam, "Params", usubjid(), sep = '_'), sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html'
        ))
      },
      
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), switch(input$format, 
                                                  HTML = "batchDownload_html.Rmd",
                                                  PDF = "batchDownload_pdf.Rmd"))
        file.copy(switch(input$format, 
                         HTML = "inst/app/www/batchDownload_html.Rmd",
                         PDF = "inst/app/www/batchDownload_pdf.Rmd"), tempReport, overwrite = TRUE)
        
        
        # Knit the document: passing in the `params` list is optional by default but will
        # make it more difficult to debug, or if in new envir = eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app). Also attached progress bar onto progress
        progress <- Progress$new(max = np + 3)
        progress$set(message = "Rendering Report...")
        on.exit(progress$close())
        rmarkdown::render(
          input = switch(input$format, 
                         HTML = "inst/app/www/batchDownload_html.Rmd",
                         PDF = "inst/app/www/batchDownload_pdf.Rmd"),
          output_file = file,
          params = list(
            bds_data_ = lb_data,
            report_summary = paste0("Data from ", input$plot_adam, " with ", np, " paramcds for patient ", usubjid(),"."),
            user_notes = input$user_batch_notes,
            html_filters = v_applied_filters_HTML_on_graph()
          )
        )
      }
    )
    
    
    # Create DT object with variables of interest, if they exist
    output$DataTable <- DT::renderDataTable(server = FALSE, { 
      # server = FALSE ALLOWS downloading all rows, and not just displayed rows
      
      # make sure a LabCode has been selected
      req(input$plot_param != " ")
      
      lb_tab <- lb_data %>%
        filter(PARAMCD == input$plot_param) %>%
        mutate(avisit_sort = ifelse(is.na(AVISITN), -9000000000, AVISITN)) %>% # if no AVISIN, order it first
        arrange_(ifelse(input$visit_var == "AVISITN", "avisit_sort", input$visit_var)) %>%
        select(ends_with("DY"), one_of(
          "VISITNUM",
          "AVISITN",
          "VISIT",
          "AVISIT",
          "ATM",
          "ATPT"
          ),
          PARAMCD,
          PARAM,
          AVAL 
        )
      
      if (nrow(lb_tab) > 0) {
        DT::datatable(lb_tab,
                      style="default",
                      extensions = "Buttons",
                      options = list(dom = 'Bftp', pageLength = 20,
                                     buttons = list(list(
                                       extend = "excel",
                                       filename = paste("Pat", usubjid(), "Param", input$plot_param, "dwnd",str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"), sep = "_")
                                     ))
                      ))
      }
    }) #renderDataTable
  }) # observe
}


## To be copied in the server -- done
# callModule(mod_indvExpPatVisits_server, "indvExpPatVisits_ui_1")
 
