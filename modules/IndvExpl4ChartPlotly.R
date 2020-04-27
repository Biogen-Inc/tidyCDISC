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
    paste0("Plot Patient Metrics by Visit") #'", usubjid, "' 
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
      selected =  " "
    )
    
    # update array of params
    updateSelectInput (
      session = session,
      inputId = "plot_param",
      # choices = c(" "),
      selected = " "
    )
    
    # cat(paste("\n",input$overlay_events))
    # cat(paste("\n",length(input$overlay_events) > 0))
    
  })

  



  
# upon selecting a plottable adam data set from dropdown
# observeEvent(list(input$plot_adam), { # ,input$bds_remove_filter # add this back in if we want to enable total tab filtering
vv_dy_name <- eventReactive(list(input$plot_adam), {
  # make sure a subject has been selected
  req(usubjid() != " " & input$plot_adam != " ") # selPatNo cannot be blank
  
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
  
  # cat(paste('\n1: ',input$plot_adam))
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
   return(sel_vst_var)
  }) # eventReactive



  observe({
    req(input$plot_adam)
    
    # can't get rid of this note flashing!
    if(substr(input$visit_var,nchar(input$visit_var)-1,nchar(input$visit_var)) == "DY"){
      shinyjs::hide(id = "display_dy")
      output$display_dy <- renderText({NULL})
      shinyjs::show(id = "overlay_events")
    } else {
      shinyjs::hide(id = "overlay_events")
      output$display_dy <- renderText({
        paste0("<br/>You can overlay events when Vist Variable ends in 'DY': ", vv_dy_name())
      })
      shinyjs::show(id = "display_dy")
    }
    
    # display Event Vals if an 1 overlay_events is selected an visit_var == ends_with("DY")
    if(length(input$overlay_events) > 0 ){
      shinyjs::show(id = "overlay_event_vals")
    } else {
      shinyjs::hide(id = "overlay_event_vals")
    }
  })
  
  
  
  
  
  # update horizontal line choices
  observeEvent(list(input$plot_param), { # ,input$bds_remove_filter # add this back in if we want to enable total tab filtering
    req(usubjid() != " " & input$plot_adam != " " & input$plot_param != " ")
    
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

  # If any param or visit var are updated, run code below
  observeEvent(list(input$plot_param, input$visit_var), { # ,input$bds_remove_filter # add this back in if we want to enable total tab filtering
    
    # don't run until a patient and ADAM are selected
    req(usubjid() != " " & input$plot_adam != " ") # selPatNo cannot be blank
    
    
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
       
       # In the labs, label what the blue lines are in the legend or hover text.
       # make sure a LabCode has been selected
       req(input$plot_param != " ")
       plot_dat <- 
         lb_data %>%
         filter(!(is.na(!!INPUT_visit_var)) & PARAMCD == input$plot_param) # make sure AVISITN is not missing
       
       if("Screening" %in% input$plot_hor){
         plot_scr <- plot_dat %>% subset(toupper(VISIT) == "SCREENING") %>% distinct(AVAL) %>% mutate(Event = "Screening")
         # plot_dat <- plot_dat %>% subset(!(toupper(plot_dat$VISIT) == "SCREENING")) # removes screening point from plot
       }
       if("Baseline" %in% input$plot_hor){
         plot_base <- plot_dat %>% subset(toupper(AVISIT) == "BASELINE") %>% distinct(AVAL) %>% mutate(Event = "Baseline")
         # plot_dat <- plot_dat %>% subset(!(toupper(plot_dat$AVISIT) == "BASELINE")) # removes baseline point from plot
       }
       
       if (nrow(plot_dat) > 0) {

         prm   <- unique(plot_dat$PARAM)
         
         lb_plot <- ggplot(plot_dat, aes(x = !!INPUT_visit_var, y = AVAL)) + 
           geom_line() +
           geom_point(na.rm = TRUE, 
                      aes(text =
                            paste0(AVISIT,
                                   "<br>",input$visit_var, ": ",!!INPUT_visit_var,
                                   "<br>",input$plot_param ,": ",AVAL
                            )
                      )) +
           scale_x_continuous(breaks = seq(0, max(plot_dat[,input$visit_var]), 30)) +
           labs(x = paste0("Study Visit (",input$visit_var,")"),
                y = prm,
                title = paste(prm,"by Relative Study Day"),
                subtitle = paste(ifelse(input$plot_adam == "ADLB","test<br>",""),"USUBJID:",usubjid())
           )
         
         if(input$plot_adam == "ADLB"){
           lohi <- paste("LO:",unique(plot_dat$LBSTNRLO),"HI:",unique(plot_dat$LBSTNRHI))
           lb_plot <- lb_plot + 
             geom_hline(aes(yintercept = mean(LBSTNRLO)), colour = "blue") +
             geom_hline(aes(yintercept = mean(LBSTNRHI)), colour = "blue") +
             theme(
               plot.margin = margin(b = 1, unit = "cm") #t = 1, # used to put margin at top of graph for caption
             ) 
             
         }
         # lohi <- paste("LO:",unique(plot_dat$LBSTNRLO),"HI:",unique(plot_dat$LBSTNRHI))
         if("Screening" %in% input$plot_hor){
           lb_plot <- lb_plot +
             geom_hline(plot_scr, mapping = aes(yintercept = AVAL, colour = Event)) #, colour = "darkgreen"
         }
         if("Baseline" %in% input$plot_hor){
           lb_plot <- lb_plot +
             geom_hline(plot_base, mapping = aes(yintercept = AVAL, colour = Event)) #, colour = "purple")
         }
         
         ly <- ggplotly(lb_plot, tooltip = "text") %>%
           layout(title = list(text = 
             paste0(prm," by Study Visit<sup>",
                         "<br>USUBJID: ",usubjid()
             )))
            # Used to have lab param range of values in title
            # , ifelse(input$plot_adam == "ADLB",paste0("<br>Study's average range shown in ",'<em style="color:blue">',"blue",'</em> ',lohi),""),
            # "</sup>")))
         
         # instead, request was made to add caption to bottom of graph
         if(input$plot_adam == "ADLB"){
           ly <- ly %>%
             add_annotations(x = ggplot_build(lb_plot)$layout$panel_params[[1]]$x.range[1],
                             y = -.10, # 10% below graph
                             yref = "paper",
                             text = paste0("<br>Note: Study's average ",input$plot_param," range shown in ",'<em style="color:blue">',"blue",'</em> ',lohi),
                             xanchor = 'left',
                             showarrow = F)
         }
         ly
         
       } # if (nrow(plot_dat) > 0)
     }) # renderPlotly
       
       
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
       }
       
     }) #renderDataTable
  
  }) # observe
} # IndvExpl4ChartPlotly