IndvExpl4ChartPlotly <- function(input, output, session, datafile, loaded_adams, usubjid){ #, dataselected
  
  ns <- session$ns
  
  # We can only plot ADaM datasets that have the variables we need for plotting. Those include:
  #   req: "PARAM", 
  #   req at least 1: "AVISIT", "AVISITN", "VISITDY"
  #   req: "AVAL"
  plotable_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    needed_cols_exists <- names(which(sapply(datafile(), FUN = function(x) all(c("PARAMCD","AVAL") %in% colnames(x)))) > 0)
    one_visit_exists <- names(which(sapply(datafile(), FUN = function(x) any(c("AVISIT","AVISITN","VISITDY","VISITNUM","VISIT") %in% colnames(x)))) > 0)
    return(intersect(needed_cols_exists,one_visit_exists))
  })
  
  
  output$plot_header <- renderText({
    req(!is.null(datafile()))
    paste0("Plot Patient '", usubjid(), "' Metrics by Visit")
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
    
  })
  
  
observeEvent(input$plot_adam, {
  
  # make sure a subject has been selected
  req(usubjid() != " ") # selPatNo cannot be blank
  
  # Clear datatable
  output$DataTable <- DT::renderDataTable({
    NULL
  })
  # Clear plotoutput
  output$PlotChart <- renderPlotly({
    NULL
  })
         

   lb_data <- datafile()[[input$plot_adam]] %>%  #ac: "ADLB" #lb_rec
     filter(USUBJID == usubjid()) # ac: input$selPatNo
   
   lbcodes <- lb_data %>%
              filter(!is.na(AVAL) & AVAL != "") %>% # if all the numeric AVAL exists
              distinct(PARAMCD) %>% #ac: PARAM and PARAMCD are both req fields. PARAM cd would be better for a dropdown
              pull()
   
   if ((length(lbcodes) == 0)) {
     shinyjs::alert(paste("No PARAMs exist for this ADaM data set & subject!"))  
     
     shinyjs::hide(id = "plot_param")
     shinyjs::hide(id = "visit_var")
     
     # # ac: Let's not drop it yet
     # plot_adams <- plotable_adams()[!plotable_adams() == input$plot_adam] # drops labs from the list
     # updateSelectInput(
     #   session = session,
     #   inputId = "plot_adam",
     #   choices = plot_adams,
     #   selected = " "
     # )
     
   } else { 
     
     shinyjs::show(id = "plot_param")
     shinyjs::show(id = "visit_var")
     shinyjs::show(id = "DataTable")
     shinyjs::show(id = "PlotChart")
     
     # update params list
     updateSelectInput (
       session = session,
       inputId = "plot_param",
       choices = c(" ",lbcodes),
       selected = " "
     )
     
     # update visit variable to display by
     possible_vst_vars <- c("AVISITN","VISITNUM")
     my_vst_vars <- possible_vst_vars[which(possible_vst_vars %in% colnames(lb_data))]
     
     updateSelectInput (
       session = session,
       inputId = "visit_var",
       choices = my_vst_vars #,
       # selected = "AVISITN"
     )
     
   }
  }) # observe      
       
  # If either param or visit var are updated, run code below
  observeEvent(list(input$plot_param, input$visit_var), {
    
    # don't run until a patient and ADAM are selected
    req(usubjid() != " " & input$plot_adam != " ") # selPatNo cannot be blank
    
    # create data
    lb_data <- datafile()[[input$plot_adam]] %>%  #ac: "ADLB" #lb_rec
      filter(USUBJID == usubjid()) # ac: input$selPatNo
    
    
    INPUT_visit_var <- sym(input$visit_var)
    
    
     output$PlotChart <- renderPlotly({
       
       # In the labs, label what the blue lines are in the legend or hover text.
       # make sure a LabCode has been selected
       req(input$plot_param != " ")

       
       # ac: changed from above. Note this is slightly different from table data
       plot_dat <- lb_data %>%
         filter(!(is.na(!!INPUT_visit_var)) & PARAMCD == input$plot_param) # make sure AVISITN is not missing
       
       if (nrow(plot_dat) > 0) {
         
         # prmcd <- unique(plot_dat$PARAMCD)
         prm   <- unique(plot_dat$PARAM)
         
         lb_plot <- ggplot(plot_dat, aes(x = !!INPUT_visit_var, y = AVAL)) + 
           geom_line() +
           geom_point(na.rm = TRUE ) +
           scale_x_continuous(breaks = seq(0, max(plot_dat[,input$visit_var]), 30)) +
           labs(x = paste0("Study Visit (",input$visit_var,")"),
                y = prm,
                title = paste(prm,"by Relative Study Day"),
                subtitle = paste("USUBJID:",usubjid())
           )
         
         ggplotly(lb_plot, tooltip = "text") %>%
           layout(title = list(text = paste0(prm," by Study Visit<br><sup>USUBJID: ",usubjid(),"</sup>")))
         
       } # if (nrow(plot_dat) > 0)
     }) # renderPlotly
       
       
     output$DataTable <- DT::renderDataTable({
       
       # make sure a LabCode has been selected
       req(input$plot_param != " ")
       
       lb_tab <- lb_data %>%
         filter(PARAMCD == input$plot_param) %>%
         mutate(avisit_sort = ifelse(is.na(AVISITN), -1000, AVISITN)) %>%
         arrange_(ifelse(input$visit_var == "AVISITN", "avisit_sort", input$visit_var)) %>% #ac: LBDY
         select(one_of(
               "VISITNUM",
               "VISIT",
               "VISITDY", #ac: ADDED
               "AVISIT", "AVISITN"), #ac: ADDED 
                # avisit_sort, # temporary VIEWING purposes only
                PARAMCD,     #ac: ADDED
                PARAM,   #ac: ADDED
                AVAL       #ac: ADDED
         )
       
       if (nrow(lb_tab) > 0) {
         DT::datatable(lb_tab, options = list(dom = 'ftp', pageLength = 20))
       }
       
       # DT::datatable(plot_dat, options = list(dom = 'ftp', pageLength = 20))
     }) #renderDataTable
  
  }) # observe
} # IndvExpl4ChartPlotly