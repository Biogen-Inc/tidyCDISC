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
      inputId = "selType",
      choices = plotable_adams(), #  seltypes ................. does this need to update/ depend on usubjid selected? Because RK was backing out datasets
                                  #                             that didn't have any PARAMs for a patient
      selected =  " "
    )
    
    # update array of params
    updateSelectInput (
      session = session,
      inputId = "selLabCode",
      # choices = c(" "),
      selected = " "
    )
    
  })
  
  
observeEvent(input$selType, {
  
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
         

   lb_data <- datafile()[[input$selType]] %>%  #ac: "ADLB" #lb_rec
     filter(USUBJID == usubjid()) # ac: input$selPatNo
   
   lbcodes <- lb_data %>%
              filter(!is.na(AVAL) & AVAL != "") %>% # if all the numeric AVAL exists
              distinct(PARAMCD) %>% #ac: PARAM and PARAMCD are both req fields. PARAM cd would be better for a dropdown
              pull()
   
   if ((length(lbcodes) == 0)) {
     shinyjs::alert(paste("No PARAMs exist for this ADaM data set & subject!"))  
     
     shinyjs::hide(id = "selLabCode")
     
     # # ac: Let's not drop it yet
     # seltypes <- plotable_adams()[!plotable_adams() == input$selType] # drops labs from the list
     # updateSelectInput(
     #   session = session,
     #   inputId = "selType",
     #   choices = seltypes,
     #   selected = " "
     # )
     
   } else { 
     
     shinyjs::show(id = "selLabCode")
     shinyjs::show(id = "DataTable")
     shinyjs::show(id = "PlotChart")
     
     # update array of lab codes
     updateSelectInput (
       session = session,
       inputId = "selLabCode",
       choices = c(" ",lbcodes),
       selected = " "
     )
   }
  }) # observe      
       

  observeEvent(input$selLabCode, {
    
    req(usubjid() != " " & input$selType != " ") # selPatNo cannot be blank
    
    lb_data <- datafile()[[input$selType]] %>%  #ac: "ADLB" #lb_rec
      filter(USUBJID == usubjid()) # ac: input$selPatNo
    
     output$PlotChart <- renderPlotly({
       
       # In the labs, label what the blue lines are in the legend or hover text.
       # make sure a LabCode has been selected
       req(input$selLabCode != " ")
       
       # ac: changed from above. Note this is slightly different from table data
       lb_tab <- lb_data %>%
         filter(!(is.na(VISITNUM)) & PARAMCD == input$selLabCode) # make sure VISITNUM is not missing

       
       if (nrow(lb_tab) > 0) {
         
         prmcd <- unique(lb_tab$PARAMCD)
         prm   <- unique(lb_tab$PARAM)
         
         lb_plot <- ggplot(lb_tab, aes(x = VISITNUM, y = AVAL)) + 
           geom_line() +
           geom_point(na.rm = TRUE ) +
           scale_x_continuous(breaks = seq(0, max(lb_tab$VISITNUM), 30)) +
           labs(x = paste(lb_tab$VISITNUM,"for USUBJID:",unique(lb_tab$USUBJID)),
                y = lb_tab$AVAL,
                title = paste(prmcd,":",prm,"by Relative Study Day"),
                subtitle = paste("USUBJID:",unique(lb_tab$USUBJID))
                  )
         
         ggplotly(
           lb_plot
           , tooltip = "text") #%>%
           # layout(title = list(text = paste(prmcd,":",prm,"by Study Visit"))) 
         
       } # if (nrow(lb_tab) > 0)
     }) # renderPlotly
       
       
     output$DataTable <- DT::renderDataTable({
       
       # make sure a LabCode has been selected
       req(input$selLabCode != " ")
       
       lb_tab <- lb_data %>%
         filter(PARAMCD == input$selLabCode) %>%
         arrange(VISITNUM) %>% #ac: LBDY
         select(one_of(
               "VISITNUM",
               "VISIT",
               "VISITDY", #ac: ADDED
               "AVISIT", "AVISITN"), #ac: ADDED 
                PARAMCD,     #ac: ADDED
                PARAM,   #ac: ADDED
                AVAL       #ac: ADDED
         )
       
       if (nrow(lb_tab) > 0) {
         DT::datatable(lb_tab, options = list(dom = 'ftp', pageLength = 20))
       }
     }) #renderDataTable
  
  }) # observe
} # IndvExpl4ChartPlotly