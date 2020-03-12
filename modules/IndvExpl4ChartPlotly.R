IndvExpl4ChartPlotly <- function(input, output, session, datafile, loaded_adams, usubjid){ #, dataselected
  
  ns <- session$ns
  
  
  # We can only plot ADaM datasets that have the variables we need for plotting. Those include:
  #   req: "PARAM", 
  #   req at least 1: "AVISIT", "AVISITN", "VISITDY"
  #   req: "AVAL"
  plotable_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    needed_cols_exists <- names(which(sapply(datafile(), FUN = function(x) all(c("PARAM","AVAL") %in% colnames(x)))) > 0)
    one_visit_exists <- names(which(sapply(datafile(), FUN = function(x) any(c("AVISIT","AVISITN","VISITDY") %in% colnames(x)))) > 0)
    return(intersect(needed_cols_exists,one_visit_exists))
  })
  
  
  output$plot_header <- renderText({
    req(!is.null(datafile()))
    paste0("Plot Patient '", usubjid(), "' Metrics by Visit")
  })
  
  seltypes = c(" ")
  # if ADCM or ADLB were selected, add to the seltypes selectInput list
  
  # ac: maybe we want to search the loaded_adams for data sets that have the columns we
  # need for graphing? USUBJID, PARAM, AVISIT, AVISITN, AVAL... maybe VISITDY
  observe({
    if ("ADCM" %in% loaded_adams()) {
      seltypes <- c(seltypes,"MEDS")
    }
    if ("ADLB" %in% loaded_adams()) {
      seltypes <- c(seltypes,"LABS")
    }

  # set default selection back to blank
  updateSelectInput(
    session = session,
    inputId = "selType",
    choices = seltypes,
    selected =  " "
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
  
  switch(input$selType,
         # use swtich() instead of if/else
         
         "MEDS" = {
           shinyjs::hide(id = "selLabCode")
           shinyjs::show(id = "DataTable")
           shinyjs::show(id = "PlotChart")
           
           if ("ADCM" %in% loaded_adams() ) {
             
             cm_tab <- as.data.frame(datafile()[["ADCM"]]) %>%
               filter(USUBJID == input$selPatNo) %>%
               filter(CMDECOD != "") %>%
               mutate(CMDECOD = ifelse(CMDECOD == "UNCODED", CMTRT, CMDECOD)) %>%
               mutate(CMSTDY = ifelse(CMSTDY < 0, 0, CMSTDY)) %>%
               arrange(CMSTDY, CMDECOD) %>%
               select(CMSTDY, CMSTDTC, CMDECOD, CMINDC, CMDOSFRQ)
             
             if ((nrow(cm_tab) == 0)) {
               shinyjs::alert(paste("No Concomitant Meds available for this subject!"))  
               
               seltypes <- seltypes[!seltypes == "MEDS"] # drops MEDS from the list
               updateSelectInput(
                 session = session,
                 inputId = "selType",
                 choices = seltypes,
                 selected = " "
               )
               
             } else ({
               output$DataTable <- DT::renderDataTable({
                 DT::datatable(cm_tab, options = list(dom = 'ftp', pageLength = 20))
               })
               
               output$PlotChart <- renderPlotly({
                 
                 cm_tab <- datafile()[["ADCM"]] %>% 
                   filter(USUBJID == input$selPatNo) %>%
                   filter(CMDECOD != "") %>%
                   mutate(CMSTDY = ifelse(CMSTDY < 0, 0, CMSTDY))
                 
                 cm_plot <-
                   ggplot(cm_tab,
                          aes(
                            x = CMSTDY,
                            fill = as.factor(CMCAT),
                            group = CMCAT
                          )) +
                   geom_histogram(position = "stack",
                                  alpha = 0.5,
                                  binwidth = 0.5) +
                   ggtitle(paste("Medications by",cm_tab$CMSTDY,"grouped by",cm_tab$CMCAT)) +
                   xlab(cm_tab$CMSTDY) +
                   ylab("Count") +
                   guides(fill = guide_legend(title = "Medication Class")) +
                   theme_classic()
                 
                 ggplotly(cm_plot)
                 
               }) # renderPlotly
             }) # else
           } # if ("ADCM" %in% loaded_adams() && ("ADCM" %in% datafile()))
           
         },
         
         
         
         
         
         "LABS" = {
           shinyjs::show(id = "selLabCode")
           shinyjs::show(id = "DataTable")
           shinyjs::show(id = "PlotChart")
           
           if ("ADLB" %in% loaded_adams() ) {
             
             lb_rec <- datafile()[["ADLB"]] %>% 
               filter(USUBJID == input$selPatNo)
             
             lbcodes <- unique(lb_rec$PARAMCD)
             
             if ((length(lbcodes) == 0)) {
               shinyjs::alert(paste("No lab codes available for this subject!"))  
               
               shinyjs::hide(id = "selLabCode")
               
               seltypes <- seltypes[!seltypes == "LABS"] # drops labs from the list
               updateSelectInput(
                 session = session,
                 inputId = "selType",
                 choices = seltypes,
                 selected = " "
               )
               
             } else ({ 
               # update array of lab codes
               updateSelectInput (
                 session = session,
                 inputId = "selLabCode",
                 choices = c(" ",lbcodes),
                 selected = " "
               )                 
               
               output$DataTable <- DT::renderDataTable({
                 
                 lb_data <- datafile()[["ADLB"]] %>%
                   filter(USUBJID == input$selPatNo) 
                 
                 # make sure a LabCode has been selected
                 req(input$selLabCode != " ")
                 
                 lb_tab <- lb_data %>%
                   filter(PARAMCD == input$selLabCode) %>%
                   arrange(LBDY) %>%
                   select(VISITNUM,
                          VISIT,
                          LBDT,
                          LBDY,
                          LBSTNRLO,
                          LBSTRESN,
                          LBSTNRHI,
                          LBNRIND)
                 
                 if (nrow(lb_tab) > 0) {
                   DT::datatable(lb_tab, options = list(dom = 'ftp', pageLength = 20))
                 }
               })
               
               output$PlotChart <- renderPlotly({
                 
                 lb_data <- datafile()[["ADLB"]] %>%
                   filter(USUBJID == input$selPatNo) %>%
                   filter(!(is.na(VISITNUM))) # make sure VISITNUM is not missing
                 
                 # In the labs, label what the blue lines are in the legend or hover text.
                 # make sure a LabCode has been selected
                 req(input$selLabCode != " ")
                 
                 lb_tab <- lb_data %>%
                   filter(PARAMCD == input$selLabCode) 
                 
                 if (nrow(lb_tab) > 0) {
                   
                   prmcd <- unique(lb_tab$PARAMCD)
                   prm   <- unique(lb_tab$PARAM)
                   
                   lohi = paste("LO:",unique(lb_tab$LBSTNRLO),"HI:",unique(lb_tab$LBSTNRHI))
                   
                   lb_plot <- ggplot(lb_tab, aes(x = LBDY, y = LBSTRESN)) +
                     geom_line() +
                     geom_hline(aes(yintercept = mean(LBSTNRLO)), colour = "blue") +
                     geom_hline(aes(yintercept = mean(LBSTNRHI)), colour = "blue") +
                     geom_point(na.rm = TRUE,
                                (aes(text = paste("LBSTNRHI:",LBSTNRHI,"<br>LBSTRESN:",LBSTRESN,"<br>LBSTNRLO:",LBSTNRLO)))) +
                     scale_x_continuous(breaks = seq(0, max(lb_tab$LBDY), 30)) +
                     labs(x = paste(lb_tab$LBDY,"for USUBJID:",unique(lb_tab$USUBJID)), y = lb_tab$LBSTRESN,
                          title = paste(prmcd,":",prm,"by Relative Study Day"))
                   
                   ggplotly(lb_plot, tooltip = "text") %>%
                     layout(title = list(text = paste(prmcd,":",prm,"by Relative Study Day",
                                                      '<br>',
                                                      '<sup>',
                                                      "Normal Range values shown in",'<em style="color:blue">',"blue",'</em>',lohi,
                                                      '</sup>'))) 
                   
                 } # if (nrow(lb_tab) > 0)
                 
               }) # renderPlotly
             }) # else
           } #if ("ADLB" %in% loaded_adams() )
           
         },
         "select" = {
           shinyjs::hide(id = "selLabCode")
           shinyjs::hide(id = "DataTable")
           shinyjs::hide(id = "PlotChart")
         })
  
}) # observe
} # IndvExpl4ChartPlotly