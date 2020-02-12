PopuExpl5Hist <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Histogram
shinyjs::show(id="selPrmCode")
shinyjs::show(id="bygroup")
shinyjs::show(id="groupbyvar")
shinyjs::hide(id="selxvar")
shinyjs::hide(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::show(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::show(id="numBins")

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",chrcols), selected = " ")

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices = c(" ",numcols), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "bygroup", value = TRUE)

bins <- reactive({
  input$numBins
})

updateSelectInput(
  session = session,
  inputId = "selPrmCode",
  choices = c(" ",unique(df()$PARAMCD)),
  selected = " ")

observe({
  if(input$bygroup == TRUE) {
    shinyjs::show(id="groupbyvar")
  } else {
    shinyjs::hide(id="groupbyvar")
  }
})

observeEvent(input$selPrmCode, {
  
  # subset data based on Parameter Code selection
  req(input$selPrmCode != " ") # using ignoreInit = TRUE
  dfsubset <- filter(df(),PARAMCD == input$selPrmCode)
  
  # restrict seltimevar to AVISIT, AVISITN, VSDY
  seltime <- select(dfsubset, ends_with("DY"), starts_with("AVIS"))
  
  if (!input$selxvar %in% names(seltime)) {
    dfsubset <- dfsubset %>%
      filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      distinct(USUBJID, .keep_all = TRUE)
  }
  
output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  y_var <- as.name(input$responsevar)
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")
    x_var <- as.name(input$groupbyvar)
    
    ggtitle <- paste("Distribution of",input$responsevar,"Grouped by",input$groupbyvar,"for PARAMCD:",unique(dfsubset$PARAMCD))
    p <- ggplot(dfsubset,
                aes(x = !!y_var, fill = !!x_var)) +
      labs(x = input$responsevar, y = "Count", fill = input$groupbyvar )
    
  } else {
    
    ggtitle <- paste("Distribution of",input$responsevar,"for PARAMCD:",unique(dfsubset$PARAMCD))
    p <- ggplot(dfsubset,
                aes(x = !!y_var )) +
      labs(y = "Count" )
    
  }
  p <- p +
    geom_histogram(bins = bins(), position = "dodge", na.rm = TRUE) +
    scale_fill_discrete() +
    ggtitle(ggtitle) 
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")  
  }   
  
  tableout <- fnsummtab(dfsubset, input$bygroup, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}, ignoreInit = TRUE)

}