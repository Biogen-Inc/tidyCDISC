PopuExpl5Hist <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Histogram
shinyjs::hide(id="advstagsem")
shinyjs::show(id="adsltagsem")
shinyjs::hide(id="selPrmCode")
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


output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  y_var <- as.name(input$responsevar)
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")
    x_var <- as.name(input$groupbyvar)
    
    ggtitle <- paste("Distribution of",input$responsevar,"Grouped by",input$groupbyvar)
    
    p <- ggplot(df(),
                aes(x = !!y_var, fill = !!x_var)) +
      geom_histogram(bins = bins(), position = "dodge", na.rm = TRUE) +
      scale_fill_discrete() +
      ggtitle(ggtitle) +
      labs(x = input$responsevar, y = "Count", fill = input$groupbyvar )
    
  } else {
    
    ggtitle <- paste("Distribution of",input$responsevar)
    p <- ggplot(df(),
                aes(x = !!y_var )) +
      geom_histogram(bins = bins(), position = "dodge", na.rm = TRUE) +
      scale_fill_discrete() +
      ggtitle(ggtitle) +
      labs(y = "Count" )
    
  }
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")  
  }   
  
  tableout <- fnsummtab(df(), input$bygroup, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})
}