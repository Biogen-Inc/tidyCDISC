PopuExpl1Scat <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Scatterplot
shinyjs::hide(id="advstagsem")
shinyjs::show(id="adsltagsem")
shinyjs::hide(id="selPrmCode")
shinyjs::show(id="bygroup")
shinyjs::show(id="groupbyvar")
shinyjs::show(id="selxvar")
shinyjs::show(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::hide(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",chrcols), selected = " ")

# selxvar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "selxvar", choices = c(" ",numcols),  selected = " ")

# selyvar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "selyvar", choices = c(" ",numcols), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "bygroup", value = TRUE)

observe({
  if(input$bygroup == TRUE) {
    shinyjs::show(id="groupbyvar")
  } else {
    shinyjs::hide(id="groupbyvar")
  }
})

output$PlotlyOut <- renderPlotly({
  
  req(input$selyvar != " ")
  
  # plot function
  p <- fnscatter(data = df(), input$bygroup, input$groupbyvar, input$selxvar, input$selyvar)
  
  ggplotly(p, tooltip = "text")
  
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selyvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")
    
    z_var <- as.name(input$groupbyvar)
    
    tableout <- df() %>%
      dplyr::select(USUBJID, !!z_var, !!x_var, !!y_var)
  } else {
    tableout <- df() %>%
      dplyr::select(USUBJID, !!x_var, !!y_var)
  }
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
  
})
}