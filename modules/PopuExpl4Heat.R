PopuExpl4Heat <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Heatmap
shinyjs::hide(id="advstagsem")
shinyjs::show(id="adsltagsem")
shinyjs::hide(id="selPrmCode")
shinyjs::hide(id="bygroup")
shinyjs::hide(id="groupbyvar")
shinyjs::show(id="selxvar")
shinyjs::show(id="selyvar")
shinyjs::show(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::hide(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")

observe({
  
  updateSelectInput(
    session = session,
    inputId = "selxvar",
    choices = c(" ",sort(names(df()))),
    selected = " ")
  
  updateSelectInput(
    session = session,
    inputId = "selyvar",
    choices = c(" ",numcols),
    selected = " ") 
  
  updateSelectInput(
    session = session,
    inputId = "selzvar",
    choices = c(" ",numcols),
    selected = " ") 
})

output$PlotlyOut <- renderPlotly({
  
  # Wait for fill variable
  req(input$selzvar != " ")
  
  ggtitle <- glue::glue("Heatmap of {input$selzvar} - {input$selyvar} by {input$selxvar}")

  pp <- ggplot(df(), aes(x = !!as.name(input$selxvar), y = !!as.name(input$selyvar) )) +
    geom_tile(aes(fill = !!as.name(input$selzvar)), colour = "white", na.rm = TRUE) +
    ggtitle(ggtitle) +
    # scale_fill_discrete() +
    scale_fill_viridis_c(option = "D", direction = -1) +
    labs(  x = input$selxvar, y = input$selyvar, fill = input$selzvar) +
    theme_light()
  
  ggplotly(pp)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selzvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  z_var <- as.name(input$selzvar)
  
  tableout <- df() %>%
    dplyr::select(USUBJID, !!x_var, !!y_var, !!z_var)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames = c('PARAMCD' = 2))
  
})
}