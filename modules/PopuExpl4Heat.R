PopuExpl4Heat <- function(input, output, session, df){
  
  ns <- session$ns
  
# Heatmap
shinyjs::show(id="selPrmCode")
shinyjs::hide(id="splitbox")
shinyjs::hide(id="splitbyvar")
shinyjs::show(id="selxvar")
shinyjs::show(id="selyvar")
shinyjs::show(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::hide(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")
shinyjs::hide(id="AddLine")
shinyjs::hide(id="AddErrorBar")
shinyjs::hide(id="DiscrXaxis")
shinyjs::show(id="UseCounts")


observeEvent(input$selPrmCode, {
    
    # subset data based on Parameter Code selection
    req(input$selPrmCode != " ") # using ignoreInit = TRUE
    dfsub <- filter(df(),PARAMCD == input$selPrmCode)
    
    # restrict seltimevar to AVISIT, AVISITN, VSDY
    seltime <- select(dfsub, ends_with("DY"), starts_with("AVIS")) 
    
    num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
    
    updateSelectInput(
      session = session,
      inputId = "selxvar",
      choices = c(" ",sort(names(df()))),
      selected = " ")
    
    updateSelectInput(
      session = session,
      inputId = "selyvar",
      choices = c(" ",sort(names(df()))),
      selected = " ") 
    
    updateSelectInput(
      session = session,
      inputId = "selzvar",
      choices = c(" ",sort(names(df()))),
      selected = " ") 
    
    # restrict seltimevar to AVISIT, AVISITN, VSDY
    seltime <- select(dfsub, ends_with("DY"), starts_with("AVIS"))
   
    if (!input$selxvar %in% names(seltime) & "AVISIT" %in% names(dfsub)) {
      dfsub <- dfsub %>%
        filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
        distinct(USUBJID, .keep_all = TRUE)
    }
    
output$PlotlyOut <- renderPlotly({
  
  # Wait for variables
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  req(input$selzvar != " " | input$UseCounts == TRUE)
  
  ggtitle <- glue::glue("Heatmap of {input$selzvar} - {input$selyvar} by {input$selxvar}, for PARAMCD: {unique(dfsub$PARAMCD)}")

  p <- ggplot(data=subset(dfsub, input$selxvar != ""),
              aes(x = !!as.name(input$selxvar), y = !!as.name(input$selyvar) ))
  
  if (input$UseCounts == TRUE) {
    p <- p +
      geom_bin2d(na.rm = TRUE) +
      scale_fill_gradient(low="yellow",high="red",na.value="white")
      
  } else {
    p <- p +
    geom_tile(aes(fill = !!as.name(input$selzvar))) +
    scale_fill_viridis_c(option = "D", direction = -1) 
  }
    p <- p + 
    ggtitle(ggtitle) +
    labs(  x = input$selxvar, y = input$selyvar, fill = input$selzvar) 
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selzvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  z_var <- as.name(input$selzvar)
  
  tableout <- dfsub %>%
    dplyr::select(!!x_var, !!y_var, !!z_var)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames = c('PARAMCD' = 2))
  
})

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

}