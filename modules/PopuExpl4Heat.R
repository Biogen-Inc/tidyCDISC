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
shinyjs::hide(id="AddSmooth")
shinyjs::hide(id="DiscrXaxis")
shinyjs::show(id="UseCounts")

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

observeEvent(input$selPrmCode, {
    
    req(input$selPrmCode != " ") 
  
    # subset data based on Parameter Code selection
    dfsub <- filter(df(),PARAMCD == input$selPrmCode)
    
output$PlotlyOut <- renderPlotly({
  
  # Wait for variables
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  req(input$selzvar != " " | input$UseCounts == TRUE)
  
  # correction for overplotting
  dfsub <- fnoverplt(dfsub,input$selxvar)
  
  ggtitle <- glue::glue("Heatmap of {input$selzvar} - {input$selyvar} by {input$selxvar}, for PARAMCD: {unique(dfsub$PARAMCD)}")

  p <- ggplot(data=subset(dfsub, !is.na(input$selxvar)),
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

# table needs more work
output$DataTable <- DT::renderDataTable({
  
  # Wait for variables
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  req(input$selzvar != " " | input$UseCounts == TRUE)
  
  if (input$UseCounts == TRUE) {
    dfcnts <- dfsub %>%
      group_by(PARAMCD, !!as.name(input$selxvar), !!as.name(input$selyvar) ) %>%
      summarise(Counts = n()) %>%
      ungroup()
    
    dfsub <- distinct(dfsub,PARAMCD, !!as.name(input$selxvar), !!as.name(input$selyvar),.keep_all = TRUE) %>%
      arrange(!!as.name(input$selxvar), !!as.name(input$selyvar))
    
    dfsub <- left_join(dfsub,dfcnts)
    
  }
  # If UseCounts == T, then set z_var to Counts, otherwise use input$selzvar
  z_var <- ifelse(input$UseCounts == TRUE,substitute(Counts),as.name(input$selzvar) )

  # correction for overplotting
  dfsub <- fnoverplt(dfsub,input$selxvar)
  
  tableout <- dfsub %>%
  dplyr::filter(!is.na(!!as.name(input$selxvar))) %>%
  dplyr::select(PARAMCD, !!as.name(input$selxvar), !!as.name(input$selyvar), !!z_var)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames =  c('PARAMCD' = 2))
  
})

}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

}