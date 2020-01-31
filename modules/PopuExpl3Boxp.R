PopuExpl3Boxp <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Box Plot
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
shinyjs::show(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",chrcols), selected = " ")

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices =  c(" ",numcols), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "bygroup", value = TRUE)

output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  p <- fnboxplot(data = df(), input$bygroup, input$groupbyvar, input$responsevar )
  
  if(input$AddPoints == TRUE) {
    p <- p +
      suppressWarnings(geom_point(position = 'jitter', alpha = 0.2,
                                  aes(text = 
                                        paste0(USUBJID,
                                               "<br>",input$groupbyvar, ": ",get(input$groupbyvar),
                                               "<br>",input$responsevar,": ",get(input$responsevar)
                                        )
                                  ))) # aes, geom_point, suppressWarnings 
  }
  
  ggplotly(p, tooltip = "text")
  # ggplotly(p)
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")  
  }   
  
  tableout <- fnsummtab(data = df(), input$bygroup, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})
}