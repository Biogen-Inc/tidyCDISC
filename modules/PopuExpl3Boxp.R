PopuExpl3Boxp <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Box Plot
shinyjs::show(id="selPrmCode")
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

updateSelectInput(
  session = session,
  inputId = "selPrmCode",
  choices = c(" ",unique(df()$PARAMCD)),
  selected = " ")

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",sort(names(df())), selected = " "))

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices =  c(" ",numcols), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "bygroup", value = TRUE)

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
  seltime <- select(dfsubset, ends_with("DY"), starts_with("AVIS")) %>%
    mutate(avisitceil = as.integer(ceiling(AVISITN)))
  
  if (!input$groupbyvar %in% names(seltime)) {
    dfsubset <- dfsubset %>%
      filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      distinct(USUBJID, .keep_all = TRUE)
  }
  
output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  p <- fnboxplot(data = dfsubset, input$bygroup, input$groupbyvar, input$responsevar )
  
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
  
  # update title
  ggtitle <- paste("Plot of",input$responsevar,"Grouped by",input$groupbyvar,"for PARAMCD:",unique(dfsubset$PARAMCD))
  p <- p + labs(title = ggtitle)
  
  ggplotly(p, tooltip = "text")
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")  
  }   
  
  tableout <- fnsummtab(data = dfsubset, input$bygroup, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

}