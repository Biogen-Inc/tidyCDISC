PopuExpl1Scat <- function(input, output, session, df, numcols, chrcols){
  
  ns <- session$ns
  
# Scatterplot
shinyjs::show(id="selPrmCode")
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

updateSelectInput(
  session = session,
  inputId = "selPrmCode",
  choices = c(" ",unique(df()$PARAMCD)),
  selected = " ")

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
  
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  # plot function
  p <- fnscatter(data = dfsubset, input$bygroup, input$groupbyvar, input$selxvar, input$selyvar)
  # update title
  ggtitle <- paste("Plot of",input$selyvar,"by",input$selxvar,"Grouped by",input$groupbyvar,"for PARAMCD:",unique(dfsubset$PARAMCD))
  p <- p + labs(title = ggtitle)
  
  ggplotly(p, tooltip = "text")
  
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  
  if(input$bygroup == TRUE) {
    req(input$groupbyvar != " ")
    z_var <- as.name(input$groupbyvar)
    tableout <- dfsubset %>%
      dplyr::select(USUBJID, !!z_var, !!x_var, !!y_var)
  } else {
    tableout <- dfsubset %>%
      dplyr::select(USUBJID, !!x_var, !!y_var)
  }
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
  
})
}, ignoreInit = TRUE)

}