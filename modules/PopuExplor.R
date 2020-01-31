PopuExplor <- function(input, output, session, datafile, dataselected){
  
  ns <- session$ns
  
# show/hide checkboxes depending on radiobutton selection
observeEvent(input$radio,{
  
  # get numeric vs char data from ADSL  
  adsl.chrcols <- sort(names(datafile()[["ADSL"]][ , which(sapply(datafile()[["ADSL"]],is.character))])) # all chr columns
  adsl.numcols <- sort(names(datafile()[["ADSL"]][ , which(sapply(datafile()[["ADSL"]],is.numeric))]))   # all num columns
  
  # Clear plotoutput
  output$PlotlyOut <- renderPlotly({
    NULL
  })
  # Clear datatable
  output$DataTable <- DT::renderDataTable({
    NULL
  })  
  
  switch(input$radio, # use swtich() instead of if/else
         "1" = {
           # scatter plot module
           dataset <- reactive({ datafile()[["ADSL"]] })
           callModule(PopuExpl1Scat, id = NULL, dataset, numcols = adsl.numcols, chrcols = adsl.chrcols)
         },
         "2" = {
           # spaghetti plot module
           if ("ADVS" %in% dataselected()) {
             dataset <- reactive({ datafile()[["ADVS"]] })
             callModule(PopuExpl2Spag, id = NULL, dataselected, dataset)
           } else {
             shinyjs::alert("ADVS data not selected; required for spaghetti plot")
           }
         },
         "3" = {
           # box plot module
           dataset <- reactive({ datafile()[["ADSL"]] })
           callModule(PopuExpl3Boxp, id = NULL, dataset, numcols = adsl.numcols, chrcols = adsl.chrcols)
         },
         "4" = {
           # heat map module
           dataset <- reactive({ datafile()[["ADSL"]] })
           callModule(PopuExpl4Heat, id = NULL, dataset, numcols = adsl.numcols, chrcols = adsl.chrcols)
         },
         "5" = {
           # histogram module
           dataset <- reactive({ datafile()[["ADSL"]] })
           callModule(PopuExpl5Hist, id = NULL, dataset, numcols = adsl.numcols, chrcols = adsl.chrcols)
         },
         # This should not happen
         stop("invalid",input$radio,"value")
  )
  
})
} # PopuExplor