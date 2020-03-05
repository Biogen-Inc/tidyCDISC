PopuExpl5Hist <- function(input, output, session, df){
  
  ns <- session$ns
  
# Histogram
shinyjs::show(id="selPrmCode")
shinyjs::show(id="splitbox")
shinyjs::show(id="splitbyvar")
shinyjs::hide(id="selxvar")
shinyjs::hide(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::show(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::show(id="numBins")
shinyjs::hide(id="AddLine")
shinyjs::hide(id="AddErrorBar")
shinyjs::hide(id="DiscrXaxis")
shinyjs::hide(id="UseCounts")

chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

# splitbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "splitbyvar", choices = c(" ",sort(c(chr,fac))), selected = " ")

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices = c(" ",num), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "splitbox", value = TRUE)

bins <- reactive({
  input$numBins
})

observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != " ") 
  
  # subset data based on Parameter Code selection
  dfsub <- filter(df(),PARAMCD == input$selPrmCode)
  

output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  y_var <- as.name(input$responsevar)
  
  if(input$splitbox == TRUE) {
    
    req(input$splitbyvar != " ")
    
    x_var <- as.name(input$splitbyvar)
    
    # correction for overplotting
    dfsub <- fnoverplt(dfsub,input$splitbyvar)

    ggtitle <- reactive({ paste("Distribution of",input$responsevar,"Grouped by",input$splitbyvar,"for PARAMCD:",unique(dfsub$PARAMCD)) })
    
    p <- ggplot(dfsub,aes(x = !!y_var, fill = !!x_var)) +
      labs(x = input$responsevar, y = "Count", fill = input$splitbyvar, title = ggtitle() )
    
  } else {
    
    ggtitle <- reactive({ paste("Distribution of",input$responsevar,"for PARAMCD:",unique(dfsub$PARAMCD)) })
    
    p <- ggplot(dfsub, aes(x = !!y_var )) +
      labs(y = "Count", title = ggtitle() )
    
  }
  
  
  p <- p +
    geom_histogram(bins = bins(), position = "stack", na.rm = TRUE) +
    scale_fill_discrete() 
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$splitbox == TRUE) {
    req(input$splitbyvar != " ")  
    
    # correction for overplotting
    dfsub <- fnoverplt(dfsub,input$splitbyvar)
    
  }  
  tableout <- fnsummtab(dfsub, input$splitbox, input$splitbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

}