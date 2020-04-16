PopuExpl5Hist <- function(input, output, session, df){
  
  ns <- session$ns
  
# Histogram
widgets <- c("selPrmCode","groupbox","groupbyvar","responsevar","numBins")

# show all the widgets using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL  # assign dfsub in function environment

chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

# groupbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",sort(c(chr,fac))), selected = " ")

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices = c(" ",num), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)

bins <- reactive({
  input$numBins
})

observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != " ") 
  
  # subset data based on Parameter Code selection
  dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator

}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  laby <- sjlabelled::get_label(dfsub[[input$responsevar]])
  
  if(input$groupbox == TRUE) {
    
    req(input$groupbyvar != " ")

    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))
    
    # correction for overplotting
    dfsub <- fnoverplt(dfsub, input$groupbyvar)

    # print(paste("N of rows",nrow(dfsub)))
    # remove missing groups from plot
    dfsub <- filter(dfsub, !is.na(!!sym(input$groupbyvar))) 
    # print(paste("N of rows",nrow(dfsub)))
    
    ggtitle <- reactive({ paste("Distribution of",laby,"Grouped by",labz,"for",unique(dfsub$PARAM)) })
    
    p <- ggplot(dfsub,aes(x = !!sym(input$responsevar), fill = !!sym(input$groupbyvar))) +
      labs(x = laby, y = "Count", fill = labz, title = ggtitle() )
    
  } else {
    
    ggtitle <- reactive({ paste("Distribution of",laby,"for",unique(dfsub$PARAM)) })
    
    p <- ggplot(dfsub, aes(x = !!sym(input$responsevar) )) +
      labs(y = "Count", title = ggtitle() )
    
  }

  p <- p +
    geom_histogram(bins = bins(), position = "stack", na.rm = TRUE) +
    scale_fill_discrete() 
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$groupbox == TRUE) {
    req(input$groupbyvar != " ")  
    
    # correction for overplotting
    # dfsub <- fnoverplt(dfsub,input$groupbyvar)
    
  }  
  tableout <- fnsummtab(dfsub, input$groupbox, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}