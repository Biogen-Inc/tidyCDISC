PopuExpl3Boxp <- function(input, output, session, df){
  
  ns <- session$ns
  
# Box Plot
widgets <- c("selPrmCode","groupbox","groupbyvar","responsevar","AddPoints")

# show all the widgets using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL  # assign dfsub in function environment
makeReactiveBinding("dfsub")

chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

# groupbyvar is loaded with all the character/factor columns
# updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected = "")
updateSelectInput(session = session, inputId = "groupbyvar", choices = c("",sort(c(chr,fac))), selected = "")

# responsevar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "responsevar", choices =  c("",num), selected = "")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)


# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != "") 

  # subset data based on Parameter Code selection
  dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator
  
}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

output$PlotlyOut <- renderPlotly({
  
  req(input$selPrmCode != "") 
  req(!is.null(dfsub))
  req(!is_empty(input$responsevar) && input$responsevar != "")
  
  laby <- sjlabelled::get_label(dfsub[[input$responsevar]], def.value = unique(input$responsevar))
  
  # correction for overplotting is located in fnboxplot
  p <- fnboxplot(data = dfsub, input$groupbox, input$groupbyvar, input$responsevar, input$AddPoints)

  # Add labels here
  if (input$groupbox == TRUE) {
    
    req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
    
    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))

    ggtitle <- reactive({ paste("Plot of",laby,"Grouped by",labz,"for",unique(dfsub$PARAM)) })
    p <- p + labs(title = ggtitle(), x = labz, y = laby)
    
  } else {
    ggtitle <- reactive({ paste("Plot of",laby,"for",unique(dfsub$PARAM)) })
    p <- p + labs(title = ggtitle(), y = laby)
  }

  # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
  nlevs <- nlevels(factor(dfsub[[input$groupbyvar]]))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
  p <- p + scale_fill_manual(values = mycolors) 
  
  p1 <-  ggplotly(p, tooltip = "text")
  
  if (input$AddPoints == TRUE) {
    # remove outliers in plotly
    p1$x$data <- lapply(p1$x$data, FUN = function(x){
      
      if (x$type == "box") {
        x$marker = list(opacity = 0)
      }
      return(x)
    })
  } 
  
  if(input$groupbox == TRUE) {
    # Now, the workaround:
    # ------------------------------------------------------
    dfflt <- filter(dfsub,!is.na(!!sym(input$groupbyvar)))
    p1Names <- unique(dfflt[[input$groupbyvar]]) # we need to know the "true" legend values
    for (i in 1:length(p1$x$data)) { # this goes over all places where legend values are stored
      n1 <- p1$x$data[[i]]$name # and this is how the value is stored in plotly
      n2 <- " "
      for (j in 1:length(p1Names)) {
        if (grepl(x = n1, pattern = p1Names[j])) {n2 = p1Names[j]} # if the plotly legend name contains the original value, replace it with the original value
      }
      p1$x$data[[i]]$name <- n2 # now is the time for actual replacement
      if (n2 == " ") {p1$x$data[[i]]$showlegend = FALSE}  # sometimes plotly adds to the legend values that we don't want, this is how to get rid of them, too
    }
  }
  
  return(p1)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selPrmCode != "") 
  req(!is.null(dfsub))
  req(!is_empty(input$responsevar) && input$responsevar != "")

  if(input$groupbox == TRUE) {
    req(!is_empty(input$groupbyvar) && input$groupbyvar != "")

    # correction for overplotting
    dfsub <- fnoverplt(dfsub,input$responsevar, input$groupbyvar)
    
  } 
  tableout <- fnsummtab(data = dfsub, input$groupbox, input$groupbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})


}