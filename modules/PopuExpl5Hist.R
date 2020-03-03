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


bins <- reactive({
  input$numBins
})

observeEvent(input$selPrmCode, {
  
  # subset data based on Parameter Code selection
  req(input$selPrmCode != " ") # using ignoreInit = TRUE
  dfsub <- filter(df(),PARAMCD == input$selPrmCode)
  
  chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
  fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
  num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
  
  # splitbyvar is loaded with all the character/factor columns
  updateSelectInput(session = session, inputId = "splitbyvar", choices = c(" ",sort(c(chr,fac))), selected = " ")

  # responsevar is loaded with all the numeric columns
  updateSelectInput(session = session, inputId = "responsevar", choices = c(" ",num), selected = " ")
  
  # set checkbox to TRUE
  updateCheckboxInput(session = session, inputId = "splitbox", value = TRUE)
  
output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  y_var <- as.name(input$responsevar)
  
  if(input$splitbox == TRUE) {
    
    req(input$splitbyvar != " ")
    
    x_var <- as.name(input$splitbyvar)
    
    # restrict seltimevar to AVISIT, AVISITN, VSDY
    seltime <- select(dfsub, ends_with("DY"), starts_with("AVIS"))
    
    # correction for overplotting
    # BDS records are usually by USUBJID, AVISIT, and PARAMCD
    # if not using AVISIT(n) then collapse to USUBJID level and set AVISIT to Baseline
    if (!input$splitbyvar %in% names(seltime) & "AVISIT" %in% names(dfsub)) {
      dfsub <- dfsub %>%
        filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
        distinct(USUBJID, .keep_all = TRUE)
    }
    
    ggtitle <- paste("Distribution of",input$responsevar,"Grouped by",input$splitbyvar,"for PARAMCD:",unique(dfsub$PARAMCD))
    p <- ggplot(dfsub,
                aes(x = !!y_var, fill = !!x_var)) +
      labs(x = input$responsevar, y = "Count", fill = input$splitbyvar )
    
  } else {
    
    ggtitle <- paste("Distribution of",input$responsevar,"for PARAMCD:",unique(dfsub$PARAMCD))
    p <- ggplot(dfsub,
                aes(x = !!y_var )) +
      labs(y = "Count" )
    
  }
  p <- p +
    geom_histogram(bins = bins(), position = "stack", na.rm = TRUE) +
    scale_fill_discrete() +
    ggtitle(ggtitle) 
  
  ggplotly(p)
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$splitbox == TRUE) {
    req(input$splitbyvar != " ")  
    # correction for overplotting
    # BDS records are usually by USUBJID, AVISIT, and PARAMCD
    # if not using AVISIT(n) then collapse to USUBJID level and set AVISIT to Baseline
    if (!input$splitbyvar %in% names(seltime) & "AVISIT" %in% names(dfsub)) {
      dfsub <- dfsub %>%
        filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
        distinct(USUBJID, .keep_all = TRUE)
    }
  }   
  
  tableout <- fnsummtab(dfsub, input$splitbox, input$splitbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

}