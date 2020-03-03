PopuExpl3Boxp <- function(input, output, session, df){
  
  ns <- session$ns
  
# Box Plot
shinyjs::show(id="selPrmCode")
shinyjs::show(id="splitbox")
shinyjs::show(id="splitbyvar")
shinyjs::hide(id="selxvar")
shinyjs::hide(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::show(id="responsevar")
shinyjs::show(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")
shinyjs::hide(id="AddLine")
shinyjs::hide(id="AddErrorBar")
shinyjs::hide(id="DiscrXaxis")
shinyjs::hide(id="UseCounts")


# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  # subset data based on Parameter Code selection
  req(input$selPrmCode != " ") # using ignoreInit = TRUE

  dfsub <- filter(df(),PARAMCD == input$selPrmCode)
  
  chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
  fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
  num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
  
  print(paste("factors:",paste(fac,collapse = )))
  
  # splitbyvar is loaded with all the character/factor columns
  updateSelectInput(session = session, inputId = "splitbyvar", choices = c(" ",sort(names(dfsub))), selected = " ")

  # responsevar is loaded with all the numeric columns
  updateSelectInput(session = session, inputId = "responsevar", choices =  c(" ",num), selected = " ")
  
  # set checkbox to TRUE
  updateCheckboxInput(session = session, inputId = "splitbox", value = TRUE)
  

output$PlotlyOut <- renderPlotly({
  
  req(input$responsevar != " ")
  
  # correction for overplotting in fnboxplot
  p <- fnboxplot(data = dfsub, input$splitbox, input$splitbyvar, input$responsevar )
  
  if(input$AddPoints == TRUE) {
    p <- p +
      suppressWarnings(geom_point(position = 'jitter', alpha = 0.2,
                       aes(text = 
                       paste0(USUBJID,
                       "<br>",input$splitbyvar, ": ",get(input$splitbyvar),
                       "<br>",input$responsevar,": ",get(input$responsevar)
                       )
                       ))) # aes, geom_point, suppressWarnings 
  }
  
  # update title
  ggtitle <- paste("Plot of",input$responsevar,"Grouped by",input$splitbyvar,"for PARAMCD:",unique(dfsub$PARAMCD))
  p <- p + labs(title = ggtitle)
  
  ggplotly(p, tooltip = "text")
})

output$DataTable <- DT::renderDataTable({
  
  req(input$responsevar != " ")
  
  if(input$splitbox == TRUE) {
    req(input$splitbyvar != " ")  
    
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
  }   
  
  tableout <- fnsummtab(data = dfsub, input$splitbox, input$splitbyvar, input$responsevar)
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 20))
  
})

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

}