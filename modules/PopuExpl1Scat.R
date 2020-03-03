PopuExpl1Scat <- function(input, output, session, df){
  
  ns <- session$ns
  
# Scatterplot
shinyjs::show(id="selPrmCode")
shinyjs::show(id="splitbox")
shinyjs::show(id="splitbyvar")
shinyjs::show(id="selxvar")
shinyjs::show(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::hide(id="seltimevar")
shinyjs::hide(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::hide(id="animate")
shinyjs::hide(id="animateby")
shinyjs::hide(id="numBins")
shinyjs::show(id="AddLine")
shinyjs::show(id="AddErrorBar")
shinyjs::show(id="DiscrXaxis")
shinyjs::hide(id="UseCounts")

# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != " ") # using ignoreInit = TRUE
  
  dfsub <- filter(df(),PARAMCD == input$selPrmCode)
  
  seltime <- select(dfsub, ends_with("DY"), starts_with("AVIS"))
  
chr <- sort(names(df()[ , which(sapply(df(),is.character))])) # all chr
fac <- sort(names(df()[ , which(sapply(df(),is.factor   ))])) # all factors
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

# splitbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "splitbyvar", choices = c(" ",sort(names(dfsub))), selected = " ")

# selxvar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "selxvar", choices = c(" ",sort(names(dfsub))),  selected = " ")

# selyvar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "selyvar", choices = c(" ",sort(names(dfsub))), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "splitbox", value = TRUE)


output$PlotlyOut <- renderPlotly({
  
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  # correction for overplotting
  # BDS records are usually by USUBJID, AVISIT, and PARAMCD
  # if not using AVISIT(n) then collapse to USUBJID level and set AVISIT to Baseline
  print(paste("befor",nrow(dfsub)))
  if (!input$selxvar %in% names(seltime) & "AVISIT" %in% names(dfsub)) {
    dfsub <- dfsub %>%
      filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      distinct(USUBJID, .keep_all = TRUE)
  } 
  print(paste("after",nrow(dfsub)))
  
  # plot function
  p <- fnscatter(data = dfsub, input$splitbox, input$splitbyvar, input$selxvar, input$selyvar)

  # add geom_line if checked
  if (input$AddLine == TRUE) {
    p <- p + geom_line() 
  }
  # add geom_errorbar if checked
  if (input$AddErrorBar == TRUE) {
    p <- p + geom_errorbar(aes(ymin=ymin, ymax=ymax))
  }
  # Discrete x-axis
  if (input$DiscrXaxis == TRUE) {
   p <- p + scale_x_discrete(limits=c(sort(unique(dfsub[[input$selxvar]]))))
  }

  # update title -- if plottitle (and xlabel and ylabel provided, use it)
  if ("xlabel" %in% colnames(dfsub) && "ylabel" %in% colnames(dfsub) && "ggtitle" %in% colnames(dfsub)) {
    p <- p + labs(x = unique(dfsub$xlabel), y = unique(dfsub$ylabel), title = unique(dfsub$ggtitle))
  } else {
  ggtitle <- paste("Plot of",input$selyvar,"by",input$selxvar,"Grouped by",input$splitbyvar,"for PARAMCD:",unique(dfsub$PARAMCD))
  p <- p + labs(title = ggtitle)
  }

  # any embedded graph instructions?
  graphinst <- suppressWarnings(unique(select(dfsub, one_of("geom_vline","geom_hline","geom_bar","geom_text","scale_x_cont","scale_y_cont"))))
  for (i in 1:length(graphinst)) {
    print(graphinst[[i]])
    gr <- graphinst[[i]]
    p <- p + eval(parse(text = gr))
  }
  ggplotly(p, tooltip = "text")

})

output$DataTable <- DT::renderDataTable({
  
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  
  if (!input$selxvar %in% names(seltime) & "AVISIT" %in% names(dfsub)) {
    dfsub <- dfsub %>%
      filter(AVISIT == "Baseline") %>% # Take analysis baseline for now
      distinct(USUBJID, .keep_all = TRUE)
  } 
  
  if(input$splitbox == TRUE) {
    req(input$splitbyvar != " ")
    z_var <- as.name(input$splitbyvar)
    if ("USUBJID" %in% colnames(dfsub)) {
      tableout <- dfsub %>%
        dplyr::select(USUBJID, !!z_var, !!x_var, !!y_var) 
    } else {
    tableout <- dfsub %>%
      dplyr::select(!!z_var, !!x_var, !!y_var)
    }
  } else {
    if ("USUBJID" %in% colnames(dfsub)) {
      tableout <- dfsub %>%
        dplyr::select(USUBJID, !!x_var, !!y_var)
    } else {
    tableout <- dfsub %>%
      dplyr::select(!!x_var, !!y_var)
    }
  }
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
  
})

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

}