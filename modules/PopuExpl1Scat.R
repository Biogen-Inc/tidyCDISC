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
shinyjs::show(id="AddSmooth")
shinyjs::show(id="DiscrXaxis")
shinyjs::hide(id="UseCounts")

# remove any graphics instructions from the lists
dfsel <- suppressWarnings(select(df(),-starts_with("geom_"),-starts_with("scale_"),-one_of("theme","ggtitle","xlabel","ylabel")))

chr <- names(which(sapply(dfsel,is.character))) # all chr
fac <- names(which(sapply(dfsel,is.factor   ))) # all factors
num <- names(which(sapply(dfsel,is.numeric  ))) # all num

# print(paste("factors:",paste(fac,collapse = ",")))
# print(paste("numeric:",paste(num,collapse = ",")))
# print(paste("character:",paste(chr,collapse = ",")))

# splitbyvar is loaded with all the character/factor columns
updateSelectInput(session = session, inputId = "splitbyvar", choices = c(" ",sort(c(chr,fac))), selected = " ")

# selxvar is loaded with all the columns
updateSelectInput(session = session, inputId = "selxvar", choices = c(" ",sort(names(dfsel))),  selected = " ")

# selyvar is loaded with all the numeric columns
updateSelectInput(session = session, inputId = "selyvar", choices = c(" ",sort(num)), selected = " ")

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "splitbox", value = TRUE)


# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != " ") 
  
  dfsub <- filter(df(),PARAMCD == input$selPrmCode)
  
output$PlotlyOut <- renderPlotly({
  
  req(dfsub$PARAMCD == input$selPrmCode)
  req(input$radio != "0")
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  # correction for overplotting is located in fnscatter
  p <- fnscatter(data = dfsub, input$splitbox, input$splitbyvar, input$selxvar, input$selyvar)

  # expand the color palette
  nb.cols <- length(unique(dfsub[[input$splitbyvar]]))
  mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)
  # use scale_fill_manual
  p <- p + scale_fill_manual(values = mycolors)

  # add geom_line if checked
  if (input$AddLine == TRUE) {
    p <- p + geom_line()
  }
  # add geom_smooth if checked
  if (input$AddSmooth == TRUE) {
    p <- p + geom_smooth(method="loess", se=FALSE)
  }
  # Discrete x-axis
  if (input$DiscrXaxis == TRUE) {
   p <- p + scale_x_discrete(limits=c(sort(unique(dfsub[[input$selxvar]]))))
  }

  # update title -- if plottitle (and xlabel and ylabel provided, use it)
  if ("xlabel" %in% colnames(dfsub) && "ylabel" %in% colnames(dfsub) && "ggtitle" %in% colnames(dfsub)) {
    p <- p + labs(x = unique(dfsub$xlabel), y = unique(dfsub$ylabel), title = unique(dfsub$ggtitle))
  } else {
  if (input$splitbox == TRUE) {
    ggtitle <- reactive({ paste("Plot of",input$selyvar,"by",input$selxvar,"Grouped by",input$splitbyvar,"for PARAMCD:",unique(dfsub$PARAMCD)) })
  } else {
    ggtitle <- reactive({ paste("Plot of",input$selyvar,"by",input$selxvar,"for PARAMCD:",unique(dfsub$PARAMCD)) })
  }
  p <- p + labs(title = ggtitle())
  }

  ggcmd <- c("geom_point","geom_line","geom_vline","geom_hline","geom_errorbar","geom_bar","geom_text","geom_text2","coord_flip","geom_pointrange","theme",
             "scale_shape","scale_x_cont","scale_y_cont","scale_x_discr","scale_y_discr","scale_y_log10","scale_x_log10")
  # any embedded graph instructions?
  graphinst <- suppressWarnings(unique(select(dfsub, one_of(ggcmd))))
  
  graphinst <- unname(graphinst)
  
  # display graph instructions for now
  if (length(graphinst) > 0) {
    # for (i in 1:length(graphinst)) {
    #   print(graphinst[i])
    # }
     p <- p + sapply(graphinst, function(gr) {eval(parse(text = gr))})
  }
  ggplotly(p, tooltip = "text") %>%
    layout(legend = list(
      orientation = "h", x = 0, y = 0   #bottom left
      ))

})

output$DataTable <- DT::renderDataTable({
  
  req(dfsub$PARAMCD == input$selPrmCode)
  req(input$radio != "0")
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  x_var <- as.name(input$selxvar)
  y_var <- as.name(input$selyvar)
  
  # correction for overplotting
  dfsub <- fnoverplt(dfsub,input$selxvar)
  
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

}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

}