PopuExpl1Scat <- function(input, output, session, df){
  
  ns <- session$ns
  
# Scatterplot
widgets <- c("selPrmCode","groupbox","groupbyvar","selxvar","selyvar","AddLine","AddSmooth","DiscrXaxis")

# show all the widgets using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL
makeReactiveBinding("dfsub")

# remove any graphics instructions from the lists.  This is unique to PopuExpl1Scat
dfsel <- suppressWarnings(select(df(),-starts_with("geom_"),-starts_with("scale_"),-one_of("theme","ggtitle","xlabel","ylabel")))

# set checkbox to TRUE
updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)

# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  prmsel <- paste(input$selPrmCode,collapse = ",")

  dfflt <- filter(dfsel,PARAMCD %in% input$selPrmCode) # dfsel created from df() above

  # if two PARAMCDs selected, then we need to use pivot_wider()
  if (str_detect(prmsel,",") == TRUE) {
    # print(paste("two PARAMCDS:",prmsel))
    suppressWarnings(req(dfflt$AVAL)) # AVAL needs to exist.

    dffltw <- dfflt %>%
      pivot_wider(id_cols=c(USUBJID, AVISIT), names_from = c(PARAMCD), values_from = c(AVAL, BASE, CHG), names_sep = "_") 

    dfmrg <- dfflt[, !names(dfflt) %in% c(names(dffltw),"PARAM","PARAMCD","AVAL","BASE","CHG")]
    
    # add USUBJID and AVISIT
    dfmrg2 <- bind_cols(select(dfflt,USUBJID,AVISIT),dfmrg) %>%
    # dfmrg2 <- cbind(select(dfsub,USUBJID,AVISIT),dfmrg,stringsAsFactors = FALSE) %>%
      distinct(USUBJID, AVISIT, .keep_all = TRUE)

    dfflt <- left_join(dffltw, dfmrg2, by = c("USUBJID","AVISIT")) %>%
      mutate(PARAMCD = str_replace(prmsel,",","_")) %>%
      arrange(USUBJID, AVISITN)
  } else {
    # print(paste("just one PARAMCD:",prmsel))
  }
  
  dfsub <<- dfflt
  
  chr <- names(which(sapply(dfsub,is.character))) # all chr
  fac <- names(which(sapply(dfsub,is.factor   ))) # all factors
  num <- names(which(sapply(dfsub,is.numeric  ))) # all num
  
  # groupbyvar is loaded with all the character/factor columns
  updateSelectInput(session = session, inputId = "groupbyvar", choices = c(" ",sort(c(chr,fac))), selected = " ")
  
  # selxvar is loaded with all the columns
  updateSelectInput(session = session, inputId = "selxvar", choices = c(" ",sort(names(dfsub))),  selected = " ")
  
  # selyvar is loaded with all the numeric columns
  # updateSelectInput(session = session, inputId = "selyvar", choices = c(" ",sort(num)), selected = " ")
  updateSelectInput(session = session, inputId = "selyvar", choices = c(" ",sort(names(dfsub))), selected = " ")
  
  # uncheck these buttons:  "AddLine","AddSmooth","DiscrXaxis"
  updateCheckboxInput(session = session, inputId = "AddLine", value = FALSE)
  updateCheckboxInput(session = session, inputId = "AddSmooth", value = FALSE)
  updateCheckboxInput(session = session, inputId = "DiscrXaxis", value = FALSE)

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

output$PlotlyOut <- renderPlotly({
  
  req(input$radio != "0")
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  req(input$selPrmCode != " ") 

  xvar <- sym(input$selxvar)
  yvar <- sym(input$selyvar)
  
  labx <- sjlabelled::get_label(dfsub[[input$selxvar]])
  laby <- sjlabelled::get_label(dfsub[[input$selyvar]])

  # If there are two PARAMCDs we prefix the label with the PARAMCD
  if (str_detect(unique(dfsub$PARAMCD),"_") == TRUE) {
    labx <- str_c(substr(xvar,str_locate(xvar,"_")[1]+1,str_length(xvar)),labx,sep=":")
    laby <- str_c(substr(yvar,str_locate(yvar,"_")[1]+1,str_length(yvar)),laby,sep=":")
  } else {
    # just use the labels above
    # labx <- str_c(unique(dfsub$PARAMCD),labx,sep=":")
    # laby <- str_c(unique(dfsub$PARAMCD),laby,sep=":")
  }

  print("renderPlotly just before call to fnscatter")
  # correction for overplotting is located in fnscatter
  p <- fnscatter(data = dfsub, input$groupbox, input$groupbyvar, input$selxvar, input$selyvar)

  # minimal theme
  p <- p + theme_minimal()
  print("continuing to add statements")
  # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
  nlevs <- nlevels(factor(dfsub[[input$groupbyvar]]))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
  p <- p + scale_fill_manual(values = mycolors) 

  # add geom_line if checked
  if (input$AddLine == TRUE) {
    p <- p + geom_smooth(method='lm',    se = FALSE, na.rm = TRUE)
  }
  # add geom_smooth if checked
  if (input$AddSmooth == TRUE) {
    p <- p + geom_smooth(method="loess", se = FALSE, na.rm = TRUE)
  }
  # Discrete x-axis
  if (input$DiscrXaxis == TRUE) {
   p <- p + scale_x_discrete(limits=c(sort(unique(dfsub[[input$selxvar]]))))
  }

  # update title -- if plottitle (and xlabel and ylabel provided, use it)
  if ("xlabel" %in% colnames(dfsub) && "ylabel" %in% colnames(dfsub) && "ggtitle" %in% colnames(dfsub)) {
    p <- p + labs(x = unique(dfsub$xlabel), y = unique(dfsub$ylabel), title = unique(dfsub$ggtitle))
  } else {
  if (input$groupbox == TRUE) {
    
    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))

    if (str_detect(unique(dfsub$PARAMCD),"_") == TRUE) {
      # use labels prefixed with PARAMCD
      ggtitle <- reactive({ paste("Plot of",laby,"by",labx,"Grouped By",labz) })
    } else {
      # use labels followed by PARAM
      ggtitle <- reactive({ paste("Plot of",laby,"by",labx,"Grouped By",labz,"for",unique(dfsub$PARAM)) })
    }
  } else {

    if (str_detect(unique(dfsub$PARAMCD),"_") == TRUE) {
      # use labels prefixed with PARAMCD
      ggtitle <- reactive({ paste("Plot of",laby,"by",labx) })
    } else {
      # use labels followed by PARAM
      ggtitle <- reactive({ paste("Plot of",laby,"by",labx,"for",unique(dfsub$PARAM)) })
    }
  }
  p <- p + labs(title = ggtitle(), x = labx, y = laby)
  }

  ggcmd <- c("geom_point","geom_line","geom_vline","geom_hline","geom_errorbar","geom_bar","geom_text","geom_text2","coord_flip","geom_pointrange","theme",
             "scale_shape","scale_x_cont","scale_y_cont","scale_x_discr","scale_y_discr","scale_y_log10","scale_x_log10")
  # any embedded graph instructions?
  graphinst <- select(dfsub, any_of(ggcmd))
  # graphinst <- suppressWarnings(unique(select(dfsub, one_of(ggcmd))))

  # print(paste("graphinst has length",length(graphinst)))
  # 
  # display graph instructions for now
  if (length(graphinst) > 0) {
    graphinst <- unique(graphinst) %>% unlist(use.names = FALSE) # convert to unnamed vector
    for (i in 1:length(graphinst)) {
      print(graphinst[i])
    }
     p <- p + sapply(graphinst, function(gr) {eval(parse(text = gr))})
  }
  
  # workaround to remove "(" and "1)" from legend
  # Now, the workaround:
  # ------------------------------------------------------

p1 <-  ggplotly(p, tooltip = "text") 

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
  
  req(input$radio != "0")
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  
  if(input$groupbox == TRUE) {
    req(input$groupbyvar != " ")
    
    if ("USUBJID" %in% colnames(dfsub)) {
      tableout <- dfsub %>%
        dplyr::select(USUBJID, !!sym(input$groupbyvar), !!sym(input$selxvar), !!sym(input$selyvar)) 
    } else {
    tableout <- dfsub %>%
      dplyr::select(!!sym(input$groupbyvar), !!sym(input$selxvar), !!sym(input$selyvar))
    }
  } else {
    if ("USUBJID" %in% colnames(dfsub)) {
      tableout <- dfsub %>%
        dplyr::select(USUBJID, !!sym(input$selxvar), !!sym(input$selyvar))
    } else {
    tableout <- dfsub %>%
      dplyr::select(!!sym(input$selxvar), !!sym(input$selyvar))
    }
  }
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
  
})

}