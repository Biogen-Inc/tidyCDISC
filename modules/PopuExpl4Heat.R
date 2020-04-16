PopuExpl4Heat <- function(input, output, session, df){
  
  ns <- session$ns
  
# Heatmap
widgets <- c("selPrmCode","selxvar","selyvar","selzvar","fillType")

# show all the widgets we need here using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL  # assign dfsub in function environment

# num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num

updateSelectInput(
  session = session,
  inputId = "selxvar",
  choices = c(" ",sort(names(df()))),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "selyvar",
  choices = c(" ",sort(names(df()))),
  selected = " ") 

updateSelectInput(
  session = session,
  inputId = "selzvar",
  choices = c(" ",sort(names(df()))),
  selected = " ") 


observeEvent(input$selPrmCode, {
    
    req(input$selPrmCode != " ") 
  
    # subset data based on Parameter Code selection
    dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator
    
    updateSelectInput(
      session = session,
      inputId = "selyvar",
      choices = c(" ",sort(names(dfsub))),
      selected = " ") 
    
}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

observeEvent(input$fillType, {

  req(input$selPrmCode != " ")
  
      if (input$fillType == "Corr Matrix") {
        
        updateSelectizeInput(
          session = session,
          inputId = "selectvars",
          choices = sort(names(select_if(df(), is.numeric))),
          options = list(maxItems = NULL),
          selected = " ")
        
        shinyjs::show(id="selectvars")
        shinyjs::show(id="runCorr")
        shinyjs::hide(id="selxvar")
        shinyjs::hide(id="selyvar")
        shinyjs::hide(id="selzvar")

      } else {
        shinyjs::hide(id="selectvars")
        shinyjs::hide(id="runCorr")
        shinyjs::show(id="selxvar")
        shinyjs::show(id="selyvar")
        shinyjs::show(id="selzvar")
        
      }
      
}) # input$fillType
    
output$PlotlyOut <- renderPlotly({
  
  if (input$fillType %in% c("Use Counts","fill selected")) {
    
    # Wait for variables
    req(input$selxvar != " ")
    req(input$selyvar != " ")
    req(input$selPrmCode != " ")

  # set def.value to use name if the variable has no label attribute
  labx <- sjlabelled::get_label(dfsub[[input$selxvar]], def.value = unique(input$selxvar))
  laby <- sjlabelled::get_label(dfsub[[input$selyvar]], def.value = unique(input$selyvar))
  
  # remove missing values from plot
  # dfsub <- filter(dfsub, !is.na(!!sym(input$selxvar)), !is.na(!!sym(input$selyvar)) ) 

  # correction for overplotting
  # dfsub <- fnoverplt(dfsub,input$selxvar)
  }
  
  if (input$fillType == "Use Counts") {
    
  ggtitle <- reactive(glue::glue("Heatmap of {laby} by {labx}, for {unique(dfsub$PARAM)}"))

  p <- ggplot(dfsub, 
              aes(x = !!sym(input$selxvar), y = !!sym(input$selyvar) ))
  
  p <- p +
      geom_bin2d(na.rm = TRUE) +
      theme_minimal() +  # from https://www.rapidtables.com/web/color/RGB_Color.html
      # this scale goes from "white smoke" to black
      scale_fill_gradient(name = "Counts", low = "#F5F5F5", high = "#000000", na.value="white") +
      # scale_fill_gradient(low="lightyellow",high="darkred",na.value="white") +
      labs(  x = labx, y = laby, title = ggtitle()) 
    
  ggplotly(p)

  } else if (input$fillType == "Fill Variable") {  

    req(input$selzvar != " ")
    
    # set def.value to use name if the variable has no label attribute
    labx <- sjlabelled::get_label(dfsub[[input$selxvar]], def.value = unique(input$selxvar))
    laby <- sjlabelled::get_label(dfsub[[input$selyvar]], def.value = unique(input$selyvar))

    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$selzvar]], def.value = unique(input$selzvar))

    # remove missing groups from plot
    dfsub <- filter(dfsub, !is.na(!!sym(input$selzvar))) 

    ggtitle <- reactive(glue::glue("Heatmap of {labz} - {laby} by {labx}, for {unique(dfsub$PARAM)}"))

    p <- ggplot(dfsub, 
                aes(x = !!sym(input$selxvar), y = !!sym(input$selyvar) ))
    p <- p +
    geom_tile(aes(fill = !!sym(input$selzvar))) +
    scale_fill_gradient(name = input$selzvar, low = "#FFFFFF", high = "#012345") +
    theme_minimal() 
    # scale_fill_viridis_c(option = "D", direction = -1) 
    
    # p <- p + 
    # labs(  x = labx, y = laby, fill = labz, title = ggtitle()) 
    
    ggplotly(p)
  } else {
    
    req(input$selectvars != "")
    req(input$runCorr > 0)
    
    dfsel <- select(dfsub, input$selectvars)
    dfsel <- select_if(dfsel, is.numeric)
    
    # fncorrmat generates correlation matrix and returns a "gathered" df
    gathered_cormat <- fncorrmat(dfsel)

    ggtitle <- reactive(glue::glue("Correlation Matrix Heatmap, for {unique(dfsub$PARAM)}"))
    
      p <- ggplot(gathered_cormat, aes(Var2, Var1, fill = value))+
        geom_tile(color = "white") +
        labs(title = ggtitle()) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0, limit = c(-1,1), space = "Lab",
                             name="Pearson\nCorrelation") +
        theme_minimal() + # minimal theme
        theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                         size = 12, hjust = 1))+
        coord_fixed()
      
      p <- p +
        geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(0.6, 0.7),
          legend.direction = "horizontal") +
        guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
              title.position = "top", title.hjust = 0.5))

    ggplotly(p)
    
    }

})

# table needs more work
output$DataTable <- DT::renderDataTable({
  
  req(input$fillType %in% c("Use Counts","Fill Variable"))
  
  # Wait for variables
  req(input$selxvar != " ")
  req(input$selyvar != " ")
  req(input$selPrmCode != " ")
  
  # correction for overplotting
  # dfsub <- fnoverplt(dfsub,input$selxvar)
  
  if (input$fillType == "Use Counts") {

    dfcnts <- dfsub %>%
      group_by(PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar) ) %>%
      summarise(Counts = n()) %>%
      ungroup()
    
    dfsub <- distinct(dfsub,PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar),.keep_all = TRUE) %>%
      arrange(!!sym(input$selxvar), !!sym(input$selyvar))
    
    dfsub <- left_join(dfsub,dfcnts)
    
    tableout <- dfsub %>%
      dplyr::filter(!is.na(!!sym(input$selxvar))) %>%
      dplyr::select(PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar), Counts)
    
    DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames =  c('PARAMCD' = 2))
    
  } else if (input$fillType == "Fill Variable") {
    
  req(input$selzvar != " ")
    
  tableout <- dfsub %>%
  dplyr::filter(!is.na(!!sym(input$selxvar))) %>%
  dplyr::select(PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar), !!sym(input$selzvar))
  
  DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames =  c('PARAMCD' = 2))
  
  } else {}

})

}