
    
#' popExpHeat Server Function
#'
#' @noRd 
mod_popExpHeat_server <- function(input, output, session, df){
  ns <- session$ns
 
  # Heatmap
  widgets <- c("selPrmCode","selxvar","selyvar","selzvar","fillType","heatMapFill")
  
  # show all the widgets we need here using an anonymous function
  map(widgets, function(x) shinyjs::show(x))
  
  dfsub <- NULL  # assign dfsub in function environment
  makeReactiveBinding("dfsub")
  
  # function mode
  mode <- function(codes){
    which.max(tabulate(codes))
  }
  # num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
  
  updateSelectInput(session = session, inputId = "selxvar", choices = c("",sort(names(df()))), selected = "")
  updateSelectInput(session = session, inputId = "selyvar", choices = c("",sort(names(df()))), selected = "") 
  updateSelectInput(session = session, inputId = "selzvar", choices = c("",sort(names(df()))), selected = "") 
  
  observeEvent(input$selPrmCode, {
    
    req(!is_empty(input$selPrmCode) && input$selPrmCode != "") 
    
    # subset data based on Parameter Code selection
    dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator
    
  }, ignoreInit = TRUE) # observeEvent(input$selPrmCode
  
  observeEvent(input$fillType, {
    
    req(input$selPrmCode != "")
    
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
      shinyjs::hide(id="heatMapFill")
      
    } else if (input$fillType == "Use Counts") {
      shinyjs::hide(id="selectvars")
      shinyjs::hide(id="runCorr")
      shinyjs::show(id="selxvar")
      shinyjs::show(id="selyvar")
      shinyjs::hide(id="selzvar")
      shinyjs::hide(id="heatMapFill")
      
    } else {
      shinyjs::hide(id="selectvars")
      shinyjs::hide(id="runCorr")
      shinyjs::show(id="selxvar")
      shinyjs::show(id="selyvar")
      shinyjs::show(id="selzvar")
      shinyjs::show(id="heatMapFill")
    }
    
  }, ignoreInit = TRUE) # input$fillType
  
  output$PlotlyOut <- renderPlotly({
    
    req(input$selPrmCode != "") 
    req(!is.null(dfsub))
    if (input$fillType %in% c("Use Counts","fill selected")) {
      
      # Wait for variables
      req(!is_empty(input$selxvar) && input$selxvar != "")
      req(!is_empty(input$selyvar) && input$selyvar != "")
      
      # set def.value to use name if the variable has no label attribute
      labx <- sjlabelled::get_label(dfsub[[input$selxvar]], def.value = unique(input$selxvar))
      laby <- sjlabelled::get_label(dfsub[[input$selyvar]], def.value = unique(input$selyvar))
      
      # remove missing values from plot
      dfsub <- filter(dfsub, !is.na(!!sym(input$selxvar)), !is.na(!!sym(input$selyvar)) ) 
      
      # correction for overplotting
      dfsub <- fnoverplt(dfsub,input$selxvar, input$selyvar)
    }
    
    if (input$fillType == "Use Counts") {
      
      ggtitle <- reactive(glue::glue("Heatmap of {laby} by {labx}, for {unique(dfsub$PARAM)}"))
      
      p <- ggplot(dfsub, 
                  aes(x = !!sym(input$selxvar), y = !!sym(input$selyvar) ))
      
      p <- p +
        geom_bin2d(na.rm = TRUE) +
        theme_minimal() +  # from https://www.rapidtables.com/web/color/RGB_Color.html
        # this scale goes from "white smoke" to black
        scale_fill_gradient(name = "Count", low = "#F5F5F5", high = "#000000", na.value="white") +
        # scale_fill_gradient(low="lightyellow",high="darkred",na.value="white") +
        labs(  x = labx, y = laby, title = ggtitle()) 
      
      ggplotly(p)
      
    } else if (input$fillType == "Fill Variable") {  
      
      req(!is_empty(input$selxvar) && input$selxvar != "")
      req(!is_empty(input$selyvar) && input$selyvar != "")
      req(!is_empty(input$selzvar) && input$selzvar != "")
      
      # set def.value to use name if the variable has no label attribute
      labx <- sjlabelled::get_label(dfsub[[input$selxvar]], def.value = unique(input$selxvar))
      laby <- sjlabelled::get_label(dfsub[[input$selyvar]], def.value = unique(input$selyvar))
      
      # set def.value to use name if the variable has no label attribute
      labz <- sjlabelled::get_label(dfsub[[input$selzvar]], def.value = unique(input$selzvar))
      
      # remove missing values from plot
      dfsub <- filter(dfsub, !is.na(!!sym(input$selxvar)), !is.na(!!sym(input$selyvar)) ) 
      # remove missing groups from plot
      dfsub <- filter(dfsub, !is.na(!!sym(input$selzvar))) 
      
      ggtitle <- reactive(glue::glue("Heatmap of {labz} - {laby} by {labx}, for {unique(dfsub$PARAM)}"))
      
      # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
      mode <- function(x, na.rm = FALSE) {
        if(na.rm){
          x = x[!is.na(x)]
        }
        ux <- unique(x)
        return(ux[which.max(tabulate(match(x, ux)))])
      }
      
      dfsum <- dfsub %>%
        group_by(!!sym(input$selxvar), !!sym(input$selyvar)) %>%
        summarise(Count = n(),
                  Min  = round(min(!!sym(input$selzvar))),
                  Mode = round(mode(!!sym(input$selzvar))),
                  Mean = round(mean(!!sym(input$selzvar))),
                  Median = round(median(!!sym(input$selzvar))),
                  Max  = round(max(!!sym(input$selzvar)))
        ) %>%
        ungroup()
      
      
      fillvar <- reactive({ input$heatMapFill })
      
      # p <- ggplot(dfsub, 
      #             aes(!!sym(input$selxvar), y = !!sym(input$selyvar), z = !!sym(input$selzvar))) +
      #             stat_summary_2d(fun = fillvar(), na.rm = TRUE) 
      p <- ggplot(dfsum,
                  aes(x = !!sym(input$selxvar), y = !!sym(input$selyvar), fill = .data[[fillvar()]] )) +
        geom_tile()
      
      labz <- paste(fillvar(),labz)
      # labs(fill = glue::glue( {rlang::quo_text(FUN)},"\nHorsepower"))
      
      p <- p +
        theme_minimal() +  # from https://www.rapidtables.com/web/color/RGB_Color.html
        # this scale goes from "white smoke" to black
        scale_fill_gradient(name = "Count", low = "#F5F5F5", high = "#000000", na.value="white") 
      
      p <- p +
        labs(  x = labx, y = laby, fill = labz, title = ggtitle()) 
      
      ggplotly(p)
      
    } else {
      # correlation matrix
      
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
    
    req(input$selPrmCode != "") 
    req(input$fillType %in% c("Use Counts","Fill Variable"))
    
    req(!is.null(dfsub))
    
    # Wait for variables
    req(!is_empty(input$selxvar) && input$selxvar != "")
    req(!is_empty(input$selyvar) && input$selyvar != "")
    
    # correction for overplotting
    dfsub <- fnoverplt(dfsub,input$selxvar, input$selyvar)
    
    if (input$fillType == "Use Counts") {
      
      tableout <- dfsub %>%
        group_by(PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar) ) %>%
        summarise(Count = n() ) %>%
        ungroup()
      
      DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames =  c('PARAMCD' = 2))
      
    } else if (input$fillType == "Fill Variable") {
      
      req(!is_empty(input$selzvar) && input$selzvar != "")
      
      tableout <- dfsub %>%
        group_by(PARAMCD, !!sym(input$selxvar), !!sym(input$selyvar) ) %>%
        summarise(Count = n(),
                  Mode = mode(!!sym(input$selzvar)),
                  Min = min(!!sym(input$selzvar),na.rm = TRUE), 
                  Q1 = quantile(!!sym(input$selzvar), 0.25, na.rm = TRUE, type=1), 
                  Median = median(!!sym(input$selzvar), na.rm = TRUE, type=1), 
                  Mean = round(mean(!!sym(input$selzvar),na.rm = TRUE),2),
                  SD   = round(sd(!!sym(input$selzvar),na.rm = TRUE),3),
                  Q3 = quantile(!!sym(input$selzvar), 0.75, na.rm = TRUE, type=1), 
                  Max = max(!!sym(input$selzvar), na.rm = TRUE) ) %>%
        ungroup()
      
      
      DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10), colnames =  c('PARAMCD' = 2))
      
    } else {}
    
  })
}
    
## To be copied in the UI --erased
# mod_popExpHeat_ui("popExpHeat_ui_1")
    
## To be copied in the server -- done
# callModule(mod_popExpHeat_server, "popExpHeat_ui_1")
 
