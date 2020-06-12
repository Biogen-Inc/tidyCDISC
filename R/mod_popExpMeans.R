#' popExpMeans Server Function
#' 
#' Generate a Means plot with error bars.
#' Child of parent module mod_popExp.R.
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param df dataframe passed to mod_popExpScat.

#'   DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom shinyjs show hide
#' @importFrom plotly ggplotly layout
#' 
#' @noRd
#' 
mod_popExpMeans_server <- function(input, output, session, df){
  ns <- session$ns
 
  # Means Plot -- similar to a ScatterPlot
  widgets <- c("selPrmCode","groupbox","groupbyvar","seltimevar","responsevar","DiscrXaxis","errorBars")
  
  # show all the widgets using an anonymous function
  map(widgets, function(x) shinyjs::show(x))
  
  dfsub <- NULL
  makeReactiveBinding("dfsub")
  
  # remove any graphics instructions from the lists.  This is unique to PopuExpl1Scat
  # dfsel <- suppressWarnings(select(df(),-starts_with("geom_"),-starts_with("scale_"),-one_of("theme","ggtitle","xlabel","ylabel")))
  
  # set checkbox to TRUE
  updateCheckboxInput(session = session, inputId = "groupbox", value = TRUE)
  
  chr <- names(which(sapply(df(),is.character))) # all chr
  fac <- names(which(sapply(df(),is.factor   ))) # all factors
  num <- names(which(sapply(df(),is.numeric  ))) # all num
  
  # uncheck these buttons:  "DiscrXaxis"
  updateCheckboxInput(session = session, inputId = "DiscrXaxis", value = FALSE)
  
  # restrict seltimevar to AVISIT, AVISITN, VSDY
  seltime <- select(df(), ends_with("DY"), contains("VIS"))
  
  updateSelectInput(session = session, inputId = "seltimevar",  
                    choices = c("",sort(names(seltime))), selected = "")
  
  updateSelectInput(session = session, inputId = "groupbyvar", 
                    choices = c("",sort(names(select(df(),any_of(c("TRT01A","TRT01P","TRT02A","TRT02P")))))), selected = "")
  
  updateSelectInput(session = session, inputId = "responsevar", 
                    choices = c("",sort(names(select(df(),ends_with("BL"),any_of(c("AVAL","BASE","CHG")))))), selected = "")
  
  updatePrettyRadioButtons(session = session, inputId = "errorBars", selected = "1")
  
  # https://groups.google.com/forum/#!topic/shiny-discuss/4GEJZW0ZDUc
  # suggestion from Joe Cheng
  # create reactive values for input$seltimevar and input$responsevar
  # v <- reactiveValues(input$seltimevar = character(0), input$responsevar = character(0))
  
  # update subsequent inputselects based on PARAM code selection
  observeEvent(input$selPrmCode, {
    
    req(input$selPrmCode != "") 
    
    # subset data based on Parameter Code selection
    dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator
    
    # Get ULN value, if available
    if ("LBSTNRHI" %in% colnames(dfsub)) {
      dfuln <- dfsub %>%
        group_by(PARAMCD) %>%
        summarise(ULN = min(LBSTNRHI))
      # merge ULN
      dfsub <<- left_join(dfsub, dfuln, by = "PARAMCD")
    } else {
      # just insert a value for now
      dfsub$ULN <<- NA
    }
    
    # updateSelectInput(session = session, inputId = "groupbyvar", 
    #                   choices = c("",sort(names(select(dfsub,any_of(c("TRT01A","TRT01P","TRT02A","TRT02P")))))), selected = "")
    
  }, ignoreInit = TRUE) # observeEvent(input$selPrmCode
  
  # observeEvent(input$input$seltimevar, {
  #   input$seltimevar <- input$input$seltimevar
  # })
  # observeEvent(input$input$responsevar, {
  #   input$responsevar <- input$input$responsevar
  # })
  
  
  
  output$PlotlyOut <- renderPlotly({
    
    req(input$selPrmCode != "") 
    req(!is.null(dfsub))
    
    # Wait for variables
    req(!is_empty(input$seltimevar) && input$seltimevar != "")
    req(!is_empty(input$responsevar) && input$responsevar != "")
    
    labx <- sjlabelled::get_label(dfsub[[input$seltimevar]], def.value = unique(input$seltimevar))
    laby <- sjlabelled::get_label(dfsub[[input$responsevar]], def.value = unique(input$responsevar))
    
    if(input$groupbox == TRUE) {
      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      # remove missing groups from plot
      dfsub <- filter(dfsub, !is.na(!!sym(input$groupbyvar))) 
      
      labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))
      
      ggtitle <- reactive({ paste("Plot of",laby,"versus",labx,"Grouped By",labz,"for",unique(dfsub$PARAM)) })
      
      switch(input$errorBars,
             "1" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$groupbyvar), !!sym(input$seltimevar)) %>%
                 summarise(SummStat = mean(get(input$responsevar),na.rm = TRUE), SD = sd(get(input$responsevar),na.rm = TRUE), Nsubj = n()) %>%
                 mutate(ymin = SummStat - SD/sqrt(Nsubj), ymax = SummStat + SD/sqrt(Nsubj)) %>%
                 ungroup()
               
               yvals <- "Mean values (+/- SE)"
             },
             "2" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$groupbyvar), !!sym(input$seltimevar)) %>%
                 summarise(SummStat = median(get(input$responsevar),na.rm = TRUE), 
                           Q1 = quantile(get(input$responsevar), 0.25, na.rm = TRUE, type=1),
                           Q3 = quantile(get(input$responsevar), 0.75, na.rm = TRUE, type=1),
                           Nsubj = n()) %>%
                 mutate(ymin = Q1, ymax = Q3) %>%
                 ungroup()
               
               yvals <- "Median values (+/- IQR)"
             },
             stop("invalid errorBars button: ",input$errorBars)
      )
      
      
      # Now the response var is no longer AVAL, but SummStat
      input_responsevar  <- deparse(substitute(SummStat))
      
      p <- ggplot(dferr,
                  aes(x = !!sym(input$seltimevar), y = !!sym(input_responsevar), group = !!sym(input$groupbyvar), color = fct_rev(!!sym(input$groupbyvar)))) 
      
      p <- p + 
        suppressWarnings(geom_point(position = 'identity', na.rm = TRUE,
                                    aes(text = paste0(input$groupbyvar, ": ",get(input$groupbyvar),
                                                      "<br>",input$seltimevar,": ",get(input$seltimevar),
                                                      "<br>",input_responsevar,": ",get(input_responsevar)))))
    } else {
      
      ggtitle <- reactive({ paste("Plot of",laby,"versus",labx,"for",unique(dfsub$PARAM)) })
      
      switch(input$errorBars,
             "1" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$seltimevar)) %>%
                 summarise(SummStat = mean(get(input$responsevar),na.rm = TRUE), SD = sd(get(input$responsevar),na.rm = TRUE), Nsubj = n()) %>%
                 mutate(ymin = SummStat - SD/sqrt(Nsubj), ymax = SummStat + SD/sqrt(Nsubj)) %>%
                 ungroup()
               
               yvals <- "Mean values (+/- SE)"
             },
             "2" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$seltimevar)) %>%
                 summarise(SummStat = median(get(input$responsevar),na.rm = TRUE), 
                           Q1 = quantile(get(input$responsevar), 0.25, na.rm = TRUE, type=1),
                           Q3 = quantile(get(input$responsevar), 0.75, na.rm = TRUE, type=1),
                           Nsubj = n()) %>%
                 mutate(ymin = Q1, ymax = Q3) %>%
                 select(-Q1, -Q3) %>%
                 ungroup()
               
               yvals <- "Median values (+/- IQR)"
             },
             stop("invalid errorBars button: ",input$errorBars)
      )
      
      
      # Now the response var is no longer AVAL, but SummStat
      input_responsevar  <- deparse(substitute(SummStat))
      
      p <- ggplot(dferr,
                  aes(x = !!sym(input$seltimevar), y = !!sym(input_responsevar), group = 1 )) 
      
      p <- p +
        suppressWarnings(geom_point(position = 'identity', alpha = 0.2, na.rm = TRUE,
                                    aes(text = paste0(input$seltimevar,": ",get(input$seltimevar),
                                                      "<br>",input_responsevar,": ",get(input_responsevar)))))
    }
    
    p <- p +
      geom_errorbar(aes(ymin = ymin, ymax = ymax), na.rm = TRUE) +
      geom_line()
    
    # add ULN, if available
    if (!is.na(unique(dfsub$ULN))) {
      yint <- unique(dfsub$ULN)
      
      p <- p +
        geom_hline(yintercept = yint, linetype =2 ) +   # x-value is 3rd from the rightmost
        geom_text(aes(label = "ULN", y = yint - 0.05, x = sort(unique(!!sym(input$seltimevar)), decreasing = TRUE)[4] ))
    }
    
    # update scale_y_continuous
    fr <- round(min(dferr$ymin, na.rm = TRUE)*0.98)
    to <- round(max(dferr$ymax,dfsub$ULN,na.rm = TRUE)*1.02)
    
    # set scale_y_continuous if the range is large enough
    if (to - fr > 12) {
      p <- p +
        scale_y_continuous(limits = c(fr, to), breaks = seq(fr, to, (to - fr)/12))
    }
    
    # light theme
    p <- p + theme_light()
    # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    nlevs <- nlevels(factor(dfsub[[input$groupbyvar]]))
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
    p <- p + scale_fill_manual(values = mycolors) 
    
    # Discrete x-axis
    if (input$DiscrXaxis == TRUE) {
      p <- p + scale_x_discrete(limits=c(sort(unique(dfsub[[input$seltimevar]]))))
    }
    
    p <- p + 
      labs(title = ggtitle(), x = labx, y = paste(yvals,unique(dferr$PARAM)))
    
    # workaround to remove "(" and "1)" from legend
    # Now, the workaround:
    # ------------------------------------------------------
    
    p1 <-  ggplotly(p, tooltip = "text")  %>%
      # ref https://plot.ly/r/reference/#layout-legend  {x,y} range from -2 to 3
      layout( legend = list(orientation = "h",  xanchor = "center", yanchor = "bottom", x=0.5, y=-0.35))

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
    req(!is_empty(input$seltimevar) && input$seltimevar != "")
    req(!is_empty(input$responsevar) && input$responsevar != "")
    
    if(input$groupbox == TRUE) {
      req(!is_empty(input$groupbyvar) && input$groupbyvar != "")
      
      switch(input$errorBars,
             "1" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$groupbyvar), !!sym(input$seltimevar)) %>%
                 summarise(SummStat = mean(get(input$responsevar),na.rm = TRUE), SD = sd(get(input$responsevar),na.rm = TRUE), Nsubj = n()) %>%
                 mutate(ymin = SummStat - SD/sqrt(Nsubj), ymax = SummStat + SD/sqrt(Nsubj)) 
               
               yvals <- "Mean values (+/- SE)"
             },
             "2" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$groupbyvar), !!sym(input$seltimevar)) %>%
                 summarise(SummStat = median(get(input$responsevar),na.rm = TRUE), 
                           Q1 = quantile(get(input$responsevar), 0.25, na.rm = TRUE, type=1),
                           Q3 = quantile(get(input$responsevar), 0.75, na.rm = TRUE, type=1),
                           Nsubj = n()) %>%
                 mutate(ymin = Q1, ymax = Q3) 
               
               yvals <- "Median values (+/- IQR)"
             },
             stop("invalid errorBars button: ",input$errorBars)
      )
      
    } else {
      
      switch(input$errorBars,
             "1" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$seltimevar)) %>%
                 summarise(SummStat = mean(get(input$responsevar),na.rm = TRUE), SD = sd(get(input$responsevar),na.rm = TRUE), Nsubj = n()) %>%
                 mutate(ymin = SummStat - SD/sqrt(Nsubj), ymax = SummStat + SD/sqrt(Nsubj)) 
               
               yvals <- "Mean values (+/- SE)"
             },
             "2" = {
               dferr <- dfsub %>%
                 group_by(PARAM,PARAMCD, !!sym(input$seltimevar)) %>%
                 summarise(SummStat = median(get(input$responsevar),na.rm = TRUE), 
                           Q1 = quantile(get(input$responsevar), 0.25, na.rm = TRUE, type=1),
                           Q3 = quantile(get(input$responsevar), 0.75, na.rm = TRUE, type=1),
                           Nsubj = n()) %>%
                 mutate(ymin = Q1, ymax = Q3) 
               
               yvals <- "Median values (+/- IQR)"
             },
             stop("invalid errorBars button: ",input$errorBars)
      )
      
    }
    
    tableout <- dferr
    
    DT::datatable(tableout, options = list(dom = 'ftp', pageLength = 10))
    
  })
  
}
    
## To be copied in the UI -- erased
# mod_popExpMeans_ui("popExpMeans_ui_1")
    
## To be copied in the server -- done
# callModule(mod_popExpMeans_server, "popExpMeans_ui_1")
 
