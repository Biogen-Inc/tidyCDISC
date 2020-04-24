PopuExpl2Spag <- function(input, output, session, dataselected, df){
  
  ns <- session$ns
  
# Spaghetti Plot
widgets <- c("selPrmCode","groupbox","groupbyvar","seltimevar","responsevar","animate","animateby")

# show all the widgets using an anonymous function
map(widgets, function(x) shinyjs::show(x))

dfsub <- NULL  # assign dfsub in function environment
makeReactiveBinding("dfsub")

# find numeric and date variables
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
is.date <- function(x) inherits(x, 'Date')
dat <- sort(names(df()[ , which(sapply(df(),is.date  ))])) # all date

# restrict seltimevar to AVISIT, AVISITN, VSDY
seltime <- select(df(), ends_with("DY"), starts_with("AVIS"))

updateSelectInput(session = session, inputId = "seltimevar",  choices = c("",sort(names(seltime))), selected = "")

updateSelectInput(session = session, inputId = "groupbyvar", choices = c("USUBJID","SUBJID"), selected = "USUBJID")

updateSelectInput(session = session, inputId = "responsevar", 
                  choices = c("",sort(names(select(df(),ends_with("BL"),any_of(c("AVAL","BASE","CHG")))))), selected = "")

updateSelectInput(session = session, inputId = "animateby", choices = c("",sort(c(num,dat))))

# set checkbox to FALSE
updateCheckboxInput(session = session, inputId = "animate", value = FALSE)

# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != "") 
  
  # subset data based on Parameter Code selection
  dfsub <<- filter(df(),PARAMCD == input$selPrmCode) # superassignment operator

}, ignoreInit = TRUE) # observeEvent(input$selPrmCode

# set default animateby var whenever seltimevar changes
observeEvent(input$seltimevar, {
  updateSelectInput(session, "animateby", selected = input$seltimevar)
})

output$PlotlyOut <- renderPlotly({
  
  req(input$selPrmCode != "") 
  req(!is.null(dfsub))
  
  # Wait for variables
  req(!is_empty(input$seltimevar) && input$seltimevar != "")
  req(!is_empty(input$responsevar) && input$responsevar != "")

  labx <- sjlabelled::get_label(dfsub[[input$seltimevar]])
  laby <- sjlabelled::get_label(dfsub[[input$responsevar]])
  
  # build subset of df() by randomly taking 25 subjects
  if (input$groupbyvar %in% c("SUBJID","USUBJID")) {
    message("Spaghetti plot module subsetting data for 25 subjects")
    set.seed(12345)
    # fix for data frames with less than 25 subjects
    subjs <- distinct(dfsub, USUBJID)
    dfsub <<- semi_join(dfsub, dplyr::sample_n(subjs,    min(nrow(subjs),25), by = "USUBJID"))
    # dfsub <- inner_join(dfsub, dplyr::sample_n(distinct(dfsub, USUBJID), 25), by = "USUBJID") 
  }
  
  if (input$groupbox == TRUE) {
    
    # remove missing groups from plot
    dfsub <- filter(dfsub, !is.na(!!sym(input$groupbyvar))) 
    
    # set def.value to use name if the variable has no label attribute
    labz <- sjlabelled::get_label(dfsub[[input$groupbyvar]], def.value = unique(input$groupbyvar))
    
    ggtitle <- reactive({ paste("Plot of",laby,"over",labx,"for",unique(dfsub$PARAM),"by",labz) })
  } else {
    ggtitle <- reactive({ paste("Plot of",laby,"over",labx,"for",unique(dfsub$PARAM) ) })
  }


  if (input$animate == TRUE) {
    req(input$animateby != "")
    
    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }
    
    dfsuby <- dfsub %>%
      accumulate_by(~get(input$seltimevar))
    
    p <- dfsuby %>%
      plot_ly(
        x = ~get(input$seltimevar), 
        y = ~get(input$responsevar), 
        color = ~get(input$groupbyvar),  
        frame = ~frame, 
        text = ~get(input$groupbyvar), 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'lines+markers',
        line = list(simplyfy = F)
      ) %>%
      animation_opts(
        frame = 100, 
        transition = 0, 
        redraw = FALSE
      ) %>%
      layout(
        title = ggtitle(),
        xaxis = list(title = labx), yaxis = list(title = laby), 
        showlegend = TRUE,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use left of legend as anchor
                      yanchor = "bottom",
                      x = 0.5)             # put legend in center of x-axis
      ) %>%
      animation_slider(
        currentvalue = list(prefix = paste0(input$animateby,": "), font = list(color="red"))
      )
    p 
    
  } else {
    
    p <- ggplot(dfsub,
                aes(x = !!sym(input$seltimevar), y = !!sym(input$responsevar), 
                group = !!sym(input$groupbyvar), fill = !!sym(input$groupbyvar), color = !!sym(input$groupbyvar) )) +
      geom_line(na.rm = TRUE) 
    
    p <- p + suppressWarnings(geom_point(na.rm = TRUE, 
                             aes(text =
                             paste0(USUBJID,
                            "<br>",input$seltimevar,": ",get(input$seltimevar),
                            "<br>",input$responsevar,": ",round(get(input$responsevar),2)
                                               )
                                         ))) # aes, geom_point, suppressWarnings
    
    p <- p +
      labs(x = labx, y = laby, title =  ggtitle()) +
      theme_classic()
    
    # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    nlevs <- nlevels(factor(dfsub[[input$groupbyvar]]))
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
    p <- p + scale_fill_manual(values = mycolors) 

    ggplotly(p, tooltip = "text")  %>%
      # ref https://plot.ly/r/reference/#layout-legend  {x,y} range from -2 to 3
      layout( legend = list(orientation = "h",  xanchor = "center", yanchor = "bottom", x=0.5, y=-0.35),
      title = ggtitle())
    
  }
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$selPrmCode != "") 
  req(!is.null(dfsub))
  
  # Wait for variables
  req(!is_empty(input$seltimevar)  && input$seltimevar  != "")
  req(!is_empty(input$responsevar) && input$responsevar != "")

  tableout <- dfsub %>%
    dplyr::select(!!sym(input$groupbyvar), !!sym(input$seltimevar), !!sym(input$responsevar))
  
  DT::datatable(tableout, 
                options = list(dom = 'tp', 
                               pageLength = 10
                ),
                colnames = c('Row' = 1, 'Group_var' = 2, 'Time Point (x)' = 3, 'Response (y)' = 4))
  
})

}