PopuExpl2Spag <- function(input, output, session, dataselected, df){
  
  ns <- session$ns
  
# Spaghetti Plot
shinyjs::show(id="selPrmCode")
shinyjs::show(id="splitbox")
shinyjs::show(id="splitbyvar")
shinyjs::hide(id="selxvar")
shinyjs::hide(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::show(id="seltimevar")
shinyjs::show(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::show(id="animate")
shinyjs::show(id="animateby")
shinyjs::hide(id="numBins")
shinyjs::hide(id="AddLine")
shinyjs::hide(id="AddErrorBar")
shinyjs::hide(id="DiscrXaxis")
shinyjs::hide(id="UseCounts")

# find numeric and date variables
num <- sort(names(df()[ , which(sapply(df(),is.numeric  ))])) # all num
is.date <- function(x) inherits(x, 'Date')
dat <- sort(names(df()[ , which(sapply(df(),is.date  ))])) # all date

# restrict seltimevar to AVISIT, AVISITN, VSDY
seltime <- select(df(), ends_with("DY"), starts_with("AVIS"))

print(paste("seltime variables:",paste(unique(names(seltime)),collapse = ",")))

updateSelectInput(
  session = session,
  inputId = "splitbyvar",
  choices = c(" ",sort(names(select(df(),starts_with("TRT0"),one_of("SUBJID","USUBJID"))))),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "seltimevar",
  choices = c(sort(names(seltime))),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "responsevar",
  choices = c(num),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "animateby",
  choices = c(sort(c(num,dat))),
  selected = " ")

# set checkbox to FALSE
updateCheckboxInput(session = session, inputId = "animate", value = FALSE)

# update subsequent inputselects based on PARAM code selection
observeEvent(input$selPrmCode, {
  
  req(input$selPrmCode != " ") 
  
  # subset data based on Parameter Code selection
  dfsub <- filter(df(),PARAMCD == input$selPrmCode)


# set default animateby var whenever seltimevar changes
observeEvent(input$seltimevar, {
  updateSelectInput(session, "animateby", selected = input$seltimevar)
})

output$PlotlyOut <- renderPlotly({
  
  req(input$selPrmCode  != " ")
  req(input$seltimevar  != " ") 
  req(input$responsevar != " ")
  
  # build subset of df() by randomly taking 25 subjects
  if (input$splitbyvar %in% c("SUBJID","USUBJID")) {
    print("subsetting data for 25 subjects")
    set.seed(12345)
    dfsub <- inner_join(dfsub, dplyr::sample_n(distinct(dfsub, USUBJID), 25), by = "USUBJID") 
  }

  ggtitle <- reactive({ paste("Plot of",input$responsevar,"over",input$seltimevar,"for PARAMCD:",unique(dfsub$PARAMCD),"by",input$splitbyvar) })
  
  if (input$animate == TRUE) {
    req(input$animateby != " ")
    
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
        color = ~get(input$splitbyvar),  
        frame = ~frame, 
        text = ~get(input$splitbyvar), 
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
        xaxis = list(title = input$seltimevar), yaxis = list(title = input$responsevar), 
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
                aes(x = !!as.name(input$seltimevar), y = !!as.name(input$responsevar), 
                    group = !!as.name(input$splitbyvar), fill = !!as.name(input$splitbyvar), color = !!as.name(input$splitbyvar) )) +
      geom_line(na.rm = TRUE) 
    
    p <- p + suppressWarnings(geom_point(na.rm = TRUE, 
                             aes(text =
                             paste0(USUBJID,
                            "<br>",input$seltimevar,": ",get(input$seltimevar),
                            "<br>",input$responsevar,": ",round(get(input$responsevar),2)
                                               )
                                         ))) # aes, geom_point, suppressWarnings
    
    # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    nlevs <- nlevels(factor(dfsub[[input$splitbyvar]]))
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
    p <- p +
      labs(x = input$seltimevar, y = input$responsevar, title =  ggtitle()) +
      scale_fill_manual(values = mycolors) +
      theme_classic()
    
    ggplotly(p, tooltip = "text")  %>%
      # ref https://plot.ly/r/reference/#layout-legend  {x,y} range from -2 to 3
      layout( legend = list(orientation = "h",  xanchor = "center", yanchor = "bottom", x=0.5, y=-0.35),
              title = paste("Plot of",input$responsevar,"over",input$seltimevar,"for PARAMCD:",unique(dfsub$PARAMCD),"grouped by",input$splitbyvar))
    
  }
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$seltimevar  != " ") 
  req(input$responsevar != " ") 
  
  x_var <- as.name(input$seltimevar)
  y_var <- as.name(input$responsevar)
  z_var <- as.name(input$splitbyvar)
  
  tableout <- dfsub %>%
    dplyr::select(!!z_var, !!x_var, !!y_var)
  
  DT::datatable(tableout, 
                options = list(dom = 'tp', 
                               pageLength = 10
                ),
                colnames = c('Row' = 1, 'Group_var' = 2, 'Time Point (x)' = 3, 'Response (y)' = 4))
  
})

}, ignoreInit = FALSE) # observeEvent(input$selPrmCode

}