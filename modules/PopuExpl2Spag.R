PopuExpl2Spag <- function(input, output, session, dataselected, df){
  
  ns <- session$ns
  
# Spaghetti Plot
shinyjs::show(id="advstagsem")
shinyjs::hide(id="adsltagsem")
shinyjs::show(id="selPrmCode")
shinyjs::hide(id="bygroup")
shinyjs::hide(id="groupbyvar")
shinyjs::hide(id="selxvar")
shinyjs::hide(id="selyvar")
shinyjs::hide(id="selzvar")
shinyjs::show(id="seltimevar")
shinyjs::show(id="responsevar")
shinyjs::hide(id="AddPoints")
shinyjs::show(id="animate")
shinyjs::show(id="animateby")
shinyjs::hide(id="numBins")

updateSelectInput(
  session = session,
  inputId = "selPrmCode",
  choices = c(" ",unique(df()$PARAMCD)),
  selected = " ")

# set checkbox to FALSE
updateCheckboxInput(session = session, inputId = "animate", value = FALSE)

# build subset ADVSX data if ADVS was selected
if ("ADVS" %in% dataselected()) {
  # ADVSX <- filter(dd$data[["ADVS"]], substr(SUBJID, 1, 3) == "310") # Subjects from SITEID 310
  ADVSX <- filter(df(), substr(SUBJID, 1, 3) == "310") # Subjects from SITEID 310
}    

# find numeric and date variables
numcols2 <- ADVSX[ , which(sapply(ADVSX,is.numeric))]
is.date <- function(x) inherits(x, 'Date')
datcols2 <- ADVSX[ , which(sapply(ADVSX,is.date))]
# restrict seltimevar to AVISIT, AVISITN, VSDY
seltime <- select(ADVSX, ends_with("DY"), starts_with("AVIS"))

updateSelectInput(
  session = session,
  inputId = "seltimevar",
  choices = c(" ",sort(names(seltime))),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "responsevar",
  choices = c(" ",sort(names(numcols2))),
  selected = " ")

updateSelectInput(
  session = session,
  inputId = "animateby",
  choices = c(" ",sort(names(c(numcols2,datcols2)))),
  selected = " ")

# set default animateby var whenever seltimevar changes
observeEvent(input$seltimevar,
             updateSelectInput(session, "animateby", 
                               selected = input$seltimevar))

output$PlotlyOut <- renderPlotly({
  
  req(input$selPrmCode  != " ")
  req(input$seltimevar  != " ") 
  req(input$responsevar != " ")
  
  # filter on paramcd
  ADVSY <- filter(ADVSX, PARAMCD == input$selPrmCode)
  
  ggtitle <- paste("Plot of",input$responsevar,"over",input$seltimevar,"for PARAMCD:",unique(ADVSY$PARAMCD),"by SUBJID")
  
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
    
    ADVSY2 <- ADVSY %>%
      accumulate_by(~get(input$seltimevar))
    
    p <- ADVSY2 %>%
      plot_ly(
        x = ~get(input$seltimevar), 
        y = ~get(input$responsevar), 
        color = ~SUBJID,  # by SUBJID
        frame = ~frame, 
        text = ~USUBJID, 
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
        title = ggtitle,
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
    
    p <- ggplot(ADVSY,
                aes(x = !!as.name(input$seltimevar), y = !!as.name(input$responsevar), group = SUBJID, fill = SUBJID, color = SUBJID )) +
      geom_line(na.rm = TRUE) 
    
    p <- p + suppressWarnings(geom_point(na.rm = TRUE, 
                                         aes(text =
                                               paste0(USUBJID,
                                                      "<br>",input$seltimevar,": ",get(input$seltimevar),
                                                      "<br>",input$responsevar,": ",round(get(input$responsevar),2)
                                               )
                                         ))) # aes, geom_point, suppressWarnings
    
    # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    nlevs <- nlevels(factor(ADVSY$SUBJID))
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nlevs)
    p <- p +
      labs(x = input$seltimevar, y = input$responsevar, title =  ggtitle) +
      scale_fill_manual(values = mycolors) +
      theme_classic()
    
    ggplotly(p, tooltip = "text")  %>%
      # ref https://plot.ly/r/reference/#layout-legend  {x,y} range from -2 to 3
      layout( legend = list(orientation = "h",  xanchor = "center", yanchor = "bottom", x=0.5, y=-0.35),
              title = paste("Plot of",input$responsevar,"over",input$seltimevar,"for PARAMCD:",unique(ADVSY$PARAMCD),"grouped by SUBJID"))
    
  }
  
})

output$DataTable <- DT::renderDataTable({
  
  req(input$seltimevar  != " ") 
  req(input$responsevar != " ") 
  
  x_var <- as.name(input$seltimevar)
  y_var <- as.name(input$responsevar)
  
  # filter on paramcd
  ADVSY <- filter(ADVSX, PARAMCD == input$selPrmCode)
  
  tableout <- ADVSY %>%
    dplyr::select(USUBJID, SUBJID, !!x_var, !!y_var)
  
  DT::datatable(tableout, 
                options = list(dom = 'tp', 
                               pageLength = 10
                ),
                colnames = c('Row' = 1, 'USUBJID' = 2, 'Subject' = 3, 'Time Point (x)' = 4, 'Response (y)' = 5))
  
})
}