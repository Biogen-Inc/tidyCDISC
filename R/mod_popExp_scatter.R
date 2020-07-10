scatterPlot_ui <- function(id, label = "scatter") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(ns("xvar"), "Select x-axis", choices = NULL)),
      column(6, selectInput(ns("week_x"), "Select Week", choices = NULL))
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("include_xvar")))),
    
    fluidRow(
      column(6, selectInput(ns("yvar"), "Select y-axis", choices = NULL)),
      column(6, selectInput(ns("week_y"), "Select Week", choices = NULL))
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("include_yvar")))),
    
    selectInput(ns("separate"), "Separate Plots By", choices = NULL),
    selectInput(ns("color"), "Color Plots By", choices = NULL)
  )
}

scatterPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  observe({
    req(data())
    
    # numeric columns, remove aval, chg, base
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"]
    
    # get unique paramcd
    paramcd <- unique(data()$PARAMCD)
    
    updateSelectInput(session, "yvar", choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col))
    updateSelectInput(session, "xvar", choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col))
  })
  
  output$include_yvar <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_y"), "Value", choices = c("AVAL", "CHG", "BASE"))
  })
  
  output$include_xvar <- renderUI({
    req(input$xvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_x"), "Value", choices = c("AVAL", "CHG", "BASE"))
  })
  
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes_string(x = "AGE", y = "AGE")) +
      ggplot2::geom_point()
  })
  
  return(p)
}