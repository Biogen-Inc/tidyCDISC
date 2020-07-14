#' Scatter Plot UI
#'
#' This module contains the widgets needed to create
#' a scatter plot
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#'  

scatterPlot_ui <- function(id, label = "scatter") {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Select x-axis:"),
      fluidRow(
        column(6, selectInput(ns("xvar"), "Explanatory Variable", choices = NULL)),
        column(6, conditionalPanel(
          condition = "output.is_x_week", ns = ns,
          selectInput(ns("week_x"), "Select Week", choices = NULL)))
      ),
      fluidRow(column(12, align = "center", uiOutput(ns("include_xvar"))))
    ),
    wellPanel(
      h4("Select y-axis:"),
      fluidRow(
        column(6, selectInput(ns("yvar"), "Response Variable", choices = NULL)),
        column(6, conditionalPanel(
          condition = "output.is_y_week", ns = ns,
          selectInput(ns("week_y"), "Select Week", choices = NULL)))
      ),
      fluidRow(column(12, align = "center", uiOutput(ns("include_yvar"))))
    ),
    wellPanel(
      selectInput(ns("separate"), "Separate Plots By", choices = NULL),
      selectInput(ns("color"), "Color Plots By", choices = NULL)
    )
  )
}


#' Scatter Plot Server Function
#'
#' Using the widgets from the scatter plot UI
#' create a ggplot object which is returned to the 
#' parent Population Explorer module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param data The combined dataframe from population explorer
#'
#' @import shiny
#' @import dplyr
#'
#' @return ggplot object
#'
#' @family popExp Functions
#'  
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
    
    # character and factor columns for coloring or separating
    char_col <- subset_colclasses(data(), is.character)
    fac_col <- subset_colclasses(data(), is.factor)
  
    updateSelectInput(session, "separate", choices = c("NONE", fac_col, char_col))
    updateSelectInput(session, "color", choices = c("NONE", fac_col, char_col))
  })
  
  output$include_yvar <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_y"), "Value", choices = c("AVAL", "CHG", "BASE"))
  })
  
  output$include_xvar <- renderUI({
    req(input$xvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_x"), "Value", choices = c("AVAL", "CHG", "BASE"))
  })
  
  weeks_list <- reactive({
    unique(data() %>% select(AVISIT) %>% filter(AVISIT != "") %>% pull(AVISIT))
  })
  
  observeEvent(input$xvar, {
    if(!input$xvar %in% colnames(data()))
      updateSelectInput(session, "week_x", choices = weeks_list(), selected = weeks_list()[1])
  })
  
  observeEvent(input$yvar, {
    if(!input$yvar %in% colnames(data()))
      updateSelectInput(session, "week_y", choices = weeks_list(), selected = weeks_list()[1])
  })
  
  output$is_x_week <- reactive(!input$xvar %in% colnames(data()))
  output$is_y_week <- reactive(!input$yvar %in% colnames(data()))
  
  outputOptions(output, "is_x_week", suspendWhenHidden = FALSE) 
  outputOptions(output, "is_y_week", suspendWhenHidden = FALSE) 
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(data(), input$yvar, input$xvar, input$week_x, input$value_x, input$week_y, input$value_y, input$separate, input$color)
    IDEA_scatterplot(data(), 
                 input$yvar, 
                 input$xvar, 
                 input$week_x, 
                 input$value_x, 
                 input$week_y, 
                 input$value_y, 
                 input$separate, 
                 input$color)
  })
  
  return(p)
}