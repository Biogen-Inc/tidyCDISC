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
    fluidRow(
      column(6, selectInput(ns("xvar"), "Explanatory Variable", choices = NULL)),
      column(6, conditionalPanel(
        condition = "output.is_x_week", ns = ns,
        selectInput(ns("week_x"), "Select Week", choices = NULL)))
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("include_xvar")))),
    
    fluidRow(
      column(6, selectInput(ns("yvar"), "Response Variable", choices = NULL)),
      column(6, conditionalPanel(
        condition = "output.is_y_week", ns = ns,
        selectInput(ns("week_y"), "Select Week", choices = NULL)))
    ),
    fluidRow(column(12, align = "center", uiOutput(ns("include_yvar")))),
    
    selectInput(ns("separate"), "Separate Plots By", choices = NULL),
    selectInput(ns("color"), "Color Plots By", choices = NULL)
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
    req(data())
    # x and y are numeric columns
    if (input$yvar %in% colnames(data()) & input$xvar %in% colnames(data())) {
      p <- ggplot2::ggplot(data()) + 
        ggplot2::aes_string(x = input$xvar, y = input$yvar) +
        ggplot2::geom_point()
      # y numeric, x is paramcd 
    } else if (input$yvar %in% colnames(data()) & !input$xvar %in% colnames(data())) {
      p <- data() %>% 
        dplyr::filter(PARAMCD == input$xvar) %>%
        dplyr::filter(AVISIT == input$week_x) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = input$value_x, y = input$yvar) +
        ggplot2::geom_line() +
        ggplot2::geom_point()
      # x numeric, y paramcd
    } else if (!input$yvar %in% colnames(data()) & input$xvar %in% colnames(data())) {
      p <- data() %>% 
        dplyr::filter(PARAMCD == input$yvar) %>%
        dplyr::filter(AVISIT == input$week_y) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = input$xvar, y = input$value_y) +
        ggplot2::geom_line() +
        ggplot2::geom_point()
      # both paramcds
    } else {
      p <- data() %>%
        dplyr::filter(PARAMCD == input$yvar | PARAMCD == input$xvar) %>%
        dplyr::filter(AVISIT == input$week_y | AVISIT == input$week_x) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = input$value_x, y = input$value_y) +
        ggplot2::geom_line() +
        ggplot2::geom_point()
    }
    
    p <- p + 
      ggplot2::theme(text = element_text(size = 20),
                     axis.text = element_text(size = 20)) +
      ggplot2::theme_bw()
    
    if (input$separate != "NONE") { p <- p + ggplot2::facet_wrap(as.formula(paste(".~", input$separate))) }
    if (input$color != "NONE") { p <- p + ggplot2::aes_string(color = input$color)}
    return(p)
  })
  
  return(p)
}