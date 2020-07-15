#' Box Plot UI
#'
#' This module contains the widgets needed to create
#' a box plot
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#'  
boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  wellPanel(
    selectInput(ns("yvar"), "Select y-axis", choices = NULL),
    fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}


#' Box Plot Server Function
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
boxPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(data())
    
    # numeric columns, remove aval, chg, base
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"]
    
    # get unique paramcd
    paramcd <- na.omit(unique(data()$PARAMCD))
    
    group_fc <- subset_colclasses(data(), is.factor)
    group_ch <- subset_colclasses(data(), is.character)
    group <- c(group_fc, group_ch)
    group <- group[group != "data_from"]
    
    updateSelectInput(session, "yvar", choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col))
    updateSelectInput(session, "group", choices = group)
  })
  
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG", "BASE"))
  })
  
  
  # -------------------------------------------------
  # Create boxplot using inputs
  # -------------------------------------------------
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(data(), input$yvar, input$group, input$value)
    IDEA_boxplot(data(), input$yvar, input$group, input$value, input$points)
  })
  
  # return the plot object to parent module
  return(p)
}