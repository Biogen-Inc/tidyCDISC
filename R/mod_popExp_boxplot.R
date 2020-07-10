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
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = NULL),
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
    paramcd <- unique(data()$PARAMCD)
    
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
    req(data())
    if (input$yvar %in% colnames(data())) {
        p <- ggplot2::ggplot(data()) + 
          ggplot2::aes_string(x = input$group, y = input$yvar) +
          ggplot2::geom_boxplot()
    } else {
        p <- data() %>% 
          dplyr::filter(PARAMCD == input$yvar) %>%
          ggplot2::ggplot() +
          ggplot2::aes_string(x = input$group, y = input$value) +
          ggplot2::geom_boxplot()
    }
    
    p <- p + 
      ggplot2::xlab("") +
      ggplot2::theme(text = element_text(size = 20),
                     axis.text.x = element_text(size = 20),
                     axis.text.y = element_text(size = 20)) +
      ggplot2::theme_bw()
    
    if (input$points) { p <- p + ggplot2::geom_jitter() }
    return(p)
  })
  
  # return the plot object to parent module
  return(p)
}