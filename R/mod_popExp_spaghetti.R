#' Spaghetti Plot UI
#'
#' This module contains the widgets needed to create
#' a spaghetti plot
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#'  
spaghettiPlot_ui <- function(id, label = "spaghetti") {
  ns <- NS(id)
  tagList(
  h4("Select axes:"),
  wellPanel(
    selectInput(ns("yvar"), "Select y-axis", choices = NULL),
    fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
    selectInput(ns("time"), "Time Variable", choices = NULL)
    )
  )
}

#' Spaghetti Plot Server Function
#'
#' Using the widgets from the spaghetti plot UI
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
spaghettiPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(data())
    
    # get time based column names
    seltime_init <- sort(colnames(dplyr::select(data(), ends_with("DY"), contains("VIS"))))
    
    # numeric columns, remove aval, chg, base
    # then remove the x-axis selectors
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"]
    num_col <- sort(c(setdiff(seltime_init, num_col), setdiff(num_col, seltime_init)))
    
    # add paramcds to y-axis options
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    
    updateSelectInput(session, "yvar",
                      choices = list(`Time Dependent` = paramcd,`Time Independent` = num_col),
                      selected = isolate(input$yvar))
    
    # Update time variable based on yvar selection
    if(input$yvar != "" & !(input$yvar %in% colnames(data()))){
      seltime <- data() %>%
        dplyr::filter(PARAMCD == input$yvar) %>% # subset data
        select_if(~!all(is.na(.))) %>%
        dplyr::select(ends_with("DY"), contains("VIS")) %>% # grab time vars remaining
        colnames() %>% sort()
    } else {
      seltime <- seltime_init
    }
    updateSelectInput(session, "time", choices = seltime, selected = isolate(input$time))
  })
  
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value"), "Value",
                                    choices = c("AVAL", "CHG", "BASE"),
                                    selected = isolate(input$value)
                                    )
  })
  
  
  # -------------------------------------------------
  # Create boxplot using inputs
  # -------------------------------------------------
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(data(), input$yvar, input$time)
    IDEA_spaghettiplot(data(), input$yvar, input$time, input$value)
  })
  
  # return the plot object to parent module
  return(p)
}