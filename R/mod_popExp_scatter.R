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
#' @noRd
#'
scatterPlot_ui <- function(id, label = "scatter") {
  ns <- NS(id)
  tagList(
    h4("Select x-axis:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("xvar"), "Explanatory Variable", choices = NULL)),
        column(6, conditionalPanel(
          condition = "output.is_x_week", ns = ns,
          selectInput(ns("week_x"), "Select Week", choices = NULL)))
      ),
      fluidRow(column(12, align = "center", uiOutput(ns("include_xvar"))))
    ),
    h4("Select y-axis:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar"), "Response Variable", choices = NULL)),
        column(6, conditionalPanel(
          condition = "output.is_y_week", ns = ns,
          selectInput(ns("week_y"), "Select Week", choices = NULL)))
      ),
      fluidRow(column(12, align = "center", uiOutput(ns("include_yvar"))))
    ),
    h4("Group data:"),
    wellPanel(
      selectInput(ns("separate"), "Separate Plots By", choices = "NONE", selected = "NONE"),
      selectInput(ns("color"), "Color Plots By", choices = "NONE", selected = "NONE")
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
#' @param run logical, TRUE if select code chunks in this module should execute
#'
#' @import shiny
#' @import dplyr
#'
#' @return ggplot object
#'
#' @family popExp Functions
#' @noRd
#'  
scatterPlot_srv <- function(input, output, session, data, run) {
  ns <- session$ns
  
  observe({
    req(data(), run())
    
    # numeric columns, remove aval, chg, base
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- sort(num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"])
    
    # get unique paramcd
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    
    updateSelectInput(session, "xvar",
                      choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col),
                      selected = isolate(input$xvar)
    )
    updateSelectInput(session, "yvar",
                      choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col),
                      selected = isolate(input$yvar)
    )
  })
  
  # generate list of viable color and separate values based on chosen xvar and yvar
  observeEvent(list(input$xvar, input$yvar), {
    req(input$xvar != "" & input$yvar != "", run())

    # Update grouping variable based on xvar & yvar selection
    if(input$yvar %in% colnames(data()) & input$xvar %in% colnames(data())){ # neither paramcd
      group_dat <- data()

    } else if(!(input$yvar %in% colnames(data())) & input$xvar %in% colnames(data())){ # yvar paramcd
      group_dat <- data() %>%
        dplyr::filter(PARAMCD == input$yvar) %>%
        select_if(~!all(is.na(.))) # remove NA cols

    } else if(input$yvar %in% colnames(data()) & !(input$xvar %in% colnames(data()))){ # xvar paramcd
      group_dat <- data() %>%
        dplyr::filter(PARAMCD == input$xvar) %>%
        select_if(~!all(is.na(.))) # remove NA cols

    } else { # both paramcds
      x_cols <- data() %>%
        dplyr::filter(PARAMCD == input$xvar) %>%
        select_if(~!all(is.na(.))) # remove NA cols

      y_cols <- data() %>%
        dplyr::filter(PARAMCD == input$yvar) %>%
        select_if(~!all(is.na(.))) # remove NA cols

      group_dat <- x_cols %>% full_join(y_cols)
    }

    group_dat <- select(group_dat, -data_from)

    # character and factor columns for coloring or separating
    char_col <- subset_colclasses(group_dat, is.character)
    fac_col <- subset_colclasses(group_dat, is.factor)
    group <- sort(c(fac_col, char_col))

    # populate dropdowns with choices
    updateSelectInput(session, "color",
                      choices = c("NONE", group),
                      selected = isolate(input$color))
    updateSelectInput(session, "separate",
                      choices = c("NONE", group),
                      selected = isolate(input$separate))

  })
  

  
  output$include_yvar <- renderUI({
    req(run(),input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_y"), "Value",
                                    choices = c("AVAL", "CHG", "BASE"),
                                    selected = isolate(input$value_y))
  })
  
  output$include_xvar <- renderUI({
    req(run(),input$xvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value_x"), "Value",
                                    choices = c("AVAL", "CHG", "BASE"),
                                    selected = isolate(input$value_y))
  })
  
  observeEvent(input$xvar, {
    if(!input$xvar %in% colnames(data())){
      x_week_list <- data() %>%
        subset(PARAMCD == input$xvar) %>%
        distinct(AVISIT) %>%
        pull() %>% na.omit()
      updateSelectInput(session, "week_x", choices = x_week_list, selected = x_week_list[1])
    }
  })
  
  observeEvent(input$yvar, {
    if(!input$yvar %in% colnames(data())){
      y_week_list <- data() %>%
        subset(PARAMCD == input$yvar) %>%
        distinct(AVISIT) %>%
        pull() %>% na.omit()
      updateSelectInput(session, "week_y", choices = y_week_list, selected = y_week_list[1])
    }
  })
  
  output$is_x_week <- reactive(!input$xvar %in% colnames(data()))
  output$is_y_week <- reactive(!input$yvar %in% colnames(data()))
  
  outputOptions(output, "is_x_week", suspendWhenHidden = FALSE) 
  outputOptions(output, "is_y_week", suspendWhenHidden = FALSE) 
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(run(),data(), input$yvar, input$xvar)
    app_scatterplot(data(), 
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
