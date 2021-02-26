#' Kaplan-Meier UI
#'
#' This module contains the widgets needed to create
#' a KM Curve
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#'  

km_ui <- function(id, label = "km") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      selectInput(ns("yvar"), "Select param", choices = NULL),
      selectInput(ns("resp_var"), "Response Variable", choices = "AVAL",selected = "AVAL"),
      selectInput(ns("group"), "Group By", choices = "NONE", selected = "NONE"),
      checkboxInput(ns("points"), "Mark censored observations?", value = TRUE),
      checkboxInput(ns("ci"), "Include 95% confidence interval?", value = FALSE)
    )
  )
}


#' Kaplan-Meier Curve Server Function
#'
#' Using the widgets from the km UI create
#' a ggplot object which is returned to the 
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
km_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  observe({
    req(data())
    
    # get unique paramcd
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    updateSelectInput(session, "yvar",
                      choices = list(paramcd),
                      selected = isolate(input$yvar)
    )
    
    # update response variable options for user based on data
    my_vars <- data() %>% 
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>% 
      select_if(~!all(is.na(.))) %>%# remove NA cols
      colnames()
    updateSelectInput (
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
  })
  
  observeEvent(input$yvar, {
    req(input$yvar != "")
    
    # yvar paramcd
    group_dat <- data() %>% 
      dplyr::filter(PARAMCD == input$yvar) %>% 
      select_if(~!all(is.na(.))) # remove NA cols
    
    # update response variable options for user based on data filtered to a
    # certain param
    my_vars <- data() %>%
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>%
      colnames()
    updateSelectInput (
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
    
    # character and factor columns for grouping or faceting (separating)
    char_col <- subset_colclasses(group_dat, is.character)
    fac_col <- subset_colclasses(group_dat, is.factor)
    group <- sort(c(fac_col, char_col))
    
    # remove some variables...
    grp <- group[!(group %in% c("data_from", "PARAM", "PARAMCD", "USUBJID"))]
                  
    # populate dropdowns with choices
    updateSelectInput(session, "group",
                      choices = c("NONE", grp),
                      selected = isolate(input$group))
    
  })
  
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(data(), input$yvar)
    IDEA_km_curve(data(), 
                 input$yvar, 
                 # input$resp_var, 
                 input$group,
                 input$points,
                 input$ci
                 )
  })
  
  return(p)
}