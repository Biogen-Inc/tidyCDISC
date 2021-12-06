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
#' @noRd
#'  
km_ui <- function(id, label = "km") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar"), "Select param", choices = NULL)),
        column(6, selectInput(ns("resp_var"), "Response Variable", choices = "AVAL",selected = "AVAL"))
      ),
      fluidRow(
        column(6, selectInput(ns("group"), "Group By", choices = "NONE", selected = "NONE")),
        column(6, selectInput(ns("cnsr_var"), "Censor Variable (0,1)", choices = "CNSR", selected = "CNSR"))
      ),
      shinyWidgets::materialSwitch(ns("points"), h6("Mark censored observations?"),
                                   status = "primary", value = TRUE),
      shinyWidgets::materialSwitch(ns("ci"), h6("Include 95% confidence interval?"),
                                   status = "primary", value = FALSE)
      # checkboxInput(ns("points"), "Mark censored observations?", value = TRUE),
      # checkboxInput(ns("ci"), "Include 95% confidence interval?", value = FALSE)
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
#' @noRd
#'  
km_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  observe({
    req(data())
    
    # get unique paramcd
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    updateSelectInput(session, "yvar",
                      choices = as.list(paramcd),
                      selected = isolate(input$yvar)
    )
  })
  
    
  # update response variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(input$yvar != "")
    
    d <- data()
    my_vars <- d %>% 
      dplyr::filter(PARAMCD == input$yvar) %>% 
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>% 
      select_if(~!all(is.na(.))) %>% # remove NA cols
      colnames()
    
    updateSelectInput (
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
  })
  
  # update censor variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(input$yvar != "")
    
    # col <- c(1:4)
    # col <- c(0, 1, 1, 1)
    # col <- c(1, 1, 1)
    
    d0 <- data()
    my_cvars <- d0 %>% 
      dplyr::filter(PARAMCD == input$yvar) %>% 
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(where(is.numeric)) %>%
      select(where(function(col) all( unique(col) %in% c(0,1) ))) %>%
      select_if(~!all(is.na(.))) %>% # remove NA cols
      colnames()
    
    updateSelectInput (
      session = session,
      inputId = "cnsr_var",
      choices = c("CNSR", my_cvars[my_cvars != "CNSR"]),
      selected = isolate(input$cnsr_var)
    )
  })
  
  
  observeEvent(input$yvar, {
    req(input$yvar != "")
    
    # yvar paramcd
    group_dat <- data() %>% 
      dplyr::filter(PARAMCD == input$yvar) %>% 
      select_if(~!all(is.na(.))) # remove NA cols
    
    # character and factor columns for grouping or faceting (separating)
    char_col <- subset_colclasses(group_dat, is.character)
    fac_col <- subset_colclasses(group_dat, is.factor)
    group <- sort(c(fac_col, char_col))
    # print("char_col:")
    # print(char_col)
    # print("fac_col:")
    # print(fac_col)
    # print(".")
    # print(".")
    
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
    req(data(), input$yvar )
    #, input$resp_var,input$group,input$points,input$ci) # can't include these in req
    app_km_curve(data(), 
                 input$yvar, 
                 input$resp_var,
                 input$cnsr_var,
                 input$group,
                 input$points,
                 input$ci
                 )
  })
  
  return(p)
}
