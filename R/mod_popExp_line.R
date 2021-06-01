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
linePlot_ui <- function(id, label = "line") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar"), "Select y-axis", choices = NULL)),
        column(6, align = "center", uiOutput(ns("include_var")))
      ),
      fluidRow(
        column(6, selectInput(ns("time"), "Time Variable", choices = NULL)),
        conditionalPanel("input.yvar && input.time", ns = ns,
           column(6, shinyWidgets::materialSwitch(ns("add_line"), 
              h6("Overlay static line"), status = "primary", value = F))
        )
      )
      , conditionalPanel("input.add_line", ns = ns,
        fluidRow(
           column(6,# uiOutput(ns("add_vert_ui"))
                  selectInput(ns("add_vert"), "Vertical line's x-intercept:",
                    choices = "NONE", selected = "NONE")
                  ),
           column(6, # uiOutput(ns("add_hor_ui"))
                  selectInput(ns("add_hor"), "Horizontal line's y-intercept:",
                    choices = "NONE", selected = "NONE")
                  )
        )
      )
    ),
    h4("Group data:"),
    wellPanel(
      selectInput(ns("color"), "Color Plots By", choices = "NONE", selected = "NONE"),
      selectInput(ns("separate"), "Separate Plots By", choices = "NONE", selected = "NONE")
    )
    , h4("Options:"),
    wellPanel(
      shinyWidgets::materialSwitch(ns("err_bars"), h6("Display 95% CI"),
                                                       status = "primary", value = F),
      fixedRow(
        column(4, shinyWidgets::materialSwitch(ns("label_points"), 
           h6("Label points:"),status = "primary", value =  F)),
        conditionalPanel("input.label_points", ns = ns,
           column(4, selectInput(ns("gtxt_x_pos"), "Label x position:",
             choices = c("left", "middle", "right"), selected = "middle")),
           column(4, selectInput(ns("gtxt_y_pos"), "Label y position:",
             choices = c("bottom", "middle", "top"), selected = "top"))
        )
      )
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
linePlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(data())
    
    # yvar cannot be from ADAE since that data has no visit var
    d <- data() %>% filter(data_from != "ADAE")
    
    # get time based column names
    seltime_init <- sort(colnames(dplyr::select(d, ends_with("DY"), contains("VIS"))))
    
    # numeric columns, remove aval, chg, base
    # then remove the x-axis selectors
    num_col <- subset_colclasses(d, is.numeric)
    num_col <- num_col[!(num_col %in% c("AVAL", "CHG", "BASE", seltime_init))]
    num_col <- num_col[substr(num_col, 1, 2) != "AE"]
    # num_col <- sort(c(setdiff(seltime_init, num_col), setdiff(num_col, seltime_init)))
    
    # add paramcds to y-axis options
    paramcd <- sort(na.omit(unique(d$PARAMCD)))
    
    updateSelectInput(session, "yvar",
                      choices = list(`Time Dependent` = paramcd,`Time Independent` = num_col),
                      selected = isolate(input$yvar))
    
    # Update time variable based on yvar selection
    if(input$yvar != "" & !(input$yvar %in% colnames(d))){
      seltime <- d %>%
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
                                    choices = c("AVAL", "CHG"),
                                    selected = isolate(input$value)
                                    )
  })
  
  observe({
    # output$add_vert_ui <- renderUI({
    # output$add_hor_ui <- renderUI({

    if(input$add_line){
      d <- data()

      if(input$yvar != "" & !(input$yvar %in% colnames(d))){
        sel_d <- d %>% dplyr::filter(PARAMCD == input$yvar) #%>% select_if(~!all(is.na(.)))
        sel_y_vals <- sel_d %>% select(input$value) %>% distinct() %>% pull()
      } else {
        sel_d <- d
        sel_y_vals <- sel_d %>% select(input$yvar) %>% distinct() %>% pull()
      }
      
      sel_time_vals <- sel_d %>% select(input$time) %>% distinct() %>% pull()
      
      # add_vert
      updateSelectInput(session, "add_vert", choices = c("NONE", sel_time_vals),
        selected = ifelse(isolate(input$add_vert) %in% sel_time_vals,
                          isolate(input$add_vert), "NONE"))
      
      # add_hor
      updateSelectInput(session, "add_hor", choices = c("NONE", sel_y_vals),
                        selected = ifelse(isolate(input$add_hor) %in% sel_y_vals,
                                          isolate(input$add_hor), "NONE"))
    }
  })
  
  observeEvent(list(input$yvar), {
    req(input$yvar != "")
    
    # Update grouping variable based on yvar selection
    if(!(input$yvar %in% colnames(data())) ){ # yvar paramcd #& input$xvar %in% colnames(data())
      group_dat <- data() %>% 
        dplyr::filter(PARAMCD == input$yvar) %>% 
        select_if(~!all(is.na(.))) # remove NA cols
    } else {
      group_dat <- data()
    } 
    
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
  
  # -------------------------------------------------
  # Create plot using inputs
  # -------------------------------------------------
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(data(), input$yvar, input$time)
    IDEA_lineplot(data(), input$yvar, input$time, input$value, input$separate, input$color,
      input$err_bars, input$label_points, input$gtxt_x_pos , input$gtxt_y_pos)
  })
  
  # return the plot object to parent module
  return(p)
}