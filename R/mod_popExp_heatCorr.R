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
heatmap_ui <- function(id, label = "line") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar_x"), "Select Parameter X", choices = NULL, multiple = T)),
        column(6, selectInput(ns("yvar_y"), "Select Parameter Y", choices = NULL, multiple = T))
        
      ),
      # fluidRow(
        # column(6, align = "center", uiOutput(ns("include_var_x"))),
        # column(6, align = "center", uiOutput(ns("include_var_y")))
      # ),
      
    )
    , h4("Options:"),
    wellPanel(
      selectInput(ns("time"), "Group by Visit Variable", choices = "NONE"),
      selectInput(ns("cor_mthd"), "Select correlation coefficient",
                  choices = c("Pearson" = "pearson","Spearman" = "spearman"),
                  selected = "Pearson"
        
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
heatmap_srv <- function(input, output, session, data) {
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
    
    updateSelectInput(session, "yvar_x",
                      choices = list(`Time Dependent` = paramcd,`Time Independent` = num_col),
                      selected = isolate(input$yvar_x))
    updateSelectInput(session, "yvar_y",
                      choices = list(`Time Dependent` = paramcd,`Time Independent` = num_col),
                      selected = isolate(input$yvar_y))
    

  })
  
  # time or by_var
  observeEvent(list(input$yvar_x, input$yvar_y), {
    # req(input$yvar != "")
    req(data(), input$yvar_x, input$yvar_y)
    
    # yvar cannot be from ADAE since that data has no visit var
    d <- data() %>% filter(data_from != "ADAE")
    
    # get time based column names
    seltime_init <- sort(colnames(dplyr::select(d, ends_with("DY"), contains("VIS"))))
    # Update time variable based on yvar selection
    if((any(!(input$yvar_x %in% colnames(d)))) |
       (any(!(input$yvar_y %in% colnames(d))))){
      if(any(!(input$yvar_x %in% colnames(d)))){
        d <- d %>% dplyr::filter(PARAMCD %in% input$yvar_x)
      }
      if(any(!(input$yvar_y %in% colnames(d)))){
        d <- d %>% dplyr::filter(PARAMCD %in% input$yvar_x)
      }
      seltime_dat <- d %>%
        filter(!is.na(AVAL)) %>% # aval is not missing...
        select_if(~!all(is.na(.)))  # grab time vars remaining
      
      seltime <- seltime_dat %>%
        select(ends_with("DY"), contains("VIS")) %>%
        colnames() %>% sort()
    } else {
      seltime <- seltime_init
    }
    updateSelectInput(session, "time", choices = c("NONE", seltime), selected = isolate(input$time))
  })
  # output$include_var <- renderUI({
  #   req(input$yvar %in% data()$PARAMCD)
  #   shinyWidgets::radioGroupButtons(ns("value"), "Value", justified = T,
  #                                   choices = c("AVAL", "CHG"),
  #                                   selected = isolate(input$value)
  #                                   )
  # })
  
  

  
  # -------------------------------------------------
  # Create plot using inputs
  # -------------------------------------------------
  # input <- list(
  #   yvar = "ALB"
  #   ,
  #   time = "VISIT1DT"
  #   ,
  #   value = "AVAL"
  #   ,
  #   separate = "NONE"
  #   ,
  #   color = "NONE"
  #   ,
  #   err_bars = F
  #   ,
  #   label_points = F
  #   ,
  #   gtxt_x_pos = NULL
  #   ,
  #   gtxt_y_pos = NULL
  # )
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p_both <- reactive({
    req(data(), input$yvar_x, input$yvar_y) #, input$time, input$cor_mthd)
    # print(data()[[input$time]])
    pp <- IDEA_heatmap(data() #%>% varN_fctr_reorder2()
                       , input$yvar_x, input$yvar_y, input$time, "AVAL", input$cor_mthd)
    
    # pp <- list(plot = qplot(x = mtcars$wt, y = mtcars$mpg),
    #            data = mtcars[, 4:6])
    return(list(plot = pp$plot, data = pp$data))
  })
  p <- reactive( p_both()$plot )
  p_data <- reactive( p_both()$data )
  
  # return the plot object to parent module
  # return(p)
  return(list(plot = p, #plot_ht = px_ht_num, plot_nm = dwnld_nm,
              plot_data = p_data))
}