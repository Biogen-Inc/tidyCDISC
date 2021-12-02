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
        column(6, selectInput(ns("time"), "Time Variable", choices = NULL))
      )
      ,
      conditionalPanel("input.yvar && input.time", ns = ns,
         fluidRow(
           column(5, shinyWidgets::materialSwitch(ns("add_vert"), h6("Overlay vertical line"), status = "primary", value = F)),
           conditionalPanel("input.add_vert", ns = ns,
             column(7, uiOutput(ns("add_vert_ui")))
           )
         ),
         fluidRow(
           column(5, shinyWidgets::materialSwitch(ns("add_hor"), h6("Overlay horizontal line"), status = "primary", value = F)),
           conditionalPanel("input.add_hor", ns = ns,
             column(7, 
                # numericInput()
                sliderInput(ns("hor_y_int"), "Y-intercept", min = 1, max = 1, value = 1))
           )
         )
      )
      # , conditionalPanel("input.add_line", ns = ns,
      #   fluidRow(
      #      column(6,# uiOutput(ns("add_vert_ui"))
      #             selectInput(ns("add_vert"), "Vertical line's x-intercept:",
      #               choices = "NONE", selected = "NONE")
      #             ),
      #      column(6, # uiOutput(ns("add_hor_ui"))
      #             selectInput(ns("add_hor"), "Horizontal line's y-intercept:",
      #               choices = "NONE", selected = "NONE")
      #             )
      #   )
      # )
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
    shinyWidgets::radioGroupButtons(ns("value"), "Value", justified = T,
                                    choices = c("AVAL", "CHG"),
                                    selected = isolate(input$value)
                                    )
  })
  
  # if use wants to overlay a horizontal line on the plot
  observe({
    req(input$add_hor)
    
    # # d <- all_data
    # d <- data()
    # 
    # if(input$yvar != "" & !(input$yvar %in% colnames(d))){
    #   sel_d <- d %>% dplyr::filter(PARAMCD == input$yvar) #%>% select_if(~!all(is.na(.)))
    #   sel_y_vals <- sel_d %>% select(input$value) %>% distinct() %>% pull() %>% sort()
    # } else {
    #   sel_d <- d
    #   sel_y_vals <- sel_d %>% select(input$yvar) %>% distinct() %>% pull() %>% sort()
    # }

    data0 <- data()
    # data0 <- all_data
    yvar <- input$yvar
    value <- input$value
    time <- input$time
    color <- input$color
    separate <- input$separate
    
    timeN <- paste0(time, "N")
    colorN <- paste0(color, "N")
    separateN <- paste0(separate, "N")

    # subset data based on yvar being paramcd or not
    if (yvar %in% colnames(data)) {
      suppressWarnings(
        d0 <- data0 %>% select(USUBJID, time, one_of(timeN), val = yvar, one_of(color, colorN, separate, separateN))
      )
    } else { # yvar is a paramcd
      suppressWarnings(
        d0 <- data0 %>%
          dplyr::filter(PARAMCD == yvar) %>%
          select(USUBJID, time, one_of(timeN), PARAM, PARAMCD, val = one_of(value), one_of(color, colorN, separate, separateN))
      )
    }

    val_sym <- rlang::sym("val")
    
    # Group data as needed to calc means
    suppressWarnings(
      d <-
        d0 %>% varN_fctr_reorder2() %>%
        group_by_at(vars(time, one_of(color, separate))) %>%
        summarize(MEAN = round(mean(!!val_sym, na.rm = T), 2),
                  # SEM = round(std_err(!!val_sym, na.rm = T),2), # NOT accurate?
                  N = n_distinct(USUBJID, na.rm = T),
                  n = n(),
                  STD = round(sd(!!val_sym, na.rm = T), 2),
                  SEM = round(STD/ sqrt(n), 2),
                  .groups = "keep") %>%
        ungroup() %>%
        mutate(Lower = MEAN - (1.96 * SEM), Upper = MEAN + (1.96 * SEM)) %>%
        select( -n)
    )
    
    sel_y <- na.omit(d$MEAN)
    sel_y_low <- floor(min(d[[ifelse(input$err_bars, "Lower", "MEAN")]], na.rm = T))
    sel_y_up <- ceiling(max(d[[ifelse(input$err_bars, "Upper", "MEAN")]], na.rm = T))
    updateSliderInput(session, "hor_y_int", min = sel_y_low, max = sel_y_up, step = .1,
      value = ifelse(between(isolate(input$hor_y_int), sel_y_low, sel_y_up),
                        isolate(input$hor_y_int), floor(median(sel_y))))

  })
  
  
  
  
  # if use wants to overlay a vertical line on the plot
  observe({
    req(input$add_vert)
    
    # d <- all_data
    d <- data()
    
    if(input$yvar != "" & !(input$yvar %in% colnames(d))){
      sel_d <- d %>% dplyr::filter(PARAMCD == input$yvar) #%>% select_if(~!all(is.na(.)))
    } else {
      sel_d <- d
    }

    varN <- paste0(input$time,"N")
    suppressWarnings(
      sel_time_vals0 <- sel_d %>%
        select(input$time, one_of(varN)) %>%
        distinct() %>%
        varN_fctr_reorder2()
    )
    
    if(is.factor(sel_time_vals0[[1]])) {
      print("is.factor")
      sel_time_vals <- sel_time_vals0 %>%
        arrange_at(vars(one_of(varN), input$time)) %>%
        pull(input$time) %>%
        as.character()
      # print(sel_time_vals)
      
      sel_time <- na.omit(sel_time_vals)
      
      output$add_vert_ui <- renderUI({
        selectInput(ns("vert_x_int"), "X-intercept:", choices = sel_time,
                    selected = ifelse(isolate(input$vert_x_int) %in% sel_time,
                                      isolate(input$vert_x_int), sel_time[1]))
      })
      
    } else if(toupper(substr(input$time, nchar(input$time) - 1, nchar(input$time))) == "DT") {
      print("is.DT")
      sel_time_vals <- sel_time_vals0 %>%
        select(input$time) %>%
        mutate_all(as.character) %>%
        mutate_all(as.Date) %>%
        pull() %>% sort()
      sel_time <- na.omit(sel_time_vals)
      # print(sel_time)
      # print(paste("isolate(input$vert_x_int):", isolate(input$vert_x_int)))
      # print(paste("!lubridate::is.Date(isolate(input$vert_x_int)):", !lubridate::is.Date(isolate(input$vert_x_int))))
      
      output$add_vert_ui <- renderUI({
        dateInput(ns('vert_x_int'), "X-intercept:",
                  min = min(sel_time), max = max(sel_time), value = sel_time[1]
                  # ifelse(!lubridate::is.Date(isolate(input$vert_x_int)), sel_time[1],
                  #   ifelse(isolate(input$vert_x_int) %in% seq(from = min(sel_time), to = max(sel_time), by = 1),
                  #    isolate(input$vert_x_int), sel_time[1]))
        )
      })
      
    } else if(typeof(sel_time_vals0[[1]]) %in% c("integer", "double")){
      print("is.double | is.integer")
      sel_time_vals <- sel_time_vals0 %>% pull(input$time) %>% sort()
      sel_time <- na.omit(sel_time_vals)
      # print(sel_time)C
      
      output$add_vert_ui <- renderUI({
        sliderInput(ns('vert_x_int'), "X-intercept:",
                    min = min(sel_time), max = max(sel_time), value = sel_time[1]
                    # ifelse(between(isolate(input$vert_x_int), min(sel_time), max(sel_time)),
                    #        isolate(input$vert_x_int), sel_time[1])
        )
      })
      
    } else {
      print("else")
      sel_time_vals <- sel_time_vals0 %>% arrange_at(vars(input$time)) %>%
        pull() %>% as.character()
      # print(sel_time_vals)
      
      sel_time <- na.omit(sel_time_vals)
      output$add_vert_ui <- renderUI({
        selectInput(ns("vert_x_int"), "X-intercept:", choices = sel_time,
                    selected = ifelse(isolate(input$vert_x_int) %in% sel_time,
                                      isolate(input$vert_x_int), sel_time[1]))
      })
    }
    # print(".")
    # print(".")

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
    req(data(), input$yvar, input$time)
    pp <- app_lineplot(data(), input$yvar, input$time, input$value, input$separate, input$color,
      input$err_bars, input$label_points, input$gtxt_x_pos , input$gtxt_y_pos,
      input$add_vert, input$vert_x_int, input$add_hor, input$hor_y_int)
    return(list(plot = pp$plot, data = pp$data))
  })
  p <- reactive( p_both()$plot )
  p_data <- reactive( p_both()$data )
  
  # return the plot object to parent module
  # return(p)
  return(list(plot = p, #plot_ht = px_ht_num, plot_nm = dwnld_nm,
              plot_data = p_data))
}
