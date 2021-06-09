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
      ),
      fixedRow(
        column(6, shinyWidgets::materialSwitch(ns("show_sig"), 
           h6("Only label significant:"),status = "primary", value =  F)),
        conditionalPanel("input.show_sig", ns = ns,
             column(6, numericInput(ns("sig_level"), "Set significance level:",
                value = .05, min = 0, max = .1, step = .01))
                         
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
    
    # count on if one, both, or none of param_x or param_y have a paramcd
    
    px_pcd <- input$yvar_x[!(input$yvar_x %in% colnames(d))]
    py_pcd <-  input$yvar_y[!(input$yvar_y %in% colnames(d))]
    pcds <- unique(c(px_pcd, py_pcd))
    num_pcds <- length(pcds)
    
    # Update time variable based on yvar selection
    if(num_pcds > 0){
      # if(!rlang::is_empty(pcds)) d <- d %>% filter(PARAMCD %in% pcds)
      
      seltime_dat <- d %>%
        filter(PARAMCD %in% pcds) %>%
        filter(!is.na(AVAL)) %>% # aval is not missing...
        select_if(~!all(is.na(.)))  # grab time vars remaining
      
      potential_times <- seltime_dat %>%
        select(ends_with("DY"), contains("VIS")) %>%
        colnames() %>% sort()
      
      # Only allows visit variables that have only 1 aval
      # test_time <- potential_times[3]
      seltime <- purrr::map_chr(potential_times, function(test_time){
        tt_sym <- rlang::sym(test_time)
        rows <-
          seltime_dat %>% filter(!is.na(!!tt_sym) & as.character(!!tt_sym) != "") %>%
            select(USUBJID, test_time, PARAMCD, AVAL) %>%
            group_by_at(vars("USUBJID", test_time, "PARAMCD")) %>%
            summarize(n = n(), .groups = "keep") %>%
            ungroup() %>%
            filter(n > 1) %>%
            nrow()
        ifelse(rows > 0, NA_character_, test_time)
      }) %>%
        na.omit() %>% as.character()
      
    } else {
      seltime <- "NONE"
    }
    updateSelectInput(session, "time", choices = unique(c("NONE", seltime)), selected = isolate(input$time))
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
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  # input <- list(
  #   data <- bds_data
  #   ,
  #   yvar_x = c("ALP", "AGE", "ALB")
  #   ,
  #   yvar_y = c("ALB","ALT", "BILI")
  #   ,
  #   show_sig = T
  #   ,
  #   sig_level = .05
  #   ,
  #   # time <- "AVISIT"
  #   time <- "VISIT"
  #   # time <- "NONE"
  #   ,
  #   value <- "AVAL"
  #   # value <- "CHG"
  #   ,
  #   cor_mthd <- "pearson"
  #   # cor_mthd <- "spearman"
  # )
  p_both <- reactive({
    req(data(), input$yvar_x, input$yvar_y) #, input$time, input$cor_mthd)

    pp <- IDEA_heatmap(data(), input$yvar_x, input$yvar_y, input$time, "AVAL",
                       input$cor_mthd, input$show_sig, input$sig_level)
    
    return(list(plot = pp$plot, data = pp$data))
  })
  
  # put each piece in it's own container
  p <- reactive( p_both()$plot )
  p_data <- reactive( p_both()$data )
  
  # return the plot object to parent module
  return(list(plot = p, #plot_ht = px_ht_num, plot_nm = dwnld_nm,
              plot_data = p_data))
}