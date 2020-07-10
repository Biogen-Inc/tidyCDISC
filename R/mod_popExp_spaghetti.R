spaghettiPlot_ui <- function(id, label = "spaghetti") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = NULL),
    fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
    selectInput(ns("time"), "Time Variable", choices = NULL),
    checkboxInput(ns("animate"), "Animate Plot?")
  )
}

spaghettiPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(data())
    
    # get time based column names
    seltime <- colnames(dplyr::select(data(), ends_with("DY"), contains("VIS")))
    
    # numeric columns, remove aval, chg, base
    # then remove the x-axis selectors
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"]
    num_col <- c(setdiff(seltime, num_col), setdiff(num_col, seltime))
    
    # add paramcds to y-axis options
    paramcd <- unique(data()$PARAMCD)
    
    updateSelectInput(session, "yvar", choices = list(`Time Dependent` = paramcd, 
                                                      `Time Independent` = num_col))
    updateSelectInput(session, "time", choices = seltime)
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
        ggplot2::aes_string(x = input$time, y = input$yvar, group = "USUBJID") +
        ggplot2::geom_line() +
        ggplot2::geom_point()
    } else {
      p <- data() %>% 
        dplyr::filter(PARAMCD == input$yvar) %>%
        ggplot2::ggplot() +
        ggplot2::aes_string(x = input$time, y = input$value, group = "USUBJID") +
        ggplot2::geom_line() +
        ggplot2::geom_point()
    }
    
    p <- p + 
      ggplot2::theme(text = element_text(size = 20),
                     axis.text = element_text(size = 20)) +
      ggplot2::theme_bw()
    
    # add feature to animate plots
    # if (input$anmate) { p <- p + ggplot2::geom_jitter() }
    return(p)
  })
  
  # return the plot object to parent module
  return(p)
}