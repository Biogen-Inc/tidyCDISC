boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = NULL),
    fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(data())
    
    # numeric columns, remove aval, chg, base
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- num_col[num_col != "AVAL" | num_col != "CHG" | num_col != "BASE"]
    
    # get unique paramcd
    paramcd <- unique(data()$PARAMCD)
    
    group_fc <- subset_colclasses(data(), is.factor)
    group_ch <- subset_colclasses(data(), is.character)
    group <- c(group_fc, group_ch)
    group <- group[group != "data_from"]

    updateSelectInput(session, "yvar", choices = c(paramcd, num_col))
    updateSelectInput(session, "group", choices = group)
  })
  
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
  })
  
  
  # -------------------------------------------------
  # Create boxplot using inputs
  # -------------------------------------------------
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    if (input$yvar %in% colnames(data())) {
        p <- ggplot2::ggplot(data()) + 
          ggplot2::aes_string(x = input$group, y = input$yvar) +
          ggplot2::geom_boxplot()
    } else {
      print(input$value)
        p <- data() %>% 
          dplyr::filter(PARAMCD == input$yvar) %>%
          ggplot2::ggplot() +
          ggplot2::aes_string(x = input$group, y = input$value) +
          ggplot2::geom_boxplot()
    }
    
    if (input$points) { p <- p + ggplot2::geom_jitter() }
    return(p)
  })
  
  # return the plot object to parent module
  return(p)
}