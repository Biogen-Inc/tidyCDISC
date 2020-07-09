boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(ns("yvar"), "Response Variable", choices = "DIABP", selected = "DIABP")),
      column(6, conditionalPanel(condition = "output.is_y_week", ns=ns,
        selectInput(ns("week"), "Select Week", choices = "")))
    ),

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
    
    group <- subset_colclasses(data(), is.character)
    group <- group[group != "data_from"]

    updateSelectInput(session, "yvar", choices = c(paramcd, num_col))
    updateSelectInput(session, "group", choices = group)
    updateSelectInput(session, "week", choices = weeks_list())
  })
  
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons("value", "Value", choices = c("AVAL", "CHG"))
  })
  
  weeks_list <- reactive({
    req(data()$AVISIT)
    unique(data() %>% select(AVISIT) %>% filter(AVISIT != "") %>% pull(AVISIT))
  })
  
  output$is_y_week <- reactive({
    req(data()$PARAMCD)
    input$yvar %in% data()$PARAMCD
  })
  
  outputOptions(output, "is_y_week", suspendWhenHidden = FALSE) 
  
  
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
        p <- data() %>% 
          dplyr::filter(PARAMCD == input$yvar) %>%
          dplyr::filter(AVISIT == input$week) %>%
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