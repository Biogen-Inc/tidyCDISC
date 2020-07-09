boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = "DIABP", selected = "DIABP"),
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
    num_col <- subset_colclasses(data(), is.numeric)
    paramcd <- data()$PARAMCD

    updateSelectInput(session, "yvar", choices = c(paramcd, num_col))
    updateSelectInput(session, "group", choices = as.list(subset_colclasses(data(), is.character)))
  })
  
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
  })
  
  # -------------------------------------------------
  # Create boxplot using inputs 
  # -------------------------------------------------
  
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes_string(x = input$group, y = input$yvar)) +
      ggplot2::geom_boxplot()
  })
  
  return(p)
}