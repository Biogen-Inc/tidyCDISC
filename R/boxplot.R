boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = NULL),
    fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session, data, plot_type) {
  ns <- session$ns
  # Update Inputs
  observeEvent(plot_type(),
               {
                 if (plot_type() == "Box Plot") {
                   updateSelectInput(session, "group", choices = subset_colclasses(data(), is.character))
                   updateSelectInput(session, "yvar", choices = subset_colclasses(data(), is.numeric))
                   
                   # why is this rendering without any dropdowns>
                   output$include_var <- renderUI({
                     req(input$yvar %in% data()$PARAMCD)
                     shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
                   })
                 }
               })
  
  # Create Plot based on inputs, return to parent app.R
  p <- eventReactive({
    input$group
    input$yvar
  },{
    req(input$group)
    req(input$yvar)
    
    ggplot2::ggplot(data()) +
      ggplot2::aes(x = .data[[input$group]], y = .data[[input$yvar]]) +
      ggplot2::geom_boxplot()
  })
  
  return(p)
}