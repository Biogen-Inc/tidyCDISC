spaghettiPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Y Variable", choices = NULL),
    fluidRow(
      column(12, align = "center",
             shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
      )
    ),
    
    selectInput(ns("time"), "Time Variable", choices = NULL),
    checkboxInput(ns("animate"), "Animate Plot?")
  )
}

spaghettiPlot_srv <- function(input, output, session, data) {
  
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes_string(x = "AGE", y = "AGE")) +
      ggplot2::geom_point() +
      ggplot2::geom_line()
  })
  
  return(p)
}