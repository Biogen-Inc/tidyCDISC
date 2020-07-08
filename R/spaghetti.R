spaghettiPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectInput(ns("yvar"), "Y Variable", choices = NULL)),
      column(6, selectInput(ns("week"), "Week", choices = NULL))
    ),
    
    fluidRow(
      column(12, align = "center",
             shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
      )
    ),
    
    selectInput(ns("time"), "Time Variable", choices = NULL),
    checkboxInput(ns("animate"), "Animate Plot?")
  )
}

spaghettiPlot_srv <- function(input, output, session) {
    ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle("Spaghetti Plot")
}