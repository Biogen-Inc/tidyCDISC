scatterPlot_ui <- function(id, label = "scatter") {
  ns <- NS(id)
  tagList(
  fluidRow(
    column(6, selectInput(ns("xvar"), "X Variable", choices = NULL)),
    column(6, selectInput(ns("week_x"), "Week", choices = NULL))
  ),
  
  fluidRow(
    column(12, align = "center",
           shinyWidgets::radioGroupButtons(ns("value_x"), "X Value", choices = c("AVAL", "CHG"))
    )
  ),
  
  fluidRow(
    column(6, selectInput(ns("xvar"), "Y Variable", choices = NULL)),
    column(6, selectInput(ns("week_x"), "Week", choices = NULL))
  ),
  
  fluidRow(
    column(12, align = "center",
           shinyWidgets::radioGroupButtons(ns("value_y"), "Y Value", choices = c("AVAL", "CHG"))
    )
  ),
  
  selectInput(ns("seperate"), "Seperate Plots By", choices = NULL),
  selectInput(ns("color"), "Color Plots By", choices = NULL)
  )
}

scatterPlot_srv <- function(input, output, session) {
    ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Length)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle("Scatter Plot")
}