boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Y Variable", choices = NULL),
    
    fluidRow(
      column(12, align = "center",
             shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
      )
    ),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session) {
    ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Width, y = Sepal.Width)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle("Box Plot")
}