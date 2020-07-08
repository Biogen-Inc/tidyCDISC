boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Y Variable", choices = NULL),
    
    fluidRow(
      column(12, align = "center",
             shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"), selected = "AVAL")
      )
    ),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session, data= reactive(NULL)) {
  
  print(subset_colclasses(data(), is.factor))
  
  observe({
    updateSelectInput(session, "group", choices = subset_colclasses(data(), is.factor))
    updateSelectInput(session, "yvar", choices = subset_colclasses(data(), is.numeric))
  })
  
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes(x = SEX, y = AGE)) +
    ggplot2::geom_boxplot()
  })
  
  return(p)
}