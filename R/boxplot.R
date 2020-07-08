subset_colclasses <- function(DF, colclasses) {
  names(DF)[purrr::map(DF, colclasses) %>% unlist()]
}

boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = NULL),
    
    fluidRow(
      column(12, align = "center",
             shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
      )
    ),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # why don't these work!?
  observe({
    print(subset_colclasses(data(), is.numeric))
    updateSelectInput(session, "group",
                      choices = as.list(subset_colclasses(data(), is.factor)),
                      selected = "Species")
    updateSelectInput(session, "yvar", 
                      choices = as.list(subset_colclasses(data(), is.numeric)),
                      selected = "Petal.Length")
  })
  
  # Create Plot based on inputs
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes_string(x = "SEX", y = "AGE")) +
      ggplot2::geom_boxplot()
  })
  
  return(p)
}