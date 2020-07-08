boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    selectInput(ns("yvar"), "Response Variable", choices = "DIABP", selected = "DIABP"),
    fluidRow(column(12, align = "center", renderUI(ns("include_var")))),
    selectInput(ns("group"), "Group By", choices = NULL),
    checkboxInput(ns("points"), "Add Points?")
  )
}

boxPlot_srv <- function(input, output, session, data) {
  ns <- session$ns
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  # why don't these work!?
  observe({
    req(data()$PARAMCD)
    # this is working!!!
    print(unique(data()$PARAMCD))
    # but this doesnt ??? 
    updateSelectInput(session, "yvar", choices = unique(data()$PARAMCD))
    updateSelectInput(session, "group", choices = as.list(subset_colclasses(data(), is.character)))
  })
  
  # if paramcd selected, let user select AVAL or CHG 
  # why isn't this rendering? 
  # I assume for same reason yvar isn't updating! 
  output$include_var <- renderUI({
    req(input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(ns("value"), "Value", choices = c("AVAL", "CHG"))
  })
  
  # -------------------------------------------------
  # Create boxplot using inputs 
  # -------------------------------------------------
  
  p <- reactive({
    ggplot2::ggplot(data(), ggplot2::aes_string(x = "SEX", y = "AGE")) +
      ggplot2::geom_boxplot()
  })
  
  return(p)
}