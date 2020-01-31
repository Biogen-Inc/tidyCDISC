IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  seltypes <- c(" ","MEDS","LABS")
  
  useShinyjs()
  
  tagList(

    selectInput(
      ns("selPatNo"),
      label = tags$small("Patient Number: (select one to proceed)"),
      choices = " "
    ),
    
    fluidRow(
      column(12, DT::dataTableOutput(ns("demogInfo")))
    ),
    
    hr(id = ns("hr2"), style = "border-color: darkblue;"),
    
    checkboxGroupInput(
      inputId = ns("checkGroup"),
      label = "Types of Events",
      choices = c("DUMMY" = "ZZ"),
      selected = NULL,
      inline = TRUE
    ),
    
    fluidRow(
      column(8, DT::dataTableOutput(ns("eventsTable")))
    ),
    
    hr(id = ns("hr3"), style = "border-color: darkblue;"),
    
    selectInput(
      ns("selType"),
      label = tags$small("Type of Value:"),
      choices = seltypes,
      selected =  " "
    ),
    selectInput(
      ns("selLabCode"),
      label = tags$small("Lab Code:"),
      choices = c(" "),selected = " "
    ),
    
    fluidRow(column(6, DT::dataTableOutput(ns("DataTable"))),
             column(6, plotlyOutput(ns("PlotChart"), width = "100%", height = "600px")))
  ) # taglist        
} # IndvExplorUI
