IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  # seltypes <- c(" ","MEDS","LABS")
  
  useShinyjs()
  
  tagList(
    

    selectInput(
      ns("selPatNo"),
      label = "Please Select a Patient Number (USUBJID)",
      choices = " "
    ),
    
    br(),
    fluidRow(
      column(12, DT::dataTableOutput(ns("demogInfo")))
    ),
    
    br(),
    hr(id = ns("hr2"), style = "border-color: darkblue;"),
    br(),
    
    checkboxGroupInput(
      inputId = ns("checkGroup"),
      label = h4(strong("Patient Events")),
      choices = c(" "),
      selected = NULL,
      inline = TRUE
    ),
    
    fluidRow(
      column(8, DT::dataTableOutput(ns("eventsTable")))
    ),
    
    br(),
    hr(id = ns("hr3"), style = "border-color: darkblue;"),
    br(),
    
    selectInput(
      ns("selType"),
      label = HTML("Type of Value: MEDS or LABS<br/>Did you select ADCM or ADLB?"),
      choices = c(" "),
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
