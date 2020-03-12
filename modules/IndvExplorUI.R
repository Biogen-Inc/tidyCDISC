IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  useShinyjs()
  
  tagList(
    

    selectInput(
      ns("selPatNo"),
      label = "Please Select a Patient Number (USUBJID)",
      choices = " "
    ),
    
    br(),
    fluidRow(
      column(10, DT::dataTableOutput(ns("demogInfo")))
    ),
    
    br(),
    hr(id = ns("hr2"), style = "border-color: darkblue;"),
    br(),
    
    
    h4(strong(textOutput(ns("events_header")))),
    checkboxGroupInput(
      inputId = ns("checkGroup"),
      label = "For additional events, load a AE, LB, or CM",
      choices = c(" "),
      selected = NULL,
      inline = TRUE
    ),
    
    fluidRow(
      column(10, DT::dataTableOutput(ns("eventsTable")))
    ),
    
    br(),
    hr(id = ns("hr3"), style = "border-color: darkblue;"),
    br(),
    
    h4(strong(textOutput(ns("plot_header")))),
    selectInput(
      ns("selType"),
      label = HTML("Select a loaded ADaM with columns 'PARAM' &<br/>'AVAL', plus either 'AVISIT', 'AVISITN, or 'VISITDY'"),
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
