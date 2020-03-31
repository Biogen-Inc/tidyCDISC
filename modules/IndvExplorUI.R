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
    h4(strong(textOutput(ns("demog_header")))),
    h6(textOutput(ns("subjid_subtitle1"))),
    fluidRow(
      column(10, DT::dataTableOutput(ns("demogInfo")))
    ),
    
    br(),
    hr(id = ns("hr2"), style = "border-color: darkblue;"),
    br(),
    
    
    h4(strong(textOutput(ns("events_header")))),
    h6(textOutput(ns("subjid_subtitle2"))),
    fluidRow(
      checkboxGroupInput(
        inputId = ns("checkGroup"),
        label = "For additional events, load a AE, LB, CM, or MH",
        choices = c(" "),
        selected = NULL,
        inline = TRUE
      )
    ),
    fluidRow(
      column(10,timevisOutput(ns("eventsPlot")))),
    fluidRow(
      column(8, DT::dataTableOutput(ns("eventsTable")))
    ),
    
    br(),
    hr(id = ns("hr3"), style = "border-color: darkblue;"),
    br(),
    
    h4(strong(textOutput(ns("plot_header")))),
    h6(textOutput(ns("subjid_subtitle3"))),
    # textOutput(ns("subjid_subtitle")),
    fixedRow(
      column(3,
        selectInput(
        ns("plot_adam"),
        label = HTML("Select a loaded ADaM<br/>with columns 'PARAM' & 'AVAL'"),
        choices = c(" "),
        selected =  " "
        )
      ),
      column(3,
        selectInput(
          ns("plot_param"),
          label = HTML("<br/>Select a Parameter / Metric:"),
          choices = c(" "),selected = " "
        )
      ),
      column(3,
       selectInput(
         ns("visit_var"),
         label = HTML("<br/>Select a Visit Variable"),
         choices = c(" ", "AVISITN"),selected = " "
       )
      ),
      column(2,
         checkboxGroupInput(
           ns("plot_hor"),
           label = HTML("<br/>Add horizontal line for:"),
           choices = c(" ")
         )
      ),
      column(1)
    ),
    
    # fluidRow(column(6, DT::dataTableOutput(ns("DataTable"))),
    #          column(6, plotlyOutput(ns("PlotChart"), width = "100%", height = "600px")))
    fluidRow(column(10,plotlyOutput(ns("PlotChart"), width = "100%", height = "600px"))),
    br(),
    fluidRow(column(6, DT::dataTableOutput(ns("DataTable")))),
    br()
  ) # taglist        
} # IndvExplorUI
