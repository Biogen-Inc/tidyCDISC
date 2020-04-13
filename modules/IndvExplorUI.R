IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  useShinyjs()
  
  tagList(
    
    h4(strong("Filter by Patient Number")),
    # h6(textOutput(ns("filter_header"))),
    # textOutput(ns("filter_bds_header")),
    fluidRow(
      column(3, #id = ns("f_waiter"),
        checkboxInput(ns("adv_filtering"), "Advanced Filtering?", value = F),
        conditionalPanel(condition = "input.adv_filtering", ns = ns,
            # uiOutput(ns("filter_df_ui")),
            selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM", multiple = TRUE,
                        choices = NULL, selected = NULL),
            conditionalPanel(condition = "!is.null(input.filter_df)", ns = ns,
                IDEAFilter::shiny_data_filter_ui(ns("data_filter")))
            ),
        selectInput(
          ns("selPatNo"),
          label = "Please Select a USUBJID",
          choices = " "
        )
      )
    )
    ,
    
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
      column(9, checkboxGroupInput(
        inputId = ns("checkGroup"),
        label = "For additional patient events, load an AE, LB, CM, or MH",
        choices = c(" "),
        selected = NULL,
        inline = TRUE
      )), 
      column(3, materialSwitch(ns("events_apply_filter")
                               , label = strong(em(h5("Apply Filters")))
                               , status = "primary"
                               , value = T))
      # ,column(1)
    )
  ,
    fluidRow(column(10,timevisOutput(ns("eventsPlot")))),
    textOutput(ns("events_tv_caption1")),
    textOutput(ns("events_tv_caption2")),
    # div(style = "color: red;", TextOutput(ns("applied_filters"))),
    div(style = "color: #0275d8; font-size: 11px;", htmlOutput(ns("applied_filters"))),
    br(),
    fluidRow(
      column(8, DT::dataTableOutput(ns("eventsTable")))
    ),
    
    br(),
    hr(id = ns("hr3"), style = "border-color: darkblue;"),
    br(),
    
    h4(strong(textOutput(ns("plot_header")))),
    h6(textOutput(ns("subjid_subtitle3"))),
    # textOutput(ns("subjid_subtitle")),
    # checkboxInput(ns("bds_remove_filter"), "Remove data filters defined above", value = FALSE),
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
