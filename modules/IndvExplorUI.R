IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  useShinyjs()
  
  tagList(
    h1("Individual Explorer", align = "center"),
    br(), br(), br(),
    h4(strong("Filter by Patient Number")),
    fluidRow(
      column(3, #id = ns("f_waiter"),
        checkboxInput(ns("adv_filtering"), "Advanced Pre-Filtering?", value = F),
        conditionalPanel(condition = "input.adv_filtering", ns = ns,
            selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM", multiple = TRUE,
                        choices = NULL, selected = NULL),
            conditionalPanel(condition = "!is.null(input.filter_df)", ns = ns,
                IDEAFilter::shiny_data_filter_ui(ns("data_filter")))
            ),
        selectInput(
          ns("selPatNo"),
          label = "Please Select a USUBJID",
          choices = ""
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
    
    br(),br(),
    
    hidden(
      div(id = ns("mytabs"), 
          tabsetPanel(type = "tabs", # id = "whichTab", # id doesn't seem to work
      
      # Events Panel
      tabPanel(div(style = "font-size: 14px;","Events"),
               br(),br(),
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
               
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      
      
      # Visits Panel
      tabPanel(div(style = "font-size: 14px;","Visits"),
               br(),br(),
               
               h4(strong(textOutput(ns("plot_header")))),
               h6(textOutput(ns("subjid_subtitle3"))),
               fixedRow(
                 column(2,
                        selectInput(
                          ns("plot_adam"),
                          label = HTML("Select a loaded ADaM<br/>with columns 'PARAM' & 'AVAL'"),
                          choices = c(" "),
                          selected =  " "
                        )
                 ),
                 column(2,
                        selectInput(
                          ns("plot_param"),
                          label = HTML("<br/>Select a Parameter / Metric:"),
                          choices = c(" "),selected = " "
                        )
                 ),
                 column(2,
                        selectInput(
                          ns("visit_var"),
                          label = HTML("<br/>Select a Visit Variable"),
                          choices = c(" ", "AVISITN"),selected = " "
                        )
                 ),
                 column(1,
                        checkboxGroupInput(
                          ns("plot_hor"),
                          label = HTML("<br/>Plot line for:"),
                          choices = c(" ")
                        )
                 ),
                 column(2,
                        checkboxGroupInput(
                          ns("overlay_events"),
                          label = HTML("<br/>Overlay Events:"),
                          choices = c(" ")
                        ),
                        div(style = "color: blue; font-size: 12px;",
                            uiOutput(ns("display_dy"))
                        )
                 ),
                 column(1, 
                        radioButtons(
                          ns("event_type_filter"),
                          label = HTML("<br/>Choose Events:"),
                          # inline = T,
                          choices = as.list(c("All", "Manually Filter")),
                          selected = "All"
                        )
                 ),
                 column(2,
                        br(),br(),
                        div(style = "color: #0275d8; font-size: 11px;", htmlOutput(ns("v_applied_filters"))))
               ), # end fixedRow
               
               # Add a row that contains another "Apply Filters" toggle
               fluidRow(
                 column(9), 
                 column(3,
                        selectizeInput(
                          ns("overlay_event_vals"),
                          label = HTML("<br/>Select Event Type(s)"),
                          multiple = T,
                          choices = c("All"),
                          selected = "All"
                        )
                 )
                 # ,column(1)
               ),
               
               fluidRow(column(10,plotlyOutput(ns("PlotChart"), width = "100%", height = "600px"))),
               br(),
               fluidRow(column(6, DT::dataTableOutput(ns("DataTable")))),
               br(),br(),br(),br(),br(),br()
      )
    ))) # TabsetPanel & it's div & hidden
    # ) # conditionalPanel
    
    
  ) # taglist        
} # IndvExplorUI
