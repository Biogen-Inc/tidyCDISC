IndvExplorUI <- function(id, label = "Individual Explorer") {
  
  ns <- NS(id)
  
  useShinyjs()
  
  tagList(
    h1("Individual Explorer", align = "center"),
    br(), br(), br(),
    fluidRow(
      column(3, #id = ns("f_waiter"),
        wellPanel(
          h4(strong("Filter by Patient Number")),
          br(),
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
      
      #####################
      # 
      # Events Panel
      #
      ######################
      
      tabPanel(div(style = "font-size: 14px;","Events"),
               br(),br(),br(),
                 
                 
               fluidRow(
                 
                 column(10, wellPanel(
                   fluidRow(
                     column(10, 
                      h4(strong(textOutput(ns("events_header")))),
                      h6(textOutput(ns("subjid_subtitle2"))),
                      checkboxGroupInput(
                       inputId = ns("checkGroup"),
                       label = "For additional patient events, load an AE, LB, CM, or MH",
                       choices = c(" "),
                       selected = NULL,
                       inline = TRUE
                     )), 
                     column(2, 
                            br(), br(),
                            materialSwitch(ns("events_apply_filter")
                                            , label = strong(em(h5("Apply Filters")))
                                            , status = "primary"
                                            , value = T)
                     )
                   ) # end inner fluidRow
                 )) # end column 10 & wellPanel
                 ,column(2,HTML(""))
               ) # end fluidRow
               ,
               fluidRow(column(10,timevisOutput(ns("eventsPlot")))),
               textOutput(ns("events_tv_caption1")),
               textOutput(ns("events_tv_caption2")),
               # div(style = "color: red;", TextOutput(ns("applied_filters"))),
               div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("applied_filters"))),
               br(),
               fluidRow(
                 column(8, DT::dataTableOutput(ns("eventsTable")))
               ),
               
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      
      #####################
      #
      # Visits Panel
      #
      #####################
      
      tabPanel(div(style = "font-size: 14px;","Visits"),
               br(),br(),br(),
               
               wellPanel(
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
                          div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("v_applied_filters"))))
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
                 ) # end fluidRow
               ), # end of well Panel
               
               fluidRow(column(11,plotlyOutput(ns("PlotChart"), width = "100%", height = "600px"))),
               div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("v_applied_filters_grphDisp"))),
               br(),
               fluidRow(
                 column(8, DT::dataTableOutput(ns("DataTable"))),
                 column(3,
                  # batch download UI
                   wellPanel(
                     h5(strong(textOutput(ns("dwnld_params_header")))),
                     br(),
                     radioButtons(ns('format'), 'Document format', c('HTML','PDF'),inline = TRUE), #'PDF', 
                     textInput(ns("user_batch_notes"), label = "Add Report Notes", placeholder = "My Notes"),
                     downloadButton(ns('batchDownReport'))
                   )
                 )
               ),
               br(),br(),br(),br(),br(),br()
      )
    ))) # TabsetPanel & it's div & hidden
    # ) # conditionalPanel
    
    
  ) # taglist        
} # IndvExplorUI
