#' indvExp UI Function
#'
#' A shiny Module that compiles the user interface for the Individual Explorer
#' Tab. In summary, the UI's main components include an IDEAFilter widget for
#' prefiltering, a brief demographic table, and another collection of tabset
#' panels that divide out the Patient Events (leveraging a timevis object that
#' plots dates on a timeline) from patient events by Parameter graphic (plotly).
#' Note: well panels encompass any user input fields
#'
#' @param id Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom IDEAFilter shiny_data_filter_ui
#' @importFrom shinyWidgets materialSwitch
#' @importFrom timevis timevisOutput
#' @importFrom plotly plotlyOutput
#' 
#' @family indvExp Functions
#' 
mod_indvExp_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    h1("Individual Explorer", align = "center"),
    br(), br(), br(),
    fluidRow(
      column(3,
         wellPanel(
           div(style="display: inline-block; ",h4(strong("Filter by Patient Number"))),
           div(style="display: inline-block; float:right;",
               actionButton(ns("help_sel") 
                            , label = NULL
                            , icon = icon("question-circle")
                            , class = "btn-start"
                            , style = "display: inline-block; float:right; margin-bottom:15px;"
               )),
           br(),
           div(id = "cic_adv_filtering",checkboxInput(ns("adv_filtering"), "Advanced Pre-Filtering?", value = F)),
           conditionalPanel(condition = "input.adv_filtering", ns = ns,
                            div(id = "cic_filter_df",
                              selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM",
                                          multiple = TRUE, choices = NULL, selected = NULL)
                            ),
                            conditionalPanel(condition = "!is.null(input.filter_df)", ns = ns,
                               div(id = "cic_data_filter",
                                  IDEAFilter::shiny_data_filter_ui(ns("data_filter"))
                               )
                            )
           ),
           div(id = "cic_selPatNo",
             selectInput(
               ns("selPatNo"),
               label = "Please Select a USUBJID",
               choices = ""
             )
           )
         )
       )
    ),
    
    br(),
    h4(strong(textOutput(ns("demog_header")))),
    h6(textOutput(ns("subjid_subtitle1"))),
    fluidRow(column(10, DT::dataTableOutput(ns("demogInfo")))), # control width of DT
    
    br(),br(),
    
    shinyjs::hidden(
      div(id = ns("mytabs"), 
          tabsetPanel(type = "tabs",
                      
            #####################
            # 
            # Events Panel
            #
            ######################
            
            tabPanel(div(id = "cic_EventsTab", style = "font-size: 14px;","Events"),
               br(),br(),br(),
               
               
               fluidRow(
                 
                 column(10, wellPanel(
                   fluidRow(
                     column(10, 
                            div(style="display: inline-block; ",h4(strong(textOutput(ns("events_header"))))),
                            div(style="display: inline-block; float:right;",
                                actionButton(ns("help_events") 
                                             , label = NULL
                                             , icon = icon("question-circle")
                                             , class = "btn-start"
                                             , style = "display: inline-block; float:right; margin-bottom:15px;"
                            )),
                            h6(textOutput(ns("subjid_subtitle2"))),
                            div(id = "cic_checkGroup", 
                              checkboxGroupInput(
                                inputId = ns("checkGroup"),
                                label = "For additional patient events, load an AE, LB, CM, or MH",
                                choices = c(" "),
                                selected = NULL,
                                inline = TRUE
                              ))
                            ), 
                     column(2, 
                          br(), br(),
                          div(id = "cic_events_apply_filter", 
                            materialSwitch(ns("events_apply_filter")
                                           , label = strong(em(h5("Apply Filters")))
                                           , status = "primary"
                                           , value = T)
                          )
                     )
                   ) # end inner fluidRow
                 )) # end column 10 & wellPanel
                 ,column(2,HTML("")) # blank column
               ) # end fluidRow
               ,
               div(id = "cic_eventsPlot", 
                 fluidRow(column(10,timevisOutput(ns("eventsPlot")))),
                 textOutput(ns("events_tv_caption1")),
                 textOutput(ns("events_tv_caption2")),
                 div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("applied_filters")))
               ),
               br(),
               div(id = "cic_eventsTable", 
                   fluidRow(column(8, DT::dataTableOutput(ns("eventsTable"))))
               ),
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
            ),
            
            #####################
            #
            # Visits Panel
            #
            #####################
            
            tabPanel(div(id = "cic_VisitsTab", style = "font-size: 14px;","Visits"),
               br(),br(),br(),
               
               wellPanel(
                 div(style="display: inline-block; ",h4(strong(textOutput(ns("plot_header"))))),
                 div(style="display: inline-block; float:right;",
                     actionButton(ns("help_visits") 
                                  , label = NULL
                                  , icon = icon("question-circle")
                                  , class = "btn-start"
                                  , style = "display: inline-block; float:right; margin-bottom:15px;"
                     )),
                 h6(textOutput(ns("subjid_subtitle3"))),
                 fixedRow(
                   column(2,
                      div(id = "cic_plot_adam",
                          selectInput(
                            ns("plot_adam"),
                            label = HTML("Select a loaded ADaM<br/>with columns 'PARAM' & 'AVAL'"),
                            choices = c(" "),
                            selected =  " "
                          )
                      )
                   ),
                   column(2,
                      div(id = "cic_plot_param",
                          selectInput(
                            ns("plot_param"),
                            label = HTML("<br/>Select a Parameter / Metric:"),
                            choices = c(" "),selected = " "
                          )
                      )
                   ),
                   column(2,
                      div(id = "cic_visit_var",
                          selectInput(
                            ns("visit_var"),
                            label = HTML("<br/>Select a Visit Variable"),
                            choices = c(" ", "AVISITN"),selected = " "
                          )
                      )
                   ),
                   column(1,
                      div(id = "cic_plot_hor",
                          checkboxGroupInput(
                            ns("plot_hor"),
                            label = HTML("<br/>Plot line for:"),
                            choices = c(" ")
                          )
                      )
                   ),
                   column(2,
                      div(id = "cic_overlay_events",
                          checkboxGroupInput(
                            ns("overlay_events"),
                            label = HTML("<br/>Overlay Events:"),
                            choices = c(" ")
                          ),
                          div(style = "color: blue; font-size: 12px;",
                              uiOutput(ns("display_dy"))
                          )
                      ),
                   ),
                   column(1, 
                      div(id = "cic_event_type_filter",
                          radioButtons(
                            ns("event_type_filter"),
                            label = HTML("<br/>Choose Events:"),
                            choices = as.list(c("All", "Manually Filter")),
                            selected = "All"
                          )
                      )
                   ),
                   column(2,
                          br(),br(),
                          div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("v_applied_filters"))))
                 ), # end fixedRow
                 
                 # Add a row that contains another "Apply Filters" toggle, right justified
                 fluidRow(
                   column(9), 
                   column(3,
                      div(id = "cic_overlay_event_vals",
                          selectizeInput(
                            ns("overlay_event_vals"),
                            label = HTML("<br/>Select Event Type(s)"),
                            multiple = T,
                            choices = c("All"),
                            selected = "All"
                          )
                      )
                   )
                 ) # end fluidRow
               ), # end of well Panel
               
               div(id = "cic_PlotlyChart",
                 fluidRow(column(11,plotlyOutput(ns("PlotChart"), width = "100%", height = "600px"))),
                 div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("v_applied_filters_grphDisp"))),
               ),
               br(),
               fluidRow(
                 column(8, div(id = "cic_DataTable",DT::dataTableOutput(ns("DataTable")))),
                 column(3,
                        #####################
                        #
                        # Batch Download UI
                        #
                        #####################
                        div(id = "cic_batchDwnld", wellPanel(
                          h5(strong(textOutput(ns("dwnld_params_header")))),
                          br(),
                          radioButtons(ns('format'), 'Document format', c('HTML','PDF'),inline = TRUE), #'PDF', 
                          textInput(ns("user_batch_notes"), label = "Add Report Notes", placeholder = "My Notes"),
                          downloadButton(ns('batchDownReport'))
                        ))
                 )
               ),
               br(),br(),br(),br(),br(),br()
            )
          ))) # TabsetPanel & it's div & hidden
    # ) # conditionalPanel
  )
}

## To be copied in the UI -- done
# mod_indvExp_ui("indvExp_ui_1")

