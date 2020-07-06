#' popExp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets prettyRadioButtons awesomeRadio
#' @importFrom plotly plotlyOutput
#' 
mod_popExp_ui <- function(id, label = "Population Explorer"){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 2,
                 
                 # fluidRow(
                 #   column(width=6,
                 #          checkboxInput(ns("adv_filtering"), "Filter data", value = F)
                 #   ),
                 #   column(width=6,
                 #          checkboxInput(ns("clearplot"), "Clear plot", value = F)
                 #   )
                 # ),
                 
                 prettyRadioButtons(
                   inputId = ns("radio"),
                   label = "Type of Chart:",
                   shape = "square",
                   choices = character(0),
                   # choices = list("Scatter Plot  " = "1",
                   #                "Spaghetti Plot" = "2",
                   #                "Box Plot      " = "3",
                   #                # "Heat Map      " = "4",
                   #                "Histogram     " = "5",
                   #                "Means Plot    " = "6",
                   #                "Hbar Plot     " = "7"
                   #                ),
                   selected = character(0),
                   icon = icon("check")
                 ),
                 # below are all the possible subparameters
                 tags$h4(id = "Parmstag", label = "Parameters:"),
                 selectizeInput(ns("selPrmCode"), label = tags$small("Parameter Code:"), choices = character(0),
                                selected=character(0), multiple = TRUE, options = list(maxItems = 1)
                 ),
                 fluidRow( 
                   column(width = 2,div(style = "height:25px;"),
                          checkboxInput(ns("groupbox"),label = NULL, value = TRUE)
                   ),
                   column(width = 10,
                          selectInput(ns("groupbyvar"), label = tags$small("Color by:"), choices=character(0), selected = character(0))
                   )
                 ),
                 awesomeRadio(
                   inputId = ns("fillType"), label = "Select one:",
                   inline = TRUE,
                   status = "info",
                   checkbox = TRUE,
                   choices = c("Use Counts", "Corr Matrix", "Fill Variable"),
                   selected = "Fill Variable"
                 ),
                 selectInput(ns("selxvar"), label = tags$small("X Variable:"), 
                             choices=character(0), selected = character(0)
                 ),             
                 selectInput(ns("selyvar"), label = tags$small("Y Variable:"), 
                             choices=character(0), selected = character(0)
                 ),
                 selectInput(ns("selzvar"), label = tags$small("Fill Variable:"), 
                             choices=character(0), selected = character(0)
                 ),
                 selectizeInput(ns("selectvars"), "Select", choices=character(0), selected = character(0),
                                multiple=TRUE, options = list(maxItems = NULL)),
                 
                 actionButton(ns("runCorr"),"Generate Graph"),
                 
                 selectInput(ns("seltimevar"), label = tags$small("Time Variable:"), 
                             choices=character(0), selected = character(0)
                 ),
                 selectInput(ns("responsevar"), label = tags$small("Response Variable:"), 
                             choices=c(""), selected = ""
                 ),
                 checkboxInput(ns("AddPoints"),label=tags$small("Add points (jitter)"),value = FALSE),
                 
                 fluidRow(
                   column(width=3, 
                          checkboxInput(ns("AddLine"), label = tags$small("Add Line"), value = FALSE)
                   ), 
                   column(width=3,
                          checkboxInput(ns("AddSmooth"), label = tags$small("Add Loess"), value = FALSE)
                   ),
                   column(width=5,
                          checkboxInput(ns("DiscrXaxis"), label = tags$small("Discrete\n x-axis"), value = FALSE))
                 ),
                 awesomeRadio(
                   inputId = ns("heatMapFill"), label = NULL,
                   inline = TRUE,
                   status = "info",
                   checkbox = TRUE,
                   choices = c("Min","Mode","Mean","Median","Max"),
                   selected = "Max"
                 ),
                 
                 prettyRadioButtons(
                   inputId = ns("errorBars"),
                   label = "Type of Error Bar:",
                   shape = "square",
                   inline = TRUE,
                   choices = list("Mean SE" = "1",
                                  "Median IQR" = "2"
                   ),
                   selected = character(0),
                   icon = icon("check")
                 ),
                 
                 prettyRadioButtons(
                   inputId = ns("hbarOptions"),
                   label = "hbar Options:",
                   shape = "square",
                   inline = TRUE,
                   choices = list("Calc Percent" = "1",
                                  "Calc Mean   " = "2"
                   ),
                   selected = character(0),
                   icon = icon("check")
                 ),
                 sliderInput(ns("numBins"), "Number of bins:",
                             min = 20, max = 200, value = 40),
                 
                 checkboxInput(ns("animate"),label="Animate Plot:",value = FALSE),
                 
                 selectInput(ns("animateby"), label = tags$small("Animate By:"),
                             choices=character(0), selected = character(0)
                 )
                 
    ), # sidebarPanel
    mainPanel(width=10,
              tabsetPanel(id=ns("tabset"), type = "tabs",
                          tabPanel("Plot", id = ns("Plot"), plotlyOutput(ns("PlotlyOut"), width = "100%", height = "600px")),
                          tabPanel("Table", id = ns("Table"), DT::dataTableOutput(ns("DataTable"))),
                          tabPanel("Filter", id = ns("Filter"),
                                   fluidRow(
                                     column(width=1,
                                            checkboxInput(ns("adv_filtering"), "Filter data", value = F)
                                       ),
                                     column(width=5, 
                                            conditionalPanel(condition = "input.adv_filtering == true", ns = ns,
                                                             IDEAFilter::shiny_data_filter_ui(ns("data_filter"))
                                       )
                                     )
                                   )
                          ) #tabPanel
              ) # tabsetPanel
              
    ) # mainPanel
  )  # sidebarLayout
}
## To be copied in the UI -- done
# mod_popExp_ui("popExp_ui_1")
