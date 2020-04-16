PopuExplorUI <- function(id, label = "Population Explorer") {
  
ns <- NS(id)
  
sidebarLayout(
  sidebarPanel(width = 2,
               prettyRadioButtons(
                 inputId = ns("radio"),
                 label = "Type of Chart:",
                 shape = "square",
                 choices = list("Scatter Plot  " = "1",
                                "Spaghetti Plot" = "2",
                                "Box Plot      " = "3",
                                "Heat Map      " = "4",
                                "Histogram     " = "5"
                                ),
                 selected = character(0),
                 icon = icon("check")
               ),
               # below are all the possible subparameters
               tags$h4("Parameters:"),
               selectizeInput(ns("selPrmCode"), label = tags$small("Parameter Code:"), choices = c(" "),
                  selected=character(0), multiple = TRUE, options = list(maxItems = 1)
               ),
               fluidRow( 
                 column(width = 2,div(style = "height:25px;"),
                 checkboxInput(ns("groupbox"),label = NULL, value = TRUE)
                 ),
                 column(width = 10,
                 selectInput(ns("groupbyvar"), label = tags$small("Color by:"), c(" "), selected = " ")
                 )
               ),
               selectInput(ns("selxvar"), label = tags$small("X Variable:"), 
                           c(" "), selected = " "
               ),             
               selectInput(ns("selyvar"), label = tags$small("Y Variable:"), 
                           c(" "), selected = " "
               ),
               awesomeRadio(
                 inputId = ns("fillType"), label = "Select one:",
                 inline = TRUE,
                 status = "info",
                 checkbox = TRUE,
                 choices = c("Use Counts", "Corr Matrix", "Fill Variable"),
                 selected = "Fill Variable"
               ),
               selectizeInput(ns("selectvars"), "Select", choices=c(" "), selected = NULL,
                              multiple=TRUE, options = list(maxItems = NULL)),
               
               actionButton(ns("runCorr"),"Generate Graph"),

               selectInput(ns("selzvar"), label = tags$small("Fill Variable:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("seltimevar"), label = tags$small("Time Variable:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("responsevar"), label = tags$small("Response Variable:"), 
                           c(" "), selected = " "
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
               
               sliderInput(ns("numBins"), "Number of bins:",
                           min = 20, max = 200, value = 40),
               
               # Still under contstruction...
               checkboxInput(ns("animate"),label="Animate Plot:",value = FALSE),
               
               selectInput(ns("animateby"), label = tags$small("Animate By:"),
                           c(" "), selected = " ")
               
  ), # sidebarPanel
  mainPanel(width = 10,
            verbatimTextOutput("text"),
            plotlyOutput(ns("PlotlyOut"), width = "100%", height = "600px"),
            DT::dataTableOutput(ns("DataTable"))
  ) # mainPanel
)  # sidebarLayout
} # PopUExplorUI