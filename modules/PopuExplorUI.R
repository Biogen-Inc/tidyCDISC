PopuExplorUI <- function(id, label = "Population Explorer") {
  
ns <- NS(id)
  
sidebarLayout(
  sidebarPanel(width = 2,
               tags$em("Note: Sample adsl data has been pre-loaded.",id=ns("adsltagsem")),
               tags$em("Note: Sample advs data has been pre-loaded.\nSelected 22 subjects from Siteid 310",id=ns("advstagsem")),
               # Copy the line below to make a set of radio buttons
               radioButtons(ns("radio"), label = h5("Type of Chart:"),
                            choices = list("Scatter Plot" = 1, 
                                           "Spaghetti Plot" = 2,
                                           "Box Plot" = 3,
                                           "Heat Map" = 4,
                                           "Histogram" = 5),
                            selected = NULL),
               
               # below are all the possible subparameters
               tags$h4("Parameters:"),
               checkboxInput(ns("bygroup"),label="Split By Group",value = TRUE),
               
               selectInput(ns("selPrmCode"), label = tags$small("Parameter Code:"),
                           choices = c(" "), selected = " "
               ),
               selectInput(ns("groupbyvar"), label = tags$small("Split by:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("selxvar"), label = tags$small("X Variable:"), 
                           c(" "), selected = " "
               ),             
               selectInput(ns("selyvar"), label = tags$small("Y Variable:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("selzvar"), label = tags$small("Fill Variable:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("seltimevar"), label = tags$small("Time Variable:"), 
                           c(" "), selected = " "
               ),
               selectInput(ns("responsevar"), label = tags$small("Response Variable:"), 
                           c(" "), selected = " "
               ),
               checkboxInput(ns("AddPoints"),label="Add points (jitter)",value = FALSE),
               
               sliderInput(ns("numBins"), "Number of bins:",
                           min = 20, max = 200, value = 40),
               
               # Still under contstruction...
               checkboxInput(ns("animate"),label="Animate Plot:",value = FALSE),
               
               selectInput(ns("animateby"), label = tags$small("Animate By:"),
                           c(" "), selected = " ")
               
  ), # sidebarPanel
  mainPanel(width = 10,
            plotlyOutput(ns("PlotlyOut"), width = "100%", height = "600px"),
            DT::dataTableOutput(ns("DataTable"))
  ) # mainPanel
)  # sidebarLayout
} # PopUExplorUI