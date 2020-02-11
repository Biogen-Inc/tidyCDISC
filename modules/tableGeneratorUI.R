tableGeneratorUI <- function(id, label = "Create Chart") {
  
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(width = 6,
                 fluidRow(column(12, recipe)),
                 textInput(ns("table_title"), "Table Title", "Table Title "),
                 fluidRow(radioGroupButtons(
                   inputId = ns("COLUMN"), "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "NONE"), selected = "NONE")),
                 div(fluidRow(
                   radioGroupButtons(ns("to_filter"), "Filter?", choices = c("No", "Yes"), status = "primary", selected = "No")
                 ), style = "text-align: center;vertical-align: middle;"),
                 
                 conditionalPanel(condition = "input.to_filter == 'Yes'", ns = ns,
                                  fluidRow(
                                    column(4, uiOutput(ns("filtering_by"))),
                                    column(4, selectInput(ns("condition"), "condition", 
                                                          choices = c("Equals" = "==",
                                                                      "Not Equal" = "!=",
                                                                      "Less Than" = "<",
                                                                      "Less Than or Equal" = "<=",
                                                                      "Greater Than" = ">",
                                                                      "Greater Than or Equal" = ">="))),
                                    column(4, selectInput(ns("filt_grp"), "By:",
                                                          character(0)))
                                  )),
                 
                 fluidRow(
                   uiOutput("all_rows"),
                   dropArea(col = 4, "Drop Here", "d_blocks", "droppable_blocks", "ui-sortable-helper sortTxtbox droppable_blocks droppable_blocks", "padding-right:0.1px"),
                   dropArea(col = 5, "Drop Here", "d_agg", "droppable_agg", "ui-sortable-helper sortTxtbox droppable_agg", "padding-left:0.1px"),
                   
                   column(1, offset = 0, style='padding:0px;',
                          h5("Stats"),
                          tags$ul(
                            id = "sortable_agg",
                            tags$li(
                              class = "ui-state-default agg", id = "ttest",
                              div(tippy(div("T-TEST"), "T-Test"))
                            ),
                            tags$li(
                              class = "ui-state-default agg", id = "chg",
                              div(tippy(div("CHG"), "Change from Baseline"))
                            ),
                            tags$li(
                              id = "mean",
                              "MEAN",
                              class = "agg"
                            ),
                            tags$li(
                              id = "freq",
                              "FREQ",
                              class = "agg"
                            )
                          ))
                 )
    ),
    
    mainPanel(
      fluidRow(htmlOutput(ns("title"))),
      fluidRow(tableOutput(ns("all")))
    ),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js")
  )
  
}
