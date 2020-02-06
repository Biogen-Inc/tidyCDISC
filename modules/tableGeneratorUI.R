tableGeneratorUI <- function(id, label = "Create Chart") {
  
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(width = 6,
                 fluidRow(column(12, recipe)),
                 fluidRow(radioGroupButtons(
                   inputId = ns("COLUMN"), "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "NONE"), selected = "NONE")),
                 fluidRow(
                   uiOutput("all_rows"),
                   dropArea("Drop Here", "d_blocks", "droppable_blocks", "ui-sortable-helper sortTxtbox droppable_blocks droppable_blocks", "padding-right:0.1px"),
                   dropArea("Drop Here", "d_agg", "droppable_agg", "ui-sortable-helper sortTxtbox droppable_agg", "padding-left:0.1px"),
                   
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
      fluidRow(tableOutput(ns("all"))),
      fluidRow(tableOutput(ns("debug")))
    ),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js")
  )
  
}
