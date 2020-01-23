tableGeneratorUI <- function(id, label = "Create Chart") {
  ns <- NS(id)
  
  fluidPage(
    sidebarPanel(width = 6,
                 fluidRow(column(12, recipe
                 )),
                 # how do I use this within JS to add 
                 # a mean block to droppable di
                 
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
                              div(tippy(div("T-TEST"), "Tooltip Text"))
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
      fluidRow(tableOutput("all"))
    ),
  )
  
}
