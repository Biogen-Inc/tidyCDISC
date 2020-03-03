customDownloadbutton <- function (outputId, label = "Download", class = NULL, ...) 
{
  aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", 
                                              class), href = "", target = "_blank", download = NA, 
                 NULL, label, ...)
}


tableGeneratorUI <- function(id, label = "Create Chart") {
  
  ns <- NS(id)
  
  fluidPage(
    div(style = "font-size: 10",
    sidebarPanel(width = 6,
      div(class = "btn-group", style="",
          id = "download_type",
          tags$button(class = "btn btn-default dropdown-toggle", 
                      `data-toggle` = "dropdown",
                      `aria-haspopup` = "true",
                      `aria-expanded` = "false",
                      "Table Output Type",
                      span(class = "caret")
          ),
          tags$ul(class = "dropdown-menu",
                  tags$li(customDownloadbutton(ns("downloadData"), "CSV", class = "downloadButton"),
                          #customDownloadbutton(ns("downloadXPT"), "XPT", class = "downloadButton"),
                          #customDownloadbutton(ns("downloadSAS"), "SAS", class = "downloadButton"),
                          customDownloadbutton(ns("downloadRTF"), "RTF", class = "downloadButton"),
                          customDownloadbutton(ns("downloadPDF"), "PDF", class = "downloadButton")
                  )
          )
      ),
      
      fluidRow(
        column(12, "Commonly Used Tables", recipe)),
                 textInput(ns("table_title"), "Table Title", "Table Title "),
                 fluidRow(uiOutput(ns("col_ADSL"))),
                 div(fluidRow(
                   radioGroupButtons(ns("to_filter"), "Filter?", choices = c("No", "Yes"), status = "primary", selected = "No")
                 ), style = "text-align: center;vertical-align: middle;"),
                 
                 conditionalPanel(condition = "input.to_filter == 'Yes'", ns = ns,
                                  fluidRow(
                                    column(4, uiOutput(ns("filtering_by"))),
                                    column(4, selectInput(ns("condition"), "Filter Operator", 
                                                          choices = c("Equals" = "==",
                                                                      "Not Equal" = "!=",
                                                                      "Less Than" = "<",
                                                                      "Less Than or Equal" = "<=",
                                                                      "Greater Than" = ">",
                                                                      "Greater Than or Equal" = ">="))),
                                    column(4, selectInput(ns("filt_grp"), "Value:",
                                                          character(0)))
                                  )),
                 
                 fluidRow(
                   uiOutput("all_rows"),
                   dropArea(col = 3, "Variables", "d_blocks", "droppable_blocks", "ui-sortable-helper sortTxtbox droppable_blocks droppable_blocks", "padding-right:0.1px"),
                   dropArea(col = 5, "Stats", "d_agg", "droppable_agg", "ui-sortable-helper sortTxtbox droppable_agg", "padding-left:0.1px"),
                   
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
                 ))
    ),
    
    mainPanel(style = "max-width: 500px;",
      fluidRow(htmlOutput(ns("title"))),
      fluidRow(tableOutput(ns("all")))
    ),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js")
  )
  
}
