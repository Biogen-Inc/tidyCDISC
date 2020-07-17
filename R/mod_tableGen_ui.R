#' tableGen UI Function
#' 
#' The UI is comprised of a drop zone
#' of both statistical and column blocks
#' as well as a means to filter and group the data.
#' The output is a gt table
#'
#' @description drag and drop table generator module
#'
#' @param id internal parameters for {shiny}.
#'
#'
#' @import shiny 
#' @importFrom IDEAFilter shiny_data_filter_ui
#' @importFrom tippy tippy
#' @importFrom gt gt_output
#' 
#' @family tableGen Functions

mod_tableGen_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("help") 
                       , label = NULL
                       , icon = icon("question-circle")
                       , class = "btn-start"
                       , style = "display: inline-block; float:right; margin-bottom:15px;"
    ),
    h1("Table Generator", align = "center"),
    br(), br(), br(),
    fluidPage(
      fluidRow(
        style = "padding: 20px",
        column(width = 6,
               # Wrangle data.
               wellPanel(
                 fluidRow(column(width = 12,
                                 div(
                                   id = "COLUMN-wrapper",
                                   uiOutput(ns("col_ADSL"))
                                 ),
                                 shinyUI(bootstrapPage(
                                   HTML('<button data-toggle="collapse" data-target="#demo" 
                                        class="btn btn-input" id="filter-accordion"
                                        style="width:100%;padding:3px;background-color:#008cba !important;color:white;size:12px;margin-bottom:10px;font-size:2em;">Filter Data</button>'),
                                   tags$div(id = 'demo',  class="collapse",
                                            selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM",
                                                        multiple = TRUE, choices = NULL, selected = NULL),
                                            IDEAFilter::shiny_data_filter_ui(ns("data_filter"))
                                   ))))),
                 wellPanel(
                   fluidRow(
                     column(12, 
                            "Commonly Used Tables", 
                            recipe,
                            br(),
                            div(class = "col-sm-3", id="all-column-blocks", style = "height:300px;overflow-y:scroll;overflow-x:hidden;",
                                uiOutput("all_rows") 
                            ),
                            
                            
                            div(class = "col-sm-8", id='all-output-blocks', style = "height:300px;overflow-y:scroll;",
                                dropArea(col = 5, styles = "padding-right:0.1px", "Variables", "d_blocks", "droppable_blocks", "ui-sortable-helper sortTxtbox droppable_blocks droppable_blocks"),
                                dropArea(col = 7, styles = "padding-left:0.1px", "Stats", "d_agg", "droppable_agg", "ui-sortable-helper sortTxtbox droppable_agg")
                            ),
                            
                            fluidRow(
                              column(1, offset = 0, style='padding:0px;',
                                     h5("Stats"),
                                     tags$ul(
                                       id = "sortable_agg",
                                       tags$li(
                                         class = "ui-state-default agg", id = "anova",
                                         div(tippy(div("ANOVA"), "ANOVA"))
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
                     )
                   )
                 ),
                 # Download data.
                 wellPanel(
                   fluidRow(column(width = 12,
                                   textInput(ns("table_title"), "Table Title", "Table Title", width = '100%'))),
                   fluidRow(column(width = 12,
                                   fluidRow(
                                     column(6, downloadButton(ns("download_gt"), "Download Table")),
                                     column(6, offset = 0,
                                            radioButtons(ns("download_type"), "Download Type", 
                                                         choices = c("CSV" = ".csv",
                                                                     "HTML" = ".html"),
                                                         inline = TRUE))
                                     
                                   )
                   )
                   )))),
        
        column(width = 6,
               wellPanel(
                 fluidRow(gt_output(ns("all"))))
        )
      )
    )
  )
}

