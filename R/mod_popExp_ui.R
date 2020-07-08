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
  tagList(
    h1("Population Explorer", align = "center"),
    br(), br(), br(),
    
    fluidRow(
      column(width = 3,
               checkboxInput(ns("adv_filtering"), span("Filter Data   ", icon("chevron-down w3-tiny")),  value = F),
               conditionalPanel(condition = "input.adv_filtering == true", ns = ns,
                                div(id = "custom_checkbox",
                                  selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM",
                                            multiple = TRUE, choices = NULL, selected = NULL)),
                                IDEAFilter::shiny_data_filter_ui(ns("data_filter"))
               ),
             
             wellPanel(
                      h4("Type of Chart:"),
                      br(),
                      radioButtons(ns("plot_type"), NULL, 
                                   choices = c("Scatter Plot", 
                                               "Spaghetti Plot", 
                                               "Box Plot")
                      )
                    ),
                    wellPanel(uiOutput(ns("plot_ui")))
             ),
             column(width = 9, wellPanel(plotOutput(ns("plot_output"))))
    )
  )
}


