#' popExp UI Function
#'
#' @description UI for population explorer: the filtering widget
#' as well as radio buttons for different plots. These radio buttons
#' are used to toggle between the child modules of each plot type, and 
#' a conditional panel of widgets based on the plot type
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label Name of module
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom IDEAFilter shiny_data_filter
#' 
#' @family popExp Functions
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
                                            multiple = TRUE, choices = NULL, selected = "ADSL"),
                                  IDEAFilter::shiny_data_filter_ui(ns("data_filter")))
                                
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

                    #wellPanel(uiOutput(ns("plot_ui")))
             wellPanel(
               conditionalPanel("input.plot_type === 'Box Plot'", ns = ns, boxPlot_ui(ns("boxPlot"))),
               conditionalPanel("input.plot_type === 'Spaghetti Plot'", ns = ns, spaghettiPlot_ui(ns("spaghettiPlot"))),
               conditionalPanel("input.plot_type === 'Scatter Plot'", ns = ns, scatterPlot_ui(ns("scatterPlot")))
             )
            ),
             column(width = 9, wellPanel(plotlyOutput(ns("plot_output"))))
    )
  )
}