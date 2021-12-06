#' popExp UI Function
#'
#' @description UI for population explorer: the filtering widget
#' as well as radio buttons for different plots. These radio buttons
#' are used to toggle between the child modules of each plot type, and 
#' a conditional panel of widgets based on the plot type
#'
#' @param id Internal parameters for {shiny}.
#' @param label Name of module
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom IDEAFilter shiny_data_filter
#' 
#' @family popExp Functions
#' @noRd
#' 
mod_popExp_ui <- function(id, label = "Population Explorer"){
  ns <- NS(id)
  tagList(
    div(style="display: inline-block; float:right;",
        actionButton(ns("help_sel") 
                     , label = NULL
                     , icon = icon("question-circle")
                     , class = "btn-start"
                     , style = "display: inline-block; float:right; margin-bottom:15px;"
        )),
    h1("Population Explorer", align = "center"),
    br(), br(), br(),
    
    fluidRow(
      column(width = 3,
               div(id = "pop_cic_adv_filtering", class="filter-accordion",
                 checkboxInput(ns("adv_filtering"), 
                   div(class="filter-container", span("Filter Data ", style="float:left;"), 
                        span(icon("chevron-down w3-tiny"), style="float:right;")),
                   value = F)
               ),
               conditionalPanel(condition = "input.adv_filtering == true", ns = ns,
                  div(id = "custom_checkbox",
                      
                      div(id = "pop_cic_apply_filters", materialSwitch(ns("apply_filters")
                                     , label = strong(em(h5("Apply Filters")))
                                     , status = "primary"
                                     , value = F)),
                      div(id = "pop_cic_filter_df", selectInput(ns("filter_df"),"Filter on Variable(s) in a loaded ADaM",
                                multiple = TRUE, choices = "ADSL", selected = "ADSL") ),
                      div(id = "pop_cic_data_filter", IDEAFilter::shiny_data_filter_ui(ns("data_filter"))))
               ),
             
             div(id = "pop_cic_chart_type", 
               h4("Type of Chart:"),
               wellPanel(
                        br(),
                        radioButtons(ns("plot_type"), NULL, 
                             choices = c("Scatter Plot", 
                                         "Spaghetti Plot", 
                                         "Box Plot",
                                         "Line plot - mean over time",
                                         "Heatmap - endpoint correlations"
                                         )
                        )
                      )
             ),

             div(id = "pop_cic_chart_inputs", 
               conditionalPanel("input.plot_type === 'Box Plot'", ns = ns, boxPlot_ui(ns("boxPlot"))),
               conditionalPanel("input.plot_type === 'Spaghetti Plot'", ns = ns, spaghettiPlot_ui(ns("spaghettiPlot"))),
               conditionalPanel("input.plot_type === 'Scatter Plot'", ns = ns, scatterPlot_ui(ns("scatterPlot"))),
               conditionalPanel("input.plot_type === 'Kaplan-Meier Curve'", ns = ns, km_ui(ns("km"))),
               conditionalPanel("input.plot_type === 'Line plot - mean over time'", ns = ns, linePlot_ui(ns("linePlot"))),
               conditionalPanel("input.plot_type === 'Heatmap - endpoint correlations'", ns = ns, heatmap_ui(ns("heatmap")))
             )
           ),
           column(width = 9,
                    div(id = "pop_cic_plot", 
                      wellPanel(
                        plotlyOutput(ns("plot_output"), height = 700),
                        div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("applied_filters")))
                        , br(), br()
                        ,DT::dataTableOutput(ns("plot_data"))
                        
                        )
                    )
                  )
    )
  )
}