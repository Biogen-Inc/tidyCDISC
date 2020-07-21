
#' dataComplyRules UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#' @param showRules If TRUE, the UI will contain the returned shiny tagList will
#'   contain a help button and respective modal when clicked. If FALSE, the
#'   dataComply module will not include a help button to display rules.
#'
#' @importFrom shiny NS tagList actionButton
#' 
#' @return An shiny tagList
#'
#' @family dataComply Functions
#'   
mod_dataComplyRules_ui <- function(id, showRules = T){
  ns <- NS(id)
  tagList(
    if(showRules == T){
        actionButton(ns("clickRules") 
                   , label = NULL
                   , icon = icon("question-circle")
                   , class = "btn-start"
      )
    }
  )
}
    
#' dataComplyRules Server Function
#'
#' Situates output from gather_rules function in
#' "mod_dataComplyRules_fct_helpers.R" to a modal condiational on clicking
#' "help".
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param modal_title A text string to be displayed as the modal's title
#'   
#' @import shiny
#'   
#' @family dataComply Functions
#'   
mod_dataComplyRules_server <- function(input, output, session,
                                       modal_title = "ADaM-ish Upload Rules"){
  ns <- session$ns
  
  observeEvent(input$clickRules, {
    
    showModal( modalDialog(
      title = div(style = "text-align:center; font-weight:bold;",modal_title),
      footer =
          tagList(
            div(style = "text-align:center; font-size: 12px;",
                HTML("If <b>Required</b> variables are missing, the file will not upload<br>If <b>Recommended</b> variables are missing, built-in features may not render")
            ),
            modalButton("Dismiss")
          )
        ,
      
      # Content of the Modal
      rulesUI # defined in "mod_dataComplyRules_fct_helpers.R"
    ))
    
  })
}
    
## To be copied in the UI -- done
# mod_dataComplyRules_ui("dataComplyRules_ui_1")
    
## To be copied in the server -- done
# callModule(mod_dataComplyRules_server, "dataComplyRules_ui_1")
 
