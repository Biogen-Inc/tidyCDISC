#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_dataComplyRules_server, "dataComplyRules_ui_1")
  callModule(mod_dataUpload_server, "dataUpload_ui_1")
}
