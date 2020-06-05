#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_dataComplyRules_server, "dataComplyRules_ui_1")
  datafile <- callModule(mod_dataUpload_server, "dataUpload_ui_1")
  
  
  # # Individual Explorer
  user_dat <- callModule(mod_indvExp_server, "indvExp_ui_1", datafile = datafile)
  usubjid  <- callModule(mod_indvExpPat_server, "indvExp_ui_1", datafile = datafile,  loaded_adams = user_dat$my_loaded_adams, filtered_dat = user_dat$all_data)
  # callModule(mod_indvExpPatEvents_server,  "indvExp_ui_1", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  # callModule(mod_indvExpPatVisits_server, "indvExp_ui_1", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected

}
