#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#' 
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  observeEvent(input$myBrowser , {
    if(str_detect(input$myBrowser, "IE")){
      showModal(tags$div(id="browserModal", modalDialog(footer = NULL,
                                                        glue("This web app doesn't function with Internet Explorer. Please use a modern browser such as Google Chrome.")
      )))
    }    
  })
  
  
  # disable tab2 on page load
  shinyjs::js$disableTab()

  observeEvent(datafile()$ADSL, {
    # enable tab2 when clicking the button
    shinyjs::js$enableTab()
  })
  
  # Increase allowed file size to 4GB
  options(shiny.maxRequestSize = 4096*1024^2)
  
  # List the first level callModules here
  callModule(mod_dataComplyRules_server, "dataComplyRules_ui_1")
  datafile <- callModule(mod_dataUpload_server, "dataUpload_ui_1")
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(mod_tableGen_server, "tableGen_ui_1", datafile = datafile)
  
  output$all_rows <- renderUI({ table_generator() })
  
  # Individual Explorer
  user_dat <- callModule(mod_indvExp_server, "indvExp_ui_1", datafile = datafile)
  usubjid  <- callModule(mod_indvExpPat_server, "indvExp_ui_1", datafile = datafile,  loaded_adams = user_dat$my_loaded_adams, filtered_dat = user_dat$all_data)
  callModule(mod_indvExpPatEvents_server,  "indvExp_ui_1", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  callModule(mod_indvExpPatVisits_server, "indvExp_ui_1", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected

}
