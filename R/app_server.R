#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

  observeEvent(input$myBrowser , {
    if(str_detect(input$myBrowser, "IE")){
      showModal(tags$div(id="browserModal", modalDialog(footer = NULL,
                                                        glue("This web app doesn't function with Internet Explorer. Please use a modern browser such as Google Chrome.")
      )))
    }    
  })
  
  
  # disable tab2 on page load
  js$disableTab()
  
  observeEvent(datafile()$ADSL, {
    # enable tab2 when clicking the button
    js$enableTab()
  })
  
  # Increase allowed file size to 4GB
  options(shiny.maxRequestSize = 4096*1024^2)
  
  # render the dataUpload module in Data tab
  datafile <- callModule(mod_dataUpload_server, "dataUpload_ui_1", stringsAsFactors = FALSE)
  
  # # render the tablegenerator module using the datafile from dataupload as an input
  # table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  # 
  # output$all_rows <- renderUI({ table_generator() })
  # 
  # # Population Explorer
  # callModule(PopuExplor, id = "popul", datafile = datafile)
  # 
  # # Individual Explorer
  # user_dat <- callModule(IndvExpl1Initial, "indvl", datafile = datafile)
  # usubjid  <- callModule(IndvExpl2SelPatno , "indvl", datafile = datafile,  loaded_adams = user_dat$my_loaded_adams, filtered_dat = user_dat$all_data) #, dataselected
  # callModule(IndvExpl3CheckGroup,  "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  # callModule(IndvExpl4ChartPlotly, "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  
}
