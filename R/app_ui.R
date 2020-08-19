#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' 
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # List the first level UI elements here 
    navbarPage(title = div(id="logo-id","IDEA", img(src="www/IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
               tabPanel(
                 title = "Data",
                 mod_dataUpload_ui("dataUpload_ui_1")
               ),
               tabPanel(
                 title = "TableGenerator",
                 div(mod_tableGen_ui("tableGen_ui_1"), id = "tableGen")
               ),
               tabPanel(
                 title = "Population Explorer",
                 # mod_selectData_ui("selectData_ui_1"),
                 # mod_selectData_ui("popExp_ui_1"),
                 mod_popExp_ui("popExp_ui_1")
               ),
               tabPanel(
                 title = "Individual Explorer",
                 mod_indvExp_ui("indvExp_ui_1")
               )
      ),
  tags$script(
    HTML("var header = $('.navbar > .container-fluid');
                              header.append('<a href=\"mailto:example@email.com\"><img src=\"www/email.svg\" style=\"width:2.5%;height:2.5%;float:right;padding-top:5px;\"></a>')")
  ))
  }

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom waiter use_waiter
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
#' @importFrom shinyjs inlineCSS
#' @importFrom cicerone use_cicerone
#' @noRd
golem_add_external_resources <- function(){
  
  # source("R/global.R")
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  # golem tags$head start
  tags$head(
    
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'IDEA'
    ),
    
    tags$script(HTML(htmljs)),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyjs::inlineCSS(css),
    shinyjs::extendShinyjs(text = jscode),
    cicerone::use_cicerone()

  )

}