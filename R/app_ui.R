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
                 mod_tableGen_ui("tableGen_ui_1")
               ),
               # tabPanel(
               #   title = "Population Explorer",
               #   mod_selectData_ui("selectData_ui_1"),
               #   mod_popExp_ui("popExp_ui_1")
               # ),
               tabPanel(
                 title = "Individual Explorer",
                 mod_indvExp_ui("indvExp_ui_1")
               )
      )
    )
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
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    waiter::use_waiter(), # include dependencies
    shinyjs::inlineCSS(css),
    shinyjs::extendShinyjs(text = jscode)
    
  )
  
  
}