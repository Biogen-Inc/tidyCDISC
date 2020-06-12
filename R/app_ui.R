#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
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
               #   # dataUploadUI("popul", "Import CSV"),  
               #   mod_selectData_ui("selectData_ui_1"),
               #   mod_popExp_ui("popExp_ui_1")
               # ),
               tabPanel(
                 title = "Individual Explorer",
                 mod_indvExp_ui("indvExp_ui_1")
               )
    ),
    
    # Add logo to top right corner
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"www/logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a></div>');")),
    shinyjs::inlineCSS(css)
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
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'IDEA'
    ),

    tags$script(HTML(htmljs)),
    tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyjs::extendShinyjs(text = jscode)
    
  )
  
  
}