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
    navbarPage(theme = "yeti.css",
  
               title = div(id="logo-id","IDEA", img(src="www/IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
               tabPanel(
                 title = "Data",
                 mod_dataUpload_ui("dataUpload_ui_1") # ac golem: need import csv label?
               )
               # , tabPanel(
               #   title = "TableGenerator", id = 't_gen', # ac golem: make tableGen? Or need?
               #   mod_tableGen_ui("tableGen_ui_1")
               # ),
               # tabPanel(
               #   title = "Population Explorer",
               #   # dataUploadUI("popul", "Import CSV"),  
               #   mod_selectData_ui("selectData_ui_1"),
               #   mod_popExp_ui("popExp_ui_1")
               # ),
               # tabPanel(
               #   title = "Individual Explorer",
               #   mod_indvExp_ui("indvExp_ui_1")
               # )
    ),
    
    ##############################################################################
    # ac golem: the order the code compiles here is important, so I'm including  
    # the items below instead of in the golem_add_external_resources()" section. 
    # After we get the app working, we should attempt to place them there to see
    # if it will work.
    ##############################################################################
    
    # Custom styling to override theme
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # Add logo to top right corner
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"www/logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a></div>');")),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js"),
    shinyjs::inlineCSS(css),
    tags$head(tags$script(src = "analytics.js"))
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
    
    favicon(), # from golem's use_favicon in 01_dev... doesn't work
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'IDEA'
    ),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
    
    tags$head(
      # tags$img(src = "www/red_x.png"), # test works
      tags$script(HTML(htmljs)),
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$script(src="accordion.js", type="text/javascript")
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(), # include dependencies
    shinyjs::extendShinyjs(text = jscode),
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css"),
    
    #######################
    # navbarpage was here
    #######################
    
  ) # golem tags$head end
  
  
}