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
               
               title = div(id="logo-id","IDEA", img(src="IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
               tabPanel(
                 title = "Data",
                 mod_dataUpload_ui("dataUpload_ui_1", "Import CSV") # ac golem: need import csv label?
               )
               # , tabPanel(
               #   title = "TableGenerator", id = 't_gen',
               #   tableGeneratorUI("table_generator")
               # ),
               # tabPanel(
               #   title = "Population Explorer",
               #   # dataUploadUI("popul", "Import CSV"),  
               #   selectDataUI(id = "popul"),
               #   PopuExplorUI(id = "popul")
               # ),
               # tabPanel(
               #   title = "Individual Explorer",
               #   # dataUploadUI("indvl", "Import CSV"),  
               #   # selectDataUI(id = "indvl"), # Removed - Issue 74
               #   IndvExplorUI(id = "indvl")
               # )
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
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    tags$head(
      tags$script(HTML(htmljs)),
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      # tags$head(tags$link(rel="shortcut icon", href="IDEA_FAVICON.ico")), # shouldn't need anymore -- ac golem
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$script(src="accordion.js", type="text/javascript")
    ),
    useShinyjs(),
    use_waiter(), # include dependencies
    extendShinyjs(text = jscode),
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css"),
    
    
    # navbarpage was here
    
    # Custom styling to override theme
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # Add logo to top right corner
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a>`</div>');")),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js"),
    tags$style(HTML("
 
                    #browserModal .modal-dialog,
                    #browserModal .modal-body,
                    #browserModal .modal-footer {
                    background-color: #CF000F;
                    border-color: #CF000F;
                    color: white;
                    font-size: 20px;
                    }
                    
                    ")),
    inlineCSS(css),
    tags$head(tags$script(src = "analytics.js"))
  )
  
  
}

