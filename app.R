library(shiny)
library(shinyjs)
library(tidyverse)
library(tippy)
library(rvest)
library(IDEAFilter)
library(haven)
library(DT)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(janitor)
library(rtf)
library(shinythemes)
library(rmarkdown)
library(shinytest)
library(reactable)
library(waiter)
library(timevis)
library(glue)

options(shiny.sanitize.errors = FALSE)
options(bitmapType='cairo') 

jscode <- "
shinyjs.disableTab = function() {
    var tabs = $('.nav').find('li:not(.active) a');
    tabs.bind('click.tab', function(e) {
        e.preventDefault();
        return false;
    });
    tabs.addClass('disabled');
}
shinyjs.enableTab = function(param) {
    var tab = $('.nav').find('li:not(.active) a');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
}
"

htmljs <- "
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
// browser detection from https://stackoverflow.com/a/5918791/8099834
navigator.sayswho= (function(){
var ua= navigator.userAgent, tem, 
M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
if(/trident/i.test(M[1])){
tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
return 'IE '+(tem[1] || '');
}
if(M[1]=== 'Chrome'){
tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
}
M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
return M.join(' ');
})(); 
// pass browser info from JS to R
Shiny.onInputChange('myBrowser', navigator.sayswho); 
});"

# if we every add another type of event to the events table,
# expand this selection to cover all our bases
my_cols <- brewer.pal(7,"Pastel2")
css <- paste0("
  .nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
  }
  
  .vis-item.DS { background-color: ",my_cols[1],"; }
  .vis-item.CM { background-color: ",my_cols[2],"; }
  .vis-item.AE { background-color: ",my_cols[3],"; }
  .vis-item.LB { background-color: ",my_cols[4],"; }
  .vis-item.MH_MH { background-color: ",my_cols[5],"; }
  .vis-item.MH_FDH { background-color: ",my_cols[6],"; }
  .vis-item.MH_DH { background-color: ",my_cols[7],"; }
")
css
source("global.R")

ui <- 
  tagList(
    tags$head(
      tags$script(HTML(htmljs)),
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$head(tags$link(rel="shortcut icon", href="IDEA_FAVICON.ico")),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
    ),
    useShinyjs(),
    use_waiter(), # include dependencies
    extendShinyjs(text = jscode),
    navbarPage(theme = "yeti.css",
               title = div(id="logo-id","IDEA", img(src="IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
               tabPanel(
                 title = "Data",
                 dataUploadUI("datafile", "Import CSV")
               ),
               tabPanel(
                 title = "TableGenerator", id = 't_gen',
                 tableGeneratorUI("table_generator")
               ),
               tabPanel(
                 title = "Population Explorer",
                 # dataUploadUI("popul", "Import CSV"),  
                 selectDataUI(id = "popul"),
                 PopuExplorUI(id = "popul")
               ),
               tabPanel(
                 title = "Individual Explorer",
                 # dataUploadUI("indvl", "Import CSV"),  
                 # selectDataUI(id = "indvl"), # Removed - Issue 74
                 IndvExplorUI(id = "indvl")
               )
    ),
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

server <- function(input, output, session) {
  
  observeEvent(input$myBrowser , {
    if(str_detect(input$myBrowser, "IE")){
      showModal(tags$div(id="browserModal", modalDialog(
        glue("Features of this app will not work with {input$myBrowser}")
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
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  output$all_rows <- renderUI({ table_generator() })
  
  # Population Explorer
  callModule(PopuExplor, id = "popul", datafile = datafile)
  
  # Individual Explorer
  user_loaded_adams <- callModule(IndvExpl1Initial, "indvl", datafile = datafile)
  usubjid  <- callModule(IndvExpl2SelPatno , "indvl", datafile = datafile,  loaded_adams = user_loaded_adams) #, dataselected
  callModule(IndvExpl3CheckGroup,  "indvl", datafile,  loaded_adams = user_loaded_adams, usubjid = usubjid)   #, dataselected
  callModule(IndvExpl4ChartPlotly, "indvl", datafile,  loaded_adams = user_loaded_adams, usubjid = usubjid)   #, dataselected
  
}

shinyApp(ui, server)