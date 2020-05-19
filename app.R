library(shiny)
library(shinyjs)
library(tidyverse)
# remotes::install_github("JohnCoene/tippy")
library(tippy)
library(rvest)
# devtools::install_github("MayaGans/IDEAFilter") # don't use for production
# drat::addRepo("aaron-clark") # add a repo where the source file (tar.gz) file is hosted on gitHub
# getOption("repos") # check if the repo was added
# install.packages("IDEAFilter")
library(IDEAFilter)
library(shinyTime) # for IDEAFilter
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
library(sjlabelled) 
library(data.table) 
library(gt)
library(shinyBS)

###############################################################
# make sure this repo exists before writing to manifest file!
###############################################################
# drat::addRepo("aaron-clark")
# getOption("repos")
# # devtools::install_github("rstudio/rsconnect")
# rsconnect::writeManifest()



options(shiny.sanitize.errors = FALSE)
options(bitmapType='cairo') 


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
               tags$head(
                 tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
               ),
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
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  
  # # Data compliance Modals: any time the reactive datalist() changes, run this code
  # callModule(dataComply, "comply_id", datalist = datafile) # ,stringsAsFactors = FALSE
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  output$all_rows <- renderUI({ table_generator() })
  
  # Population Explorer
  callModule(PopuExplor, id = "popul", datafile = datafile)
  
  # Individual Explorer
  user_dat <- callModule(IndvExpl1Initial, "indvl", datafile = datafile)
  usubjid  <- callModule(IndvExpl2SelPatno , "indvl", datafile = datafile,  loaded_adams = user_dat$my_loaded_adams, filtered_dat = user_dat$all_data) #, dataselected
  callModule(IndvExpl3CheckGroup,  "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  callModule(IndvExpl4ChartPlotly, "indvl", datafile,  loaded_adams = user_dat$my_loaded_adams, usubjid = usubjid, filtered_dat = user_dat$all_data)   #, dataselected
  
}

shinyApp(ui, server)
