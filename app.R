library(shiny)
library(shinyjs)
library(tidyverse)
library(tippy)

source("global.R")

ui <- 
  tagList(
    tags$head(
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
    ),
    useShinyjs(),
    navbarPage(#theme = "yeti.css",
               title = "IDEA",
               id = "navbarID",
      tabPanel(
        title = "Data",
          dataUploadUI("datafile", "Import CSV")
      ),
      tabPanel(
        title = "Table Generator",
          tableGeneratorUI("table_generator")
      ),
      tabPanel(
        title = "Population Explorer"
      ),
      tabPanel(
        title = "Individual Explorer"
      )
    ),
    # Custom styling to override theme
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # Add logo to top right corner
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a>`</div>');")),
    tags$script(src = "script.js"),
    tags$script(src = "recipe.js")
  )

server <- function(input, output, session) {
  
  # Increase allowed file size to 800MB
  options(shiny.maxRequestSize = 800*1024^2)
  
  # render the dataUpload module in Data tab
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  output$table <- renderTable({ datafile() })
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  output$all_rows <- renderUI({ table_generator() })

}

shinyApp(ui, server)