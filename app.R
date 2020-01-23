library(shiny)
library(shinyjs)
library(tidyverse)
library(tippy)

source("global.R")

ui <- 
  tagList(
    useShinyjs(),
    navbarPage(theme = "yeti.css",
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
    tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"logo.svg\" alt=\"alt\" style=\"float:right;width:66px;height:41px;\"> </a>`</div>');"))
  )

server <- function(input, output, session) {
  
  # Increase allowed file size to 800MB
  options(shiny.maxRequestSize = 800*1024^2)
  
  # call the module with the function dataupload and the label we want to give it
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  # now we can render the table as the module we called
  output$table <- renderTable({ datafile() })
  
  # PASS datafile WITHOUT () INTO THE MODULE 
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  # now we can render the plot as the chart module we called in the line above
  output$all_rows <- renderUI({ table_generator() })

}

shinyApp(ui, server)