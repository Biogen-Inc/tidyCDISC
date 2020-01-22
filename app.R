library(shiny)
library(shinyjs)
library(tidyverse)
library(tippy)

source("global.R")

ui <- 
  tagList(
    useShinyjs(),
     tags$head(
       tags$link(
         rel = "stylesheet",
         type = "text/css",
         href = "styles.css"
       ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css"
      )
    ),
    navbarPage(
      "TWO MODULES",
      tabPanel(
        title = "Data",
          dataUploadUI("datafile", "Import CSV")
      ),
      tabPanel(
        title = "Table Generator",
          chartUI("table_generator")
      )
    )
  )

server <- function(input, output, session) {
  
  # Increase allowed file size to 800MB
  options(shiny.maxRequestSize = 800*1024^2)
  
  # call the module with the function dataupload and the label we want to give it
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  # now we can render the table as the module we called
  output$table <- renderTable({ datafile() })
  
  # PASS datafile WITHOUT () INTO THE MODULE 
  table_generator <- callModule(chart, "table_generator", datafile = datafile)
  # now we can render the plot as the chart module we called in the line above
  output$all_rows <- renderUI({ table_generator() })

}

shinyApp(ui, server)