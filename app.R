library(shiny)
library(shinyjs)
library(tidyverse)
library(tippy)
library(rvest)
library(shinyWidgets)
library(formattable)
library(haven)
library(DT)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(broom)
library(data.table)

source("global.R")

ui <- 
  tagList(
    tags$head(
      tags$link(rel = "//code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
      tags$head(tags$link(rel="shortcut icon", href="IDEA_FAVICON.ico")),
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js")
    ),
    useShinyjs(),
    navbarPage(theme = "yeti.css",
               title = div(id="logo-id","IDEA", img(src="IDEA_ICON.png", style="float:left; padding-right:3px; height:25px; width:30px")), 
               id = "navbarID",
               windowTitle = "IDEA",
      tabPanel(
        title = "Data",
          dataUploadUI("datafile", "Import CSV")
      ),
      tabPanel(
        title = "Table Generator",
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
		  selectDataUI(id = "indvl"),
		  IndvExplorUI(id = "indvl")
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
  
  # Increase allowed file size to 4GB
  options(shiny.maxRequestSize = 4096*1024^2)
  
  # render the dataUpload module in Data tab
  datafile <- callModule(dataUpload, "datafile", stringsAsFactors = FALSE)
  
  # render the tablegenerator module using the datafile from dataupload as an input
  table_generator <- callModule(tableGenerator, "table_generator", datafile = datafile)
  output$all_rows <- renderUI({ table_generator() })
  
  # Individual Explorer
  # datafile <- callModule(dataUpload, "indvl", stringsAsFactors = FALSE)
  # dataselected <- callModule(selectData, "indvl", datafile)
  dataselected <- callModule(IndvExpl1Initial,   "indvl", datafile)
  usubjid  <- callModule(IndvExpl2SelPatno , "indvl", datafile, dataselected)
  callModule(IndvExpl3CheckGroup,  "indvl", datafile, dataselected, usubjid = usubjid)
  callModule(IndvExpl4ChartPlotly, "indvl", datafile, dataselected, seltypes = seltypes, usubjid = usubjid)

  # Population Explorer
  # datafile <- callModule(dataUpload, "popul", stringsAsFactors = FALSE)
  # dataselected <- callModule(selectData, "indvl", datafile)
  callModule(PopuExplor, id = "popul", datafile = datafile, dataselected = dataselected)

}

shinyApp(ui, server)