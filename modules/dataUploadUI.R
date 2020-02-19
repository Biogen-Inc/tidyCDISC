
dataUploadUI <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             wellPanel(
               h3("Data upload"),
               h4("Uploading an ADSL file is mandatory, all additional BDS files are optional"),
               fileInput(ns("file"), "Upload a sas7bdat files",accept = c(".sas7bdat"), multiple = TRUE),
               uiOutput(ns("radio_test"))
             )
      ),
      column(6,
             fluidRow(
               wellPanel(
                 uiOutput(ns("datapreview_header")),
                 div(DT::dataTableOutput(ns("data_preview")), style = "font-size: 75%")
               )
             )
      )
    )
  )
}
