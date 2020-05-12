
dataUploadUI <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             wellPanel(
               h3("Data upload"),
               "Uploading an ADSL file is mandatory, all additional BDS files are optional",
               fileInput(ns("file"), "Upload sas7bdat files",accept = c(".sas7bdat"), multiple = TRUE),
               uiOutput(ns("radio_test"))
             )
      ),
      column(6,
             fluidRow(
               wellPanel(
                 span(textOutput(ns("multi_studies")), style="color:red;font-size:20px"),
                 uiOutput(ns("datapreview_header")),
                 div(DT::dataTableOutput(ns("data_preview")), style = "font-size: 75%")
               )
             )
      )
    )
    ,dataComplyUI(id = "comply_id")
  )
}
