
dataUploadUI <- function(id, label = "CSV file") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             wellPanel(
               h3("Data upload"),
               fileInput(ns("file"), "Upload a sas7bdat file",accept = c(".sas7bdat"), multiple = TRUE),
               radioButtons(ns("select_file"),"Inspect Uploaded Data",
                            choiceNames = preload_data_list$display,
                            choiceValues = names(preload_data_list$data))
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
