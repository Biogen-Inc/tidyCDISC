selectDataUI <- function(id, label = "Select Datasets") {
  ns <- NS(id)
  
  tagList(
    # add fluidRow so as the two widgets line up side by side each other
    fluidRow(
      column(3,div(style = "height:10px;"),
      pickerInput(ns("datalist"),label = h5("Select Datasets"), choices=NULL, multiple = TRUE, 
      options = list(`actions-box` = TRUE,
      title = 'Select zero or more datasets below')) ),
      column(3,div(style = "height:50px;"),
              actionButton(ns("done"), "Click when done") )
    ) # fluidRow
  ) #tagList
}