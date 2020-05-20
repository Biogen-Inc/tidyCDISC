selectDataUI <- function(id, label = "Select Datasets") {
  ns <- NS(id)
  
  tagList(
    # add fluidRow so as the two widgets line up side by side each other
    fluidRow(
      column(2,div(style = "height:10px;"),
      # pickerInput(ns("datalist"),label = h5("Select Datasets"), choices=NULL, multiple = TRUE, 
      pickerInput(ns("datalist"),label = NULL, choices=NULL, multiple = TRUE, 
      options = list(`actions-box` = TRUE,title = "Select Data")) 
      ),
      column(2,div(style = "height:10px;"),
              actionButton(ns("done"), "Click when done") 
      )
    ) # fluidRow
  ) #tagList
}