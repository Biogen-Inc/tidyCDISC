#' selectData UI Function
#'
#' @description UI for selecting datasets.
#'
#' @param id Internal parameters for {shiny}.
#' @param datafile A list of dataframes
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets pickerInput 
#' 
mod_selectData_ui <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}
    
#' selectData Server Function
#' @description server for selecting datasets.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param datafile A list of dataframes.
#'
#' @return reactive data from selection.
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets updatePickerInput 
#'
mod_selectData_server <- function(input, output, session, datafile){
  ns <- session$ns
 
  # print("in mod_selectData. session id is...")
  # print(session$ns(""))

  observeEvent(datafile(), {
    
    req(!is.null(datafile()))
    
    sasdata <- toupper(names(datafile()))
    
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    sasdata <- names(which(sapply(sasdata,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    
    # Update the picker input list
    updatePickerInput(
      session = session,
      inputId = "datalist",
      choices = sasdata
    )
    
  })
  
  # show action button done when selected
  observeEvent(input$datalist,{
    # print("input$datalist observed")
    shinyjs::show(id="done")
  })
  
  # hide action button done when clicked
  observeEvent(input$done, {
    # print("input$done observed")
    shinyjs::hide(id="done")
  })
  
  # return reactive data on input button click
  return(eventReactive(input$done, { 
    
    input$datalist
    
  }))
}
    
## To be copied in the UI
# mod_selectData_ui("selectData_ui_1")
    
## To be copied in the server
# callModule(mod_selectData_server, "selectData_ui_1")
 
