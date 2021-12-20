#' The dataUpload UI 
#' provides the interface for uploading ADSL data
#' and a table overview of the uploaded file
#'
#' @description A shiny Module.
#'
#' @return a shiny \code{\link[shiny]{tagList}} 
#' containing the filter ui
#'
#' @param id Internal parameters for {shiny}.
#'
#' @import shiny
#' @noRd
#' 
mod_dataUpload_ui <- function(id){
  ns <- shiny::NS(id)
  tagList(
    h1("Data Upload/Preview", align = "center"),
    br(), br(), br(),
    actionButton(ns("pilot"), "Use CDISC Pilot Data"),
    fluidRow(
      style = "padding: 20px",
      column(3,
             wellPanel(
               div(style="display: inline-block; ",h3("Data upload")),
               div(style="display: inline-block; float:right;",mod_dataComplyRules_ui("dataComplyRules_ui_1")),
               HTML("<br>ADSL file is mandatory & BDS/ OCCDS files are optional"),
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
  )
}


#' dataUpload Server Function stores 
#' the uploaded data as a list and 
#' is exported to be used in other modules
#'
#' @param input,output,session 
#' Internal parameters for {shiny}.
#' 
#' @return a list of dataframes 
#' \code{dd$dataframe} 
#' to be used in other modules
#' 
#' @import shiny
#' @importFrom haven zap_formats 
#' @importFrom haven read_sas
#' @importFrom stringr str_remove
#' 
#' @noRd
#' 
mod_dataUpload_server <- function(input, output, session){
  ns <- session$ns
  
  # initiate reactive values - list of uploaded data files
  # standard to imitate output of detectStandard.R
  dd <- reactiveValues()
  
  # modify reactive values when data is uploaded
  observeEvent(input$file, {
    
    data_list <- list()
    
    ## data list
    for (i in 1:nrow(input$file)){
      if(length(grep(".sas7bdat", input$file$name[i], ignore.case = TRUE)) > 0){
        data_list[[i]] <- haven::zap_formats(haven::read_sas(input$file$datapath[i])) %>%
          dplyr::mutate(dplyr::across(.cols = where(is.character),
                               .fns = na_if, y = ""))
      }else{
        data_list[[i]] <- NULL
      }
    }
    
    # names
    names(data_list) <- toupper(stringr::str_remove(input$file$name, ".sas7bdat"))
    
    
    
    # run that list of dfs through the data compliance module, replacing list with those that comply
    dl_comply <- callModule(mod_dataComply_server, 
                             id = NULL, #"dataComply_ui_1", 
                             datalist = reactive(data_list))
    
    if(length(names(dl_comply)) > 0){
      # append to existing reactiveValues list
      dd$data <-  c(dd$data, dl_comply) # dl_comply #
      
    }
    
    # set dd$current to FALSE for previous & TRUE for current uploads
    dd$current <- c(rep(FALSE, length(dd$current)), rep(TRUE, length(data_list)))
    
  })
  
  
  
  ### make a reactive combining dd$data & standard
  data_choices <- reactive({
    req(dd$data)
    #req(dd$standard)
    
    choices  <- list()
    for (i in 1:length(dd$data)){
      choices[[i]] <- names(dd$data)[i]
    }
    
    return(choices)
  })
  
  
  
  observeEvent(dd$data, {
    req(data_choices())
    
    vals <- data_choices()
    names(vals) <- NULL
    names <- data_choices()
    prev_sel <- lapply(reactiveValuesToList(input), unclass)$select_file  # retain previous selection
    
    output$radio_test <- renderUI(
      radioButtons(session$ns("select_file"), label = "Inspect Uploaded Data",
                   choiceNames = names, choiceValues = vals, selected = prev_sel))
    
  })
  
  # get selected dataset when selection changes
  data_selected <- eventReactive(input$select_file, {
    isolate({index <- which(names(dd$data)==input$select_file)[1]})
    dd$data[[index]]
  })
  
  studies <- reactive({ 
    unique(unlist(lapply(dd$data, `[[`, "STUDYID"))) 
  })
  
  output$multi_studies <- renderText({
    req(length(studies()) > 1)
    paste0("Warning: data uploaded from multiple studies: ", paste(studies(), collapse = " & "))
  }) 
  
  # upon a dataset being uploaded and selected, generate data preview
  output$datapreview_header <- renderUI({
    data_selected()
    isolate(data_name <- input$select_file)
    h3(paste("Data Preview for", data_name))
  })
  
  output$data_preview <- DT::renderDataTable({
    DT::datatable(data = data_selected(),
                  style="default",
                  class="compact",
                  extensions = "Scroller", options = list(scrollY=400, scrollX=TRUE))
  })
  
  observeEvent( input$pilot, {
    
    shinyjs::disable(id = "file")
    
    dd$data <- list(
      ADSL = tidyCDISC::adsl,
      ADVS = tidyCDISC::advs,
      ADAE = tidyCDISC::adae,
      ADLBC = tidyCDISC::adlbc,
      ADTTE = tidyCDISC::adtte
    )
    
    shinyjs::hide(id = "pilot")
  })
  
  ### return all data
  return(reactive(dd$data))
}
