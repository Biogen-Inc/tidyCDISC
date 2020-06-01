selectData <- function(input, output, session, datafile) {

  ns <- session$ns

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
