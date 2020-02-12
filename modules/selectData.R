selectData <- function(input, output, session, datafile) {

  ns <- session$ns

  observe({

  req(!is.null(datafile()))

  sasdata <- toupper(names(datafile()))

  # Only select data that starts with AD followed by one or more alphanumerics
  sasdata <- names(which(sapply(sasdata,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9]+")) })))

  
  # remove ADSL from list
  if ("ADSL" %in% sasdata) {
    adslsave <- "ADSL"
    sasdata <- sasdata[sasdata != "ADSL"]
  } else {
    adslsave = " "
  }

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

    if (is.null(input$datalist)) {
      c("ADSL") 
    } else {
      if ("ADSL" %in% toupper(names(datafile()))) {
        # add ADSL back only if it was already in datafile()
        c("ADSL",input$datalist)
      } else {
        c(input$datalist)
      }
    } 
    }))
}
