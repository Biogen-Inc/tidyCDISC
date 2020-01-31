selectData <- function(input, output, session, datafile) {

  observe({
    
  sasdata <- toupper(names(datafile()))

  # remove ADSL from list
  if ("ADSL" %in% sasdata) {
    # x[x != "F"]
    sasdata <- sasdata[sasdata != "ADSL"]
  }

  # Update the picker input list
  updatePickerInput(
    session = session,
    inputId = "datalist",
    choices = sasdata
  )
})

  # return reactive data on input button click, and hide the widgets
    return(eventReactive(input$done, { 
    # hide the helptext and datalist after clicking on done
    shinyjs::hide(id="datalist")
    shinyjs::hide(id="done")
    
    if (is.null(input$datalist)) {
      c("ADSL") 
    } else {
      c("ADSL",input$datalist) 
    } 
    }))
}
