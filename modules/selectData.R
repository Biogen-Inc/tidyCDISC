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
  
  # hide all the widgets
  widgets <- c("adv_filtering","clearplot","Parmstag","radio","selPrmCode","groupbox","groupbyvar","selxvar","selyvar","selzvar","seltimevar",
               "responsevar","AddPoints","animate","animateby","numBins","AddLine","AddSmooth",
               "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill")
  
  # hide all the widgets using an anonymous function
  map(widgets, function(x) shinyjs::hide(x))
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
