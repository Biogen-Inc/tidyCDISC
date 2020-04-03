IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns

  
  my_loaded_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  
  observe({
    # make sure selectData has been run
    req(!is.null(datafile())) #74
    
    # Need to create a file that contains all the pertinent information users may want to filter on in this tab
    # processed_data <-
    
    all_data <- callModule(
      shiny_data_filter,
      "data_filter", #whatever you named the widget
      data = processed_data, #the name of your pancaked data
      verbose = FALSE)
    
    
    # The rest of the widgets will be shown after the USUBJID has been selected
    subj <- unique(all_data[, "USUBJID"]) # unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
    
    updateSelectInput(
      session = session,
      inputId = "selPatNo",
      choices = c(" ",subj),
      selected = " "
    )
    shinyjs::show(id = "selPatNo")
    # Hide widgets until the input file has been selected
    shinyjs::hide(id = "demog_header")
    shinyjs::hide(id = "subjid_subtitle1")
    shinyjs::hide(id = "demogInfo")
    shinyjs::hide(id = "hr2")
    shinyjs::hide(id = "events_header")
    shinyjs::hide(id = "subjid_subtitle2")
    shinyjs::hide(id = "checkGroup")
    # shinyjs::hide(id = "plot_events_timeline")
    shinyjs::hide(id = "eventsPlot")
    shinyjs::hide(id = "events_tv_caption1")
    shinyjs::hide(id = "events_tv_caption2")
    shinyjs::hide(id = "eventsTable")
    shinyjs::hide(id = "hr3")
    shinyjs::hide(id = "plot_header")
    shinyjs::hide(id = "subjid_subtitle3")
    shinyjs::hide(id = "plot_adam")
    shinyjs::hide(id = "plot_param")
    shinyjs::hide(id = "visit_var")
    shinyjs::hide(id = "plot_hor")
  })
  
  # return(dataselected) # #74
  return(my_loaded_adams) #74
  
} # IndvExpl1Initial
