IndvExpl1Initial <- function(input, output, session, datafile, dataselected){
  # Initial processing
  
  ns <- session$ns
  rv = reactiveValues(processed_data = NULL)
  
  my_loaded_adams <- reactive({
    # Only select data that starts with AD followed by one or more alphanumerics or underscore
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  ###
  # Need to create a file that contains all the pertinent information users may want to filter on in this tab
  ###
  # Here the user will be able to filter the data in order to select a subject. After that, the user will have
  # a checkbox next to each section (1) Patient Events by date & (2) Patient Metrics by Visit in order to apply
  # those filters to that data module. By default, they will be applied.
  ###
  
  
  observe({ #Event(input$adv_filtering,
    req(input$adv_filtering == T)
    # selectInput(ns("filter_df"),"Filter on Variable in a loaded ADaM", multiple = TRUE,
    #             choices = NULL, selected = NULL)
    updateSelectInput("filter_df", session = session, choices = as.list(my_loaded_adams()), selected = "ADSL") #
  })
  
  # upon selection of data set(s) to filter... combine to feed shiny_data_filter module with those selected
  pre_processed_data <- eventReactive(input$filter_df, {
    
    req(input$adv_filtering == T)

    # grab only df's included in the filter
    select_dfs <- datafile()[input$filter_df]
    
    non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
      # note: join may throw some warnings if labels are different between two datasets, which is fine! Ignore

    BDS <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
    PARAMCD_dat <- map(BDS, ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
    

    if (!is_empty(PARAMCD_dat)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from")  %>%
        distinct(.keep_all = T)
      
      if (!is_empty(non_bds)){
        combined_data <- inner_join(non_bds %>% reduce(inner_join), all_PARAMCD) #, by = "USUBJID", suffix = c(".x", ".y")
      } else {
        combined_data <-all_PARAMCD
      }
    } else {
        combined_data <- non_bds %>% reduce(inner_join)
    }
    
    return(
      if(!is_empty(input$filter_df)){
        combined_data
      } else{
        datafile()$ADSL
      }
    )
  })
  observe({
    req(!is.null(datafile()))
    if(input$adv_filtering){
      rv$processed_data <- pre_processed_data()
    } else {
      rv$processed_data <- datafile()$ADSL
    }
  })
  feed_filter <- reactive({rv$processed_data})
  
  
  
  
  # IDEAFilter
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  


  
  observe({
    # make sure selectData has been run
    req(!is.null(filtered_data())) #74 #datafile()

    # The rest of the widgets will be shown after the USUBJID has been selected
    subj <- unique(filtered_data()$USUBJID) # unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
    
    updateSelectInput(
      session = session,
      inputId = "selPatNo",
      choices = c(" ",subj),
      selected = " "
    )
    
    # Hide widgets until the input file has been selected
    shinyjs::show(id = "selPatNo")
    
    hide_init <- c("demog_header", "subjid_subtitle1", "demogInfo", "mytabs", "events_header",
                 "subjid_subtitle2", "events_apply_filter","checkGroup","eventsPlot",
                 "events_tv_caption1","events_tv_caption2","eventsTable",
                 "plot_header", "subjid_subtitle3", "plot_adam", "event_type_filter", "plot_param",
                 "visit_var", "plot_hor", "display_dy", "overlay_events", "overlay_event_vals", ""
                 )
    map(hide_init, ~ shinyjs::hide(.x))
    
    # # shinyjs::hide(id = "applied_filters")
    output$display_dy <- renderUI({NULL})
    
  })
  
  # pass the filtered data from above, but also filtered by 
  return_filtered <- reactive({
    if(input$selPatNo != ""){
      filtered_data() %>% filter(USUBJID == input$selPatNo)
    } else {
      filtered_data()
    }
  })
  
  
  return(list(my_loaded_adams = my_loaded_adams, all_data = return_filtered)) #isolate(rv$all_data)))
} # IndvExpl1Initial
