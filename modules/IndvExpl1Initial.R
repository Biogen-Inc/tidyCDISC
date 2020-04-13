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
  
  
  
  
  # Create waiting screen over this fluidRow and column area... where IDEAFilter displays
  # waiting_screen <- tagList(
  #   spin_folding_cube(),
  #   h4("Hold on a bit while we merge datasets...")
  # )
  
  # upon selection of data set(s) to filter... combine to feed shiny_data_filter module with those selected
  pre_processed_data <- eventReactive(input$filter_df, {
    
    req(input$adv_filtering == T)
    # validate(need(input$filter_df, "Must select a loaded ADaM for advanced filtering"))
    
    # waiter_show(html = waiting_screen, color = "lightblue")
    # Sys.sleep(.5)
    
    # grab only df's included in the filter
    select_dfs <- datafile()[input$filter_df]
    
    non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
      # note: join may throw some warnings if labels are different between two datasets, which is fine! Ignore

    BDS <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
    PARAMCD_dat <- map(BDS, ~ if(!"CHG" %in% names(.)) {update_list(., CHG = NA)} else {.})
    
    
    cat(paste("\nNonbds:",names(non_bds)))
    cat(paste("\nBDS:",names(BDS)))
    cat("\n")

    if (!is_empty(PARAMCD_dat)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from")  %>%
        distinct(.keep_all = T)
      
      if (!is_empty(non_bds)){
        combined_data <- full_join(non_bds %>% reduce(full_join), all_PARAMCD) #, by = "USUBJID", suffix = c(".x", ".y")
      } else {
        combined_data <-all_PARAMCD
      }
    } else {
        combined_data <- non_bds %>% reduce(full_join)
    }
    
    # Sys.sleep(.5)
    # waiter_hide()
    
    return(
      if(!is_empty(input$filter_df)){
        combined_data
      } else{
        datafile()$ADSL
        # NULL
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
  

  #     # now pin point variable that was filtered and merge it with tab_data and filter tab data
  #     # 1. Create a DF with only the columns we need for this tab (from applicable loaded_adams) plus the columns
  #          # they originally had in common with the filtered_data() data set
  #     # 2. Do a semi_join
  # 
  
  # not using anymore
  # output$filter_header <- renderText({
  #   req(!is.null(filter_header()))
  #   paste0("If desired, pre-filter USUBJID by variables from loaded data sets")
  # })
  # not using anymore
  # output$filter_bds_header <- renderText({
  #   req(!is.null(all_data()) & length(BDS) > 0)
  #   paste0("For convenience: AVAL, CHG, and BASE values are displayed for each PARAMCD")
  # })
  
  observe({
    # make sure selectData has been run
    req(!is.null(datafile())) #74
    
    # # stringr
    # # grab the output
    # orig_code <- paste(capture.output(attr(filtered_data(), "code")),collapse = "")
    # # orig_code <- 'feed_filter %>% filter(ACTIVE1 %in% c(NA, "0")) %>% filter(var1 == 2)' #
    # # convert double quotes to single quotes
    # code_text <- gsub('\"',"\'",orig_code)
    # 
    # # find the character position for the end of the string
    # len <- nchar(code_text)
    # 
    # # find the start of the variable expressions using position of "filter"
    # f_loc <- str_locate_all(code_text,"filter\\(")
    # filter_loc <- as_tibble(f_loc[[1]])
    # var_st <- filter_loc$end + 1
    # 
    # # find the end of variable expression susing position of "%>%"
    # p_loc <- str_locate_all(code_text,"\\%\\>\\%") # have to use this
    # pipe_loc <- as_tibble(p_loc[[1]])
    # num_pipes <- nrow(pipe_loc)
    # var_end <- c(pipe_loc$start[ifelse(num_pipes == 1, 1, 2):num_pipes] - 3, len - 1) # ifelse(num_pipes == 1, 1, 2)
    # 
    # # use map2, to apply multiple arguments to the substr function, returing a list
    # filter_vectors <- map2(.x = var_st, .y = var_end, function(x,y) substr(code_text,x,y))
    # my_msgs <- filter_vectors[!(is.na(filter_vectors) | filter_vectors == "")] # get rid of NA msgs
    # 
    # # clean up messages to read more naturally
    # disp_msg <- gsub("\\%in\\%","IN",
    #                  gsub("c\\(","\\(",
    #                       gsub("NA","Missing",
    #                            gsub("na","Missing",
    #                                 gsub("   "," ", # 3 spaces
    #                                      gsub("  "," ", # 2 spaces
    #                                           gsub("\\|","OR",
    #                                                gsub("\\&","AND",
    #                                                     my_msgs
    #                                                ))))))))
    # # disp_msg
    # # cat(paste("\n1:",attr(filtered_data(), "code")$column_name))
    # # cat(paste("\n"))
    # cat(paste("\n1:",orig_code))
    # cat(paste("\n2:",var_st))
    # cat(paste("\n3:",var_end))
    # cat(paste0("\n4:\n",disp_msg,"\n"))
    
  
    # The rest of the widgets will be shown after the USUBJID has been selected
    subj <- unique(filtered_data()[, "USUBJID"]) # unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
    
    updateSelectInput(
      session = session,
      inputId = "selPatNo",
      choices = c(" ",subj),
      selected = " "
    )
    
    
    # Hide widgets until the input file has been selected
    shinyjs::show(id = "selPatNo")
    shinyjs::hide(id = "demog_header")
    shinyjs::hide(id = "subjid_subtitle1")
    shinyjs::hide(id = "demogInfo")
    shinyjs::hide(id = "hr2")
    
    shinyjs::hide(id = "events_header")
    shinyjs::hide(id = "subjid_subtitle2")
    shinyjs::hide(id = "events_apply_filter")
    shinyjs::hide(id = "checkGroup")
    shinyjs::hide(id = "eventsPlot")
    shinyjs::hide(id = "events_tv_caption1")
    shinyjs::hide(id = "events_tv_caption2")
    # shinyjs::hide(id = "applied_filters")
    shinyjs::hide(id = "eventsTable")
    shinyjs::hide(id = "hr3")
    
    shinyjs::hide(id = "plot_header")
    shinyjs::hide(id = "subjid_subtitle3")
    # shinyjs::hide(id = "bds_remove_filter")
    shinyjs::hide(id = "plot_adam")
    shinyjs::hide(id = "plot_param")
    shinyjs::hide(id = "visit_var")
    shinyjs::hide(id = "plot_hor")
  })
  
  # pass the filtered data from above, but also filtered by 
  return_filtered <- reactive({
    if(input$selPatNo != " "){
      filtered_data() %>% filter(USUBJID == input$selPatNo)
    } else {
      filtered_data()
    }
  })
  
  
  return(list(my_loaded_adams = my_loaded_adams, all_data = return_filtered)) #isolate(rv$all_data)))
} # IndvExpl1Initial
