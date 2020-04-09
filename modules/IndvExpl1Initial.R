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
  
  ###
  # Need to create a file that contains all the pertinent information users may want to filter on in this tab
  ###
  # Here the user will be able to filter the data in order to select a subject. After that, the user will have
  # a checkbox next to each section (1) Patient Events by date & (2) Patient Metrics by Visit in order to apply
  # those filters to that data module. By default, they will be applied.
  ###
  
  # find datasets that don't have PARAMCD, and merge them together by USUBJID
  non_adsl <- reactive({
    req(!is.null(datafile())) #74
    datafile()[sapply(datafile(), function(x) !("PARAMCD" %in% colnames(x)))] %>%
      reduce(full_join) #, by = "USUBJID", suffix = c(toupper(names(datafile()$.x)), toupper(names(datafile()$.y))) )
    # may throw some warnings if labels are different between two datasets, which is fine
  })
  BDS <- reactive({
    req(!is.null(datafile())) #74
    datafile()[sapply(datafile(), function(x) "PARAMCD" %in% colnames(x))]
    })
  
  processed_data <- reactive({
    
    req("ADSL" %in% names(datafile()))

    PARAMCD_dat <- map(BDS(), ~ if(!"CHG" %in% names(.)) update_list(., CHG = NA) else .)
    
    if (!is_empty(PARAMCD_dat)) {
      # Bind all the PARAMCD files 
      all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from")  %>%
        distinct(.keep_all = T)
      # may throw some warnings if labels are different between two datasets, which is fine
      combined_data <- full_join(non_adsl(), all_PARAMCD) #, by = "USUBJID", suffix = c(".x", ".y")
      
    } else {
      combined_data <- non_adsl() #%>%
        #mutate(data_from = "Non-BDS") #, PARAMCD = NA, AVAL = NA, BASE = NA, CHG = NA)
    }
  })
  
  
  
  all_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = processed_data, # the name of your pancaked data
    verbose = FALSE)
  

  output$filter_header <- renderText({
    req(!is.null(all_data()))
    paste0("If desired, pre-filter USUBJID by variables from loaded data sets")
  })
  # output$filter_bds_header <- renderText({
  #   req(!is.null(all_data()) & length(BDS()) > 0)
  #   paste0("For convenience: AVAL, CHG, and BASE values are displayed for each PARAMCD")
  # })
  
  observe({
    # make sure selectData has been run
    req(!is.null(datafile())) #74
    
    


    
    # stringr
    orig_code <- paste(capture.output(attr(all_data(), "code")),collapse = "")
    # orig_code <- 'processed_data %>% filter(ABIFN1 %in% c(NA, "NEGATIVE")) %>% filter(ABIFN1 %in% c(NA, "POSITIVE"))'
    code_text <- gsub('\"',"\'",orig_code)
    len <- nchar(code_text)
    f_loc <- str_locate_all(code_text,"filter")
    filter_loc <- as_tibble(f_loc[[1]])
    var_st <- filter_loc$end + 2
    
    p_loc <- str_locate_all(code_text,"\\%\\>\\%") # have to use this
    pipe_loc <- as_tibble(p_loc[[1]])
    num_pipes <- nrow(pipe_loc)
    var_end <- c(pipe_loc$start[2:num_pipes] - 3, len - 1)
    filter_vectors <- map2(.x = var_st, .y = var_end, function(x,y) substr(code_text,x,y))
    my_msg <- paste(filter_vectors[!is.na(filter_vectors)], collapse = "\n")
    disp_msg <- gsub("\\%in\\%","IN",
                     gsub("c\\(","\\(",
                          gsub("NA","Missing",
                               gsub("na","Missing",
                                  gsub("   "," ",
                                       gsub("  "," ",
                                       gsub("\\|","OR",
                                            gsub("\\&","AND",
                 my_msg
                 ))))))))
    
    
    cat(paste("\n1:",orig_code))
    cat(paste0("\n2:\n",disp_msg,"\n"))
    
    # cat(paste0("\n1: ",capture.output(attr(all_data(), "code"))))
    # cat(paste0("\n2: ",any(str_detect(capture.output(attr(all_data(), "code")), "%>%"))))
    # cat(paste0("\n3: ",any(regexpr("%>%",capture.output(attr(all_data(), "code"))) > 0),"\n"))
    
    # cat(paste0("\n2: ",gsub("%>%", "%>% \n ",
    #          gsub("\\s{2,}", " ",
    #               paste0(
    #                 capture.output(attr(all_data(), "code")),
    #                 collapse = " "))
    # )))
    # see if any dup columns are discovered
    # cat(paste("\n",paste(colnames(all_data())[which(regexpr(".x",colnames(all_data()),ignore.case = T) > 0)],collapse = ", "),"\n"))
    # cat(paste("\n",class(all_data()$TR01EDTM)))
              
    # The rest of the widgets will be shown after the USUBJID has been selected
    subj <- unique(all_data()[, "USUBJID"]) # unique(datafile()$ADSL[, "USUBJID"]) # get list of unique USUBJIDs
    
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
  
  # return(dataselected) # #74
  return(list(my_loaded_adams = my_loaded_adams,all_data = all_data)) #74
  
} # IndvExpl1Initial
