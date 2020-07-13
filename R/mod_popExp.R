#' popExp Server Function
#' 
#' Prepare Individual Explorer Tab with some of the basics
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param datafile A list of dataframes
#' 
#' @import shiny
#' @import dplyr
#' @import IDEAFilter
#' @importFrom haven zap_label zap_formats
#' @importFrom purrr map walk2
#' 
#' @family popExp Functions
#' 
mod_popExp_server <- function(input, output, session, datafile) {
  ns <- session$ns
 
  rv <- reactiveValues(# all_data = NULL,
                       df = NULL#,
                       # processed_data = NULL
                       )
  
  # show/hide checkboxes depending on radiobutton selection
  process <- eventReactive(datafile(), {
    
    # make sure selectData has been run
    req(!is.null(datafile()))
    
    # wait until ADSL has been selected
    req("ADSL" %in% names(datafile()) )
    
    #####################################################################    
    # The data used by the population explorer is going to be one of:
    # (1) one or more BDS datasets row-joined ("pancaked") together 
    #     and ADSL will be column-joined with the BDS data
    # (2) ADSL data alone
    # 
    # Also, build fake PARAMCDs for ADAE and ADCM, if they were selected.
    ######################################################################
    
    rv$df <- datafile()
    
    # Isolate ADSL 
    if ("ADSL" %in% names(rv$df)) {
      
      ADSL <- rv$df$ADSL %>%
        haven::zap_formats() %>%
        mutate(PARAMCD = "ADSL", PARAM = "Subject-Level Data", AVISIT = "Baseline", AVISITN = 0) 
      sjlabelled::set_label(ADSL$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADSL$PARAM)   <- "Parameter"
      sjlabelled::set_label(ADSL$AVISIT)  <- "Analysis Visit"
      sjlabelled::set_label(ADSL$AVISITN) <- "Analysis Visit (N)"
    }
    
    # add a PARAMCD and PARAM to ADAE, if it exists and put it back in the list
    if ("ADAE" %in% names(rv$df)) {
      ADAE <- rv$df$ADAE %>%
        haven::zap_formats() %>%
        filter(!AETERM %in% c(""," ",".")) %>%  # drop records where AETERM is missing
        mutate_if(is.character, list(~na_if(., ""))) %>%
        mutate(PARAMCD = "ADAE", PARAM = "Adverse Events")
      
      sjlabelled::set_label(ADAE$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADAE$PARAM)   <- "Parameter"
      
      rv$df <-  append(isolate(rv$df[!names(rv$df) %in% "ADAE"]),list("ADAE" = ADAE)) 
      
    }
    # add a PARAMCD and PARAM to ADCM, if it exists and put it back in the list
    if ("ADCM" %in% names(rv$df)) {
      ADCM <- datafile()$ADCM %>%
        haven::zap_formats() %>%
        mutate_if(is.character, list(~na_if(., ""))) %>%
        mutate(PARAMCD = "ADCM", PARAM = "Concomitant Meds")
      
      sjlabelled::set_label(ADCM$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADCM$PARAM)   <- "Parameter"
      
      rv$df <-  append(isolate(rv$df[!names(rv$df) %in% "ADCM"]),list("ADCM" = ADCM)) 
      
    }
    
    # split the non-ADSL data into those which have a USUBJID or not
    NOTADSL <- rv$df[names(rv$df) != "ADSL" ]
    if (!is_empty(NOTADSL)) {
      # keep only BDS/OCCDS datasets -- one of the colnames has to be "PARAMCD"
      BDSOCCDS <- NOTADSL[which(sapply(NOTADSL, function(df) "PARAMCD" %in% colnames(df)))]
      
      # zap formats
      for (i in 1:length(BDSOCCDS)) (
        BDSOCCDS[[i]] <- haven::zap_formats(BDSOCCDS[[i]])
      )
      
      # Bind all the BDS (PARAMCD) files and filter them
      all_BDSDATA <- bind_rows(BDSOCCDS, .id = "data_from")  
      
      # remove any "ADSL" variables lurking in all_BDSDATA
      all_BDSDATA <- all_BDSDATA %>% select(-tidyselect::any_of(c("AGEGR","AGEGRN","RACE","RACEN","SEX","SEXN")))
      
      # take by= variable USUBJID plus all the names that are unique to ADSL
      ADSL.1 <- select(ADSL, USUBJID, dplyr::setdiff(names(ADSL), names(all_BDSDATA)))
      my_adsl_cols <- c(colnames(ADSL.1))
      # Warning: Column `USUBJID` has different attributes on LHS and RHS of join
      # suppressWarnings(all_data <- left_join(all_BDSDATA, ADSL.1, by = "USUBJID"))
      suppressWarnings( # Warning: Column `USUBJID` has different attributes on LHS and RHS of join
        all_data <- all_BDSDATA %>%
          left_join(ADSL.1, by = "USUBJID") %>%
          bind_rows(
            ADSL %>%
            mutate(data_from = 'ADSL') %>%
            select(data_from, everything())
          )
      )
      rm(ADSL.1)
      
    } else {
      # just ADSL by itself
      my_adsl_cols <- colnames(ADSL)
      all_data <- bind_rows(ADSL, .id = "data_from")
      all_data$data_from <- "ADSL" # set to ADSL, defaults to "1" here???
    }
    
    # SAS data uses blanks as character missing; replace blanks with NAs for chr columns
    # na_if can also be used with scoped variants of mutate
    # like mutate_if to mutate multiple columns
    all_data <- all_data %>%
      mutate_if(is.character, list(~na_if(., "")))
    
    # copy SAS labels back into data
    for (i in seq_along(datafile())) {
      all_data <- sjlabelled::copy_labels(all_data, as.data.frame(datafile()[[i]]))
    }
    
    # Now this is more generic, not specific to one study
    if ("STUDYID" %in% colnames(all_data)) {
      if ("CHG" %in% colnames(all_data) ) {
        # set CHG to zero instead of NA at Baseline
        all_data <- mutate(all_data, CHG = ifelse(AVISIT == "Baseline", replace_na(CHG, 0), CHG))
      }
      
      refact <- function(data, varc, varn) {
        datac <- deparse(substitute(data))
        if (varc %in% colnames(data) && varn %in% colnames(data)) {
          message(paste("A factor was created for", varc, "based on", varn, "levels"))
          data[, (varc) := forcats::fct_reorder(get(varc), get(varn))]
        } 
      }
      varclst <- c("AGEGR", "AGEGR1", "SEX", "RACE", "RACETXT", "TRTA", "TRT01A", "TRT02A", "TRTP", "TRT01P", "TRT02P", "AVISIT", "APHASE", "AETOXGR", "AESEV", "AEREL")
      varnlst <- c("AGEGRN","AGEGR1N","SEXN","RACEN","RACETXTN","TRTAN","TRT01AN","TRT02AN","TRTPN","TRT01PN","TRT02PN","AVISITN","APHASEN","AETOXGRN","AESEVN","AERELN")
      
      # save the variable labels into savelbls vector
      savelbls <- sjlabelled::get_label(all_data)
      
      data.table::setDT(all_data)
      purrr::walk2(varclst, varnlst, ~ refact(all_data, .x, .y))
      
      # copy SAS labels back into data
      all_data <- sjlabelled::set_label(all_data, label = savelbls)
      
    }
    
    # rv$all_data <- all_data 
    return(list(all_data = all_data, adsl_cols = my_adsl_cols))
  }, ignoreNULL = FALSE) # observeEvent datafile()
  
  #
  # section for filtering
  #
  output$hide_panel <- eventReactive(input$adv_filtering, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  
  # Only select data that starts with AD followed by one or more alphanumerics or underscore
  my_loaded_adams <- reactive({
    req(!is.null(datafile()))
    cat(paste("\nadsl cols:", paste(adsl_cols(), collapse = ", ")))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  # If User wants to perform advance filtering, update drop down of data frames they can filter on
  observe({
    req(input$adv_filtering == T)
    updateSelectInput("filter_df", session = session, choices = as.list(my_loaded_adams()), selected = "ADSL") #
  })
  
  # must make reactive
  all_data <- reactive({
    process()$all_data
  })
  # must make reactive
  adsl_cols <- reactive({
    process()$adsl_cols
  })
  
  # must make reactive
  feed_filter <- reactive({
    # req(!is.null(datafile()))
    if(input$adv_filtering == T){
      all_data() %>% subset(data_from %in% input$filter_df)
    } else {
      all_data()
      # rv$all_data
    }
    # processed_data()
  })
  
  # must make reactive
  not_filtered <- reactive({
    # req(!is.null(datafile()))
    if(input$adv_filtering){
      all_data() %>% subset(!(data_from %in% input$filter_df))
    } else {
      NULL
    }
  })
  
  # IDEAFilter
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
  
  # Update datset, depending on adv_filtering or filtered_data() changing
  dataset <- eventReactive(list(input$adv_filtering,filtered_data()), {
    if (!is.null(filtered_data()) && input$adv_filtering == TRUE ) {
      # extract just the ADSL columns from the filtered data frame so we can
      # apply those changes to the unfiltered data
      adsl_filt_cols <- 
        filtered_data() %>%
        subset(data_from %in% input$filter_df) %>%
        select(adsl_cols()) %>%
        distinct()
      
      d <- filtered_data()
      
      # If there are any datasets that were not filtered, then semi_join those
      if(!is.null(not_filtered())){
        d <- d %>%
          union(
            not_filtered()%>%
              semi_join(adsl_filt_cols)
          )
      }
    } else {
      # d <- filtered_data() # by default, this should be all_data() # if doesn't work, try 
      # d <- all_data()
      d <- all_data()
    }
    return(d)
  }) 
  
  p_scatter <- callModule(scatterPlot_srv, "scatterPlot", data = dataset)
  p_spaghetti <- callModule(spaghettiPlot_srv, "spaghettiPlot", data = dataset)
  p_box <- callModule(boxPlot_srv, "boxPlot", data = dataset)
  

  # use plot output of the module to create the plot 
  output$plot_output <- renderPlotly({
        switch(input$plot_type,
               `Scatter Plot` = p_scatter() %>% plotly::ggplotly() %>% config(displayModeBar = F),
               `Box Plot` = p_box() %>% plotly::ggplotly() %>% config(displayModeBar = F),
               `Spaghetti Plot` = p_spaghetti() %>% plotly::ggplotly() %>% config(displayModeBar = F)
        )
  })
  
}