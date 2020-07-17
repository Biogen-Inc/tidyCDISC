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
  
  # When the user asks for help, guide them through the UI
  observeEvent( input$help_sel, {
    if(input$adv_filtering == TRUE){
      guide_popex_sel_adv$init()$start() # guide includes IDEAFilter
    } else {
      guide_popex$init()$start()
    }
  })
  
  # show/hide checkboxes depending on radiobutton selection
  process <- eventReactive(datafile(), {
    
    # make sure selectData has been run
    req(!is.null(datafile()))
    
    # wait until ADSL has been selected
    req("ADSL" %in% names(datafile()) )
    
    #####################################################################    
    # The data used by the population explorer is going to be one of:
    # (1) one or more BDS datasets row-joined ("pancaked") together 
    #     and ADSL will be column-joined with the BDS data or...
    # (2) ADSL data alone
    ######################################################################

    # Isolate ADSL 
    if ("ADSL" %in% names(datafile())) {
      ADSL <- datafile()$ADSL %>%
        haven::zap_formats()
    }
    
    # split the non-ADSL data into those which have a USUBJID or not
    NOTADSL <- datafile()[names(datafile()) != "ADSL" ]
    if (!is_empty(NOTADSL)) {
      
      # zap formats
      for (i in 1:length(NOTADSL)) ( NOTADSL[[i]] <- haven::zap_formats(NOTADSL[[i]]) )
      
      # Bind all the BDS (PARAMCD) files and filter them & remove any "ADSL" variables lurking
      all_BDSDATA <- bind_rows(NOTADSL, .id = "data_from")  %>%
        select(-tidyselect::any_of(c("AGEGR","AGEGRN","RACE","RACEN","SEX","SEXN")))
      
      # Manipulate ADSL to contain USUBJID plus all the names that are unique to ADSL
      ADSL.1 <- select(ADSL, USUBJID, dplyr::setdiff(names(ADSL), names(all_BDSDATA)))
      my_adsl_cols <- colnames(ADSL.1)
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
      
    } else { # just ADSL loaded by itself
      my_adsl_cols <- colnames(ADSL)
      all_data <- bind_rows(ADSL, .id = "data_from")
      all_data$data_from <- "ADSL" # set to ADSL, defaults to "1" here???
    }
    
    # SAS data uses blanks as character missing; replace blanks with NAs for chr columns
    # na_if can also be used with scoped variants of mutate
    # like mutate_if to mutate multiple columns
    all_data <- all_data %>% mutate_if(is.character, list(~na_if(., "")))
    if ("ADAE" %in% names(datafile())) {
      all_data <- all_data %>% filter(!AETERM %in% c(""," ","."))
    }
    # copy SAS labels back into data
    for (i in seq_along(datafile())) {
      all_data <- sjlabelled::copy_labels(all_data, as.data.frame(datafile()[[i]]))
    }
    
    # Now this is more generic, not specific to one study
    if ("STUDYID" %in% colnames(all_data)) {
      if ("CHG" %in% colnames(all_data) ) {
        # set CHG to zero instead of NA at Baseline
        chg_lab <- sjlabelled::get_label(all_data$CHG)
        all_data <- mutate(all_data, CHG = ifelse(AVISIT == "Baseline", replace_na(CHG, 0), CHG))
        all_data$CHG <- sjlabelled::set_label(all_data$CHG, label = chg_lab)
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
    return(list(all_data = all_data, adsl_cols = my_adsl_cols))
    
  }, ignoreNULL = FALSE) # end of observeEvent on datafile()
  
  
  ############################
  #
  # Filtering Pre-processing
  #
  ############################
  
  output$hide_panel <- eventReactive(input$apply_filters, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  
  # Only select data that starts with AD followed by one or more alphanumerics or underscore
  my_loaded_adams <- reactive({
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  # If User wants to perform advance filtering, update drop down of data frames they can filter on
  observe({
    updateSelectInput("filter_df", session = session, choices = as.list(my_loaded_adams()))
  })
  
  # must make reactive
  all_data <- reactive({ process()$all_data })
  adsl_cols <- reactive({ process()$adsl_cols })
  
  # Data to provide IDEAFilter
  feed_filter <- reactive({
    if(input$apply_filters == T){
      req(input$filter_df)
      all_data() %>% subset(data_from %in% input$filter_df)
    } else {
      all_data()
    }
  })
  
  # Data NOT provided to IDEAFilter... will need to subset later
  not_filtered <- reactive({
    if(input$apply_filters){
      all_data() %>% subset(!(data_from %in% input$filter_df))
    } else {
      NULL
    }
  })
  
  # Call IDEAFilter Module
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
  
  # Update datset, depending on apply_filters or filtered_data() changing
  dataset <- eventReactive(list(input$apply_filters,filtered_data()), {
    if (!is.null(filtered_data()) && input$apply_filters == TRUE ) {
      
      req(input$filter_df) # needed 100% as this can be slow to update, causing an error
      
      # extract just the ADSL columns from the filtered data frame so we can
      # apply those changes to the unfiltered data
      adsl_filt_cols <- 
        filtered_data() %>%
        subset(data_from %in% input$filter_df) %>%
        select(data_from, all_of(adsl_cols())) %>%
        distinct()
      
      # grab distinct adsl_filt_col values among any dataset (data_from) in case more
      # than 1 was selected
      adsl_filt <-
        split(adsl_filt_cols %>% select(-data_from), adsl_filt_cols$data_from) %>%
        purrr::reduce(inner_join)
      
      d <- filtered_data() %>%
        semi_join(adsl_filt)

      # If there are any datasets that were not filtered, then semi_join those
      if(!is.null(not_filtered())){
        d <- d %>%
          union(
            not_filtered()%>%
              semi_join(adsl_filt)
          )
      }
    } else {
      d <- all_data()
    }
    return(d)
  })
  
  # Preview Dataset sent to plots
  # output$dataset <- DT::renderDataTable({
  #   DT::datatable(dataset())
  # })

  p_scatter <- callModule(scatterPlot_srv, "scatterPlot", data = dataset)
  p_spaghetti <- callModule(spaghettiPlot_srv, "spaghettiPlot", data = dataset)
  p_box <- callModule(boxPlot_srv, "boxPlot", data = dataset)
  

  # use plot output of the module to create the plot 
  output$plot_output <- renderPlotly({
    # print(p_scatter())
        switch(input$plot_type,
               `Scatter Plot` = p_scatter(),
               `Box Plot` = p_box(),
               `Spaghetti Plot` = p_spaghetti()
        )%>% 
        plotly::ggplotly() %>%
          config(displaylogo = FALSE, 
                modeBarButtonsToRemove = 
                  c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
                    "hoverClosestCartesian", "hoverCompareCartesian",
                    "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
                    "orbitRotation", "tableRotation",
                    "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                    "sendDataToCloud",
                    "hoverClosestGl2d",
                    "hoverClosestPie",
                    "toggleHover",
                    "resetViews",
                    "toggleSpikelines",
                    "resetViewMapbox"
                  # , 'toImage', 'resetScale2d', 'zoomIn2d', 'zoomOut2d','zoom2d', 'pan2d'
                )
          )
  })
  
  # Output text string of what was filtered in IDEAFilter widget/ module
  output$applied_filters <- renderUI({
    req(
      any(regexpr("%>%",capture.output(attr(filtered_data(), "code"))) > 0)
      & input$apply_filters == T
    )
    filters_in_english(filtered_data())
  })
}