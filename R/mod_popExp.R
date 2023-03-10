#' popExp Server Function
#' 
#' Prepare Individual Explorer Tab with some of the basics
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param datafile A list of dataframes
#' 
#' @import shiny
#' @import dplyr
#' @importFrom IDEAFilter shiny_data_filter
#' @importFrom haven zap_label zap_formats
#' @importFrom purrr map walk2
#' 
#' @family popExp Functions
#' @noRd
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
  
  output$study_pop_exp <- renderUI({
    req(datafile())
    
    studies <- unique(unlist(lapply(datafile(), `[[`, "STUDYID")))
    study_ids <- paste(studies, collapse = " & ")
    h4(paste("Study ID: ", study_ids))
  })
  
  col_list <- eventReactive(datafile(), {
    map(datafile(), colnames)
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
    if (!rlang::is_empty(NOTADSL)) {
      
      # zap formats
      for (i in 1:length(NOTADSL)) ( NOTADSL[[i]] <- haven::zap_formats(NOTADSL[[i]]) )
      
      # Bind all the BDS (PARAMCD) files and filter them & remove any "ADSL" variables lurking
      all_BDSDATA <- bind_rows(NOTADSL, .id = "data_from")  %>%
        select(-dplyr::any_of(c("AGEGR","AGEGRN","RACE","RACEN","SEX","SEXN")))
      
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
        all_data <- mutate(all_data, CHG = ifelse(AVISIT == "Baseline", tidyr::replace_na(CHG, 0), CHG))
        all_data$CHG <- sjlabelled::set_label(all_data$CHG, label = chg_lab)
      }

      if("AVISIT" %in% colnames(all_data)) all_data <- all_data %>% mutate(AVISIT = stringr::str_wrap(AVISIT, width = 9))
      if("VISIT" %in% colnames(all_data)) all_data <- all_data %>% mutate(VISIT = stringr::str_wrap(VISIT, width = 9))
        
      all_data <- all_data %>% 
        varN_fctr_reorder() 

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
  
  # if ADTT* exists in data & it has the cnsr column, add KM curve as radioButton choice
  observeEvent(my_loaded_adams(), {
    # req(any(substr(my_loaded_adams(), 1, 4) == "ADTT"))
    # adtt_ <- my_loaded_adams()[substr(my_loaded_adams(), 1, 4) == "ADTT"]
    req(any(purrr::map_lgl(my_loaded_adams(), ~ "CNSR" %in% colnames(datafile()[[.x]]))))
    updateRadioButtons(session, "plot_type",
                       choices = c("Kaplan-Meier Curve",
                                   "Line plot - mean over time",
                                   "Heatmap - endpoint correlations",
                                   "Box Plot",
                                   "Scatter Plot", 
                                   "Spaghetti Plot"
                                   ) # new ... added KM
    )
  })
  
  # must make reactive
  all_data <- reactive({ process()$all_data })
  adsl_cols <- reactive({ process()$adsl_cols })
  
  # Data to provide IDEAFilter
  feed_filter <- reactive({
    all_data() %>% subset(data_from %in% input$filter_df)
  })
  
  filter_cols <- reactive({
    col_list()[input$filter_df] %>% unlist()
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
  filters <- callModule(
    IDEAFilter::shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = reactive(feed_filter()[filter_cols()]),    # the name of your pre-processed data
    verbose = FALSE)
  
  filtered_data <- reactive({
    if (input$apply_filters == FALSE) {
      all_data()
    } else if (any(regexpr("%>%",capture.output(attr(filters(), "code"))) > 0)) {
    attr(filters(), "code") %>% 
        capture.output() %>% 
        paste(collapse = "") %>% 
        str_replace("^.*?(%>%)", "feed_filter\\(\\) \\1") %>% 
        rlang::parse_expr() %>% 
        rlang::eval_tidy()
    } else {
      feed_filter()
    }
  }) %>%
    bindEvent(filters(), input$apply_filters)
  
  
  # Update datset, depending on apply_filters or filtered_data() changing
  dataset <- eventReactive(list(input$apply_filters,filtered_data()), {
    if (input$apply_filters == TRUE && !is.null(filtered_data())) {
      
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
      d <- sjlabelled::copy_labels(d, feed_filter()) # add labels back in after filter
      
    } else {
      d <- all_data()
    }
    return(d)
  })

  km_data <- reactive({
    req(any(purrr::map_lgl(my_loaded_adams(), ~ "CNSR" %in% colnames(datafile()[[.x]]))))
    dataset() %>% 
      # filter(substr(data_from, 1, 4) == "ADTT") %>%
      filter(!is.na(CNSR))
  })
  
  run_scat <- reactive(ifelse(input$plot_type == "Scatter Plot", TRUE, FALSE))
  run_boxp <- reactive(ifelse(input$plot_type == "Box Plot", TRUE, FALSE))
  run_spag <- reactive(ifelse(input$plot_type == "Spaghetti Plot", TRUE, FALSE))
  run_line <- reactive(ifelse(input$plot_type == "Line plot - mean over time", TRUE, FALSE))
  run_heat <- reactive(ifelse(input$plot_type == "Heatmap - endpoint correlations", TRUE, FALSE))
  run_kapm <- reactive(ifelse(input$plot_type == "Kaplan-Meier Curve", TRUE, FALSE))

  
  p_scatter <- callModule(scatterPlot_srv, "scatterPlot", data = dataset, run = run_scat)
  p_spaghetti <- callModule(spaghettiPlot_srv, "spaghettiPlot", data = dataset, run = run_spag)
  p_box <- callModule(boxPlot_srv, "boxPlot", data = dataset, run = run_boxp)
  p_line <- callModule(linePlot_srv, "linePlot", data = dataset, run = run_line)
  p_heatmap <- callModule(heatmap_srv, "heatmap", data = dataset, run = run_heat)
  p_km <- callModule(km_srv, "km", data = km_data, run = run_kapm)
  

  # use plot output of the module to create the plot 
  output$plot_output <- renderPlotly({
        switch(input$plot_type,
         `Scatter Plot` = p_scatter() %>% plotly::ggplotly(),
         `Box Plot` = p_box() %>% plotly::ggplotly(),
         `Spaghetti Plot` = p_spaghetti() %>% plotly::ggplotly(),
         `Line plot - mean over time` = p_line$plot() %>% plotly::ggplotly(tooltip = c("text")),
         `Heatmap - endpoint correlations` = p_heatmap$plot() %>% plotly::ggplotly(tooltip = c("text"))
         , `Kaplan-Meier Curve` = p_km() %>% plotly::ggplotly()
        ) %>%
          config(displaylogo = FALSE, 
                modeBarButtonsToRemove = 
                  c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
                    "hoverClosestCartesian", "hoverCompareCartesian", "zoom3d", "pan3d",
                     "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
                    "orbitRotation", "tableRotation", "zoomInGeo", "zoomOutGeo",
                     "resetGeo", "hoverClosestGeo", "sendDataToCloud","hoverClosestGl2d",
                    "hoverClosestPie", "toggleHover","resetViews","toggleSpikelines","resetViewMapbox"
                  # , 'toImage', 'resetScale2d', 'zoomIn2d', 'zoomOut2d','zoom2d', 'pan2d'
                )
          )
  })
  
  # Output text string of what was filtered in IDEAFilter widget/ module
  output$applied_filters <- renderUI({
    req(
      any(regexpr("%>%",capture.output(attr(filters(), "code"))) > 0)
      & input$apply_filters == TRUE
    )
    filters_in_english(filters())
  })

  p_data <- 
    reactive({
      req(input$plot_type)
      switch(input$plot_type,
             `Scatter Plot` = NULL, #p_scatter$data(),
             `Box Plot` = NULL, #p_box$data(),
             `Spaghetti Plot` = NULL, #p_spaghetti$data(),
             `Line plot - mean over time` = p_line$plot_data(),
             `Heatmap - endpoint correlations` = p_heatmap$plot_data(),
             `Kaplan-Meier Curve` =  NULL, #p_km$data()
      )
    })

  # p_filename_base <-
  #   reactive({
  #     req(input$plot_type)
  #     switch(input$plot_type,
  #            `Scatter Plot` = NULL, #p_scatter$data(),
  #            `Box Plot` = NULL, #p_box$data(),
  #            `Spaghetti Plot` = NULL, #p_spaghetti$data(),
  #            `Line plot - mean over time` = "Line Plot of Mean over time", #p_line$plot_nm(),
  #            `Heatmap - endpoint correlations` = "Line Plot of Mean over time", #p_heatmap$plot_nm(),
  #            `Kaplan-Meier Curve` =  NULL, #p_km$data()
  #     )
  #   })
  
  output$plot_data <- DT::renderDataTable({
    if(!is.null(p_data())){
      DT::datatable(p_data(), 
                extensions = "Buttons"
                , options = list(  
                  dom = 'Blftpr'
                  , pageLength = 20
                  , lengthMenu = list(c(20, 50, 100, -1),c('20', '50', '100', "All"))
                  , buttons = list(list(
                    extend = "excel", 
                    filename = paste("tidyCDISC data for", input$plot_type #p_filename_base()
                    # ,str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"), sep = "_")
                    )
                  ))
                )
                , style="default")
    } else {
      p_data()
    }
    
  })
}

