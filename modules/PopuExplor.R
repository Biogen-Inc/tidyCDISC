PopuExplor <- function(input, output, session, datafile){
  
  ns <- session$ns
  
  waiting_screen <- tagList(
    spin_folding_cube(),
    h4("Hold on a bit while we merge datasets...")
  ) 

  dataselected <- callModule(selectData, id = NULL, datafile)

  rv <- reactiveValues(all_data = NULL)

# show/hide checkboxes depending on radiobutton selection
  observeEvent(input$done,{
    
    # make sure selectData has been run
    req(!is.null(dataselected()))    
    
    waiter_show(html = waiting_screen, color = "lightblue")
    Sys.sleep(0.5) # wait 1/2 second
    
    # print("setting adv_filtering checkbox to FALSE")
    updateCheckboxInput(session = session, "adv_filtering", value = F)
    updateTabsetPanel(session = session, "tabset", selected = "Plot")
    
    inputids <- c("groupbyvar","responsevar","seltimevar","selxvar","selyvar","selzvar")
    # set these to un-selected using an anonymous function
    map(inputids, function(x) updateSelectInput(session = session, x, choices = " ", selected = character(0)) )

    # I think what we are up against here is the UI design principle that 
    # there shouldn't be such a thing as radio buttons with nothing selected. 
    # To represent that state you need to add an option called 'None selected' as one of the buttons. 
    #
    updatePrettyRadioButtons(
      session = session,
      inputId = "radio",
      choices = list("Scatter Plot  " = "1",
                     "Spaghetti Plot" = "2",
                     "Box Plot      " = "3",
                     # "Heat Map      " = "4",
                     "Histogram     " = "5",
                     "None selected " = "0"
      ),
      selected = "0"
    )

    updatePrettyRadioButtons(
      session = session,
      inputId = "radio",
      choices = list("Scatter Plot  " = "1",
                     "Spaghetti Plot" = "2",
                     "Box Plot      " = "3",
                     # "Heat Map      " = "4",
                     "Histogram     " = "5"
      ),
      selected = character(0)
    )

  datakeep <- reactive({ datafile()[dataselected()] })
  
  # The data used by the population explorer is going to be one of:
  # (1) one or more BDS datasets row-joined ("pancaked") together 
  #     if ADSL was also selected, it will be column-joined with the BDS data
  # (2) ADSL data alone
  # (3) A custom dataset (with a PARAMCD but without a USUBJID)
  # 
  # Also, build fake PARAMCDs for ADAE and ADCM, if they were selected.
  
  all_data <- NULL
  
  # Isolate ADSL 
  if ("ADSL" %in% names(datakeep())) {

  ADSL <- datakeep()$ADSL %>%
    mutate(PARAMCD = "ADSL", PARAM = "Subject-Level Data", AVISIT = "Baseline", AVISITN = 0) 
    set_label(ADSL$AVISIT) <- "Analysis Visit"
  }
  
  # add a PARAMCD and PARAM to ADAE, if it exists and put it back in the list
  if ("ADAE" %in% names(datakeep())) {
  ADAE <- datakeep()$ADAE %>%
    filter(!AETERM %in% c(""," ",".")) %>%  # drop records where AETERM is missing
    mutate_if(is.character, list(~na_if(., ""))) %>%
    mutate(PARAMCD = "ADAE", PARAM = "Adverse Events", AVISIT = "Baseline", AVISITN = 0)
  
  datazz <- append(datakeep()[!names(datakeep()) %in% "ADAE"],list("ADAE" = ADAE)) 
  datakeep <- reactive({ datazz })
  }
  # add a PARAMCD and PARAM to ADCM, if it exists and put it back in the list
  if ("ADCM" %in% names(datakeep())) {
    ADCM <- datakeep()$ADCM %>%
      mutate(PARAMCD = "ADCM", PARAM = "Concomitant Meds", AVISIT = "Baseline", AVISITN = 0)
    
    datazz <- append(datakeep()[!names(datakeep()) %in% "ADCM"],list("ADCM" = ADCM)) 
    datakeep <- reactive({ datazz })
  }
  
  # split the non-ADSL data into those which have a USUBJID or not
  NOTADSL <- datakeep()[names(datakeep()) != "ADSL" ]
  if (!is_empty(NOTADSL)) {
    # keep only BDS datasets -- one of the colnames has to be "PARAMCD"
    BDS <- NOTADSL[which(sapply(NOTADSL, function(df) "PARAMCD" %in% colnames(df)))]
    # which of the above contain USUBJID ?
    BDSUSUBJ <- BDS[which(sapply(BDS, function(df) "USUBJID" %in% colnames(df)))]
    BDSOTHER <- BDS[!names(BDS) %in% names(BDSUSUBJ)]
  } else {
    BDSUSUBJ <- list()
    BDSOTHER <- list()
  }

  # we're going to assume you just want to look at the custom dataframe
  if (!is_empty(BDSOTHER)) {
    all_data <- bind_rows(BDSOTHER, .id = "data_from")
    
  } else if (!is_empty(BDSUSUBJ)) {
    # Bind all the BDS (PARAMCD) files and filter them
    all_USUBJ <- bind_rows(BDSUSUBJ, .id = "data_from")  %>%
      filter(SAFFL == "Y") %>% # safety population
      filter(!is.na(AVISITN)) %>%
      select("USUBJID","STUDYID","AVISIT","AVISITN",ends_with("DY"),"PARAMCD",
             any_of(c("PARAM","AVAL","BASE","CHG","data_from"))) %>%
      distinct(USUBJID, PARAMCD, AVISIT, .keep_all = TRUE) %>%
      mutate(AVISITN = as.integer(ceiling(AVISITN))) %>%
      arrange(USUBJID, PARAMCD, AVISITN) 

    # Join ADSL and all_PARAMCD, if it exsits
    if (exists("ADSL")) {
      # take the names that are unique to ADSL
      ADSL.1 <- ADSL[, !names(ADSL) %in% names(all_USUBJ)]
      # add USUBJID
      # ADSL.2 <- cbind(select(ADSL,USUBJID),ADSL.1,stringsAsFactors = FALSE)
      ADSL.2 <- bind_cols(select(ADSL,USUBJID),ADSL.1)
      # remove Warning: Column `USUBJID` has different attributes on LHS and RHS of join
      ADSL.2 <- zap_label(ADSL.2)  
      all_data <- left_join(all_USUBJ, ADSL.2, by = "USUBJID")
      rm(ADSL.1,ADSL.2)

    } else {
      # all BDS files without ADSL variables
      all_data <- all_USUBJ
      
    }
    
  } else {
    # just ADSL by itself
    all_data <- bind_rows(ADSL, .id = "data_from")
    all_data$data_from <- "ADSL" # set to ADSL, defaults to "1" here???
    set_label(all_data$AVISIT) <- "Analysis Visit"

  }
  
  # SAS data uses blanks as character missing; replace blanks with NAs for chr columns
  # na_if can also be used with scoped variants of mutate
  # like mutate_if to mutate multiple columns
  all_data <- all_data %>%
    mutate_if(is.character, list(~na_if(., "")))

  # copy SAS labels back into data
  for (i in seq_along(datakeep())) {
    # print(names(datakeep()[i]))
    all_data <- copy_labels(all_data, as.data.frame(datakeep()[[i]]))
  }
  
  # Now this is more generic, not specific to one study
  if ("STUDYID" %in% colnames(all_data)) {
    
    if ("TRT01A" %in% colnames(all_data)) {
      # dummy section
    }
    if ("TRT01P" %in% colnames(all_data)) {
      # dummy section
    }
    if ("AGEGR" %in% colnames(all_data) && "AGEGRN" %in% colnames(all_data)) {
      tmplabl <- get_label(all_data$AGEGR)
      all_data <- all_data %>%
        mutate(AGEGR = fct_reorder(AGEGR, AGEGRN))
      set_label(all_data$AGEGR) <- tmplabl
      # Hmisc::label(all_data$AVISIT) = tmplabl
    } 
    if ("AVISIT" %in% colnames(all_data) && "AVISITN" %in% colnames(all_data)) {
      tmplabl <- get_label(all_data$AVISIT)
      all_data <- all_data %>%
        mutate(AVISITN = as.integer(ceil(AVISITN))) %>%
        mutate(AVISIT = fct_reorder(AVISIT, AVISITN))
      set_label(all_data$AVISIT) <- tmplabl
      # Hmisc::label(all_data$AVISIT) = tmplabl
    } 
  }

  rv$all_data <- reactive({ all_data })

  waiter_hide()
  
}, ignoreNULL = FALSE) # observeEvent input$done

  # section for filtering
  #

  observeEvent(input$adv_filtering, {
    if (input$adv_filtering == F) {
      updateTabsetPanel(session = session, "tabset", selected = "Plot")
    } else {
    # req(input$adv_filtering == T)

    updateTabsetPanel(session = session, "tabset", selected = "Filter")
    # print(paste("rv$all_data is",head(unique(rv$all_data()$data_from))))
    
    updateSelectInput("filter_df", session = session, choices = as.list(unique(rv$all_data()$data_from)), selected = "ADSL") #
    }
  })
  
  # feed_filter <- reactive({rv$processed_data})
  feed_filter <- reactive({ rv$all_data() })
  
  # IDEAFilter
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
 observeEvent(input$clearplot,{
   req(input$clearplot == TRUE)
   # # Clear plotoutput
   # output$PlotlyOut <- renderPlotly({
   #   NULL
   # })
   # # Clear datatable
   # output$DataTable <- DT::renderDataTable({
   #   NULL
   # }) 
   updatePrettyRadioButtons(
     session = session,
     inputId = "radio",
     choices = list("Scatter Plot  " = "1",
                    "Spaghetti Plot" = "2",
                    "Box Plot      " = "3",
                    # "Heat Map      " = "4",
                    "Histogram     " = "5",
                    "None selected " = "0"
     ),
     selected = "0"
   )

   updatePrettyRadioButtons(
     session = session,
     inputId = "radio",
     choices = list("Scatter Plot  " = "1",
                    "Spaghetti Plot" = "2",
                    "Box Plot      " = "3",
                    # "Heat Map      " = "4",
                    "Histogram     " = "5"
     ),
     selected = character(0)
   )
 }, ignoreInit = TRUE)  
 
 observeEvent(input$radio,{
    
    req(!is.null(rv$all_data))

    updateTabsetPanel(session = session, "tabset", selected = "Plot")
    
    if (!is.null(filtered_data()) && input$adv_filtering == T ) {
      dataset <- reactive({ filtered_data() })
    } else {
      dataset <- reactive({ rv$all_data() })
    }

    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    }) 
    
    updateCheckboxInput(session = session, "clearplot", value = F)
    
    # Update Parameter Code choices
    updateSelectizeInput(
      session = session,
      inputId = "selPrmCode",
      choices = sort(unique(dataset()$PARAMCD)),
      options = list(maxItems = 1),
      selected = "")

    # widgets to show
    widgets <- c("adv_filtering","clearplot","Parmstag","radio")
    map(widgets, function(x) shinyjs::show(x))
    
    # widgets to hide
    widgets <- c("selPrmCode","groupbox","groupbyvar","selxvar","selyvar","selzvar","seltimevar",
                 "responsevar","AddPoints","animate","animateby","numBins","AddLine","AddSmooth",
                 "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill")
    map(widgets, function(x) shinyjs::hide(x))

    
    switch(input$radio, # use swtich() instead of if/else
           "0" = {
             # print("radio button is zero.")
           },
           "1" = {
             # scatter plot module
             # Update Parameter Code choices
             # allow users to select up to two PARAMCDs here
             updateSelectizeInput(
               session = session,
               inputId = "selPrmCode",
               choices = sort(unique(dataset()$PARAMCD)),
               options = list(maxItems = 2),
               selected = "")
             
             callModule(PopuExpl1Scat, id = NULL, dataset)
           },
           "2" = {
             # spaghetti plot module
             # if ADSL is in data_from then no BDS datasets were selected
             if (!"ADSL" %in% unique(dataset()$data_from)) {
               callModule(PopuExpl2Spag, id = NULL, dataselected, dataset)
             } else {
               message("Spaghetti Plot needs a BDS dataset, not ADSL")
               shinyjs::alert("An ADaM BDS dataset is required for spaghetti plot")
             }
           },
           "3" = {
             # box plot module
             callModule(PopuExpl3Boxp, id = NULL, dataset)
           }, 
           "4" = {
             # heat map module
             # commented out for now
             message("heat map is commented out for now")
             # updateAwesomeRadio(session=session, inputId = "fillType", selected = "Fill Variable")
             # callModule(PopuExpl4Heat, id = NULL, dataset)
           },
           "5" = {
             # histogram module
             callModule(PopuExpl5Hist, id = NULL, dataset)
           },
           # This should not happen
           stop("invalid radio button: ",input$radio)
    )
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # observeEvent input$radio
  
}   # PopuExplor