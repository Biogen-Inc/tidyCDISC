PopuExplor <- function(input, output, session, datafile){
  
  ns <- session$ns
  
  waiting_screen <- tagList(
    spin_folding_cube(),
    h4("Hold on a bit while we merge datasets...")
  ) 

  dataselected <- callModule(selectData, id = NULL, datafile)

  values <- reactiveValues(popudata = NULL)

# show/hide checkboxes depending on radiobutton selection
  observeEvent(input$done,{
    
    # make sure selectData has been run
    req(!is.null(dataselected()))    
    
    waiter_show(html = waiting_screen, color = "lightblue")
    Sys.sleep(0.5) # wait 1/2 second
    
    # set these to un-selected
    updateSelectInput(session = session, inputId = "groupbyvar", choices = " ", selected = character(0))
    updateSelectInput(session = session, inputId = "responsevar", choices = " ", selected = character(0))
    updateSelectInput(session = session, inputId = "seltimevar", choices = " ", selected = character(0))
    updateSelectInput(session = session, inputId = "selxvar", choices = " ", selected = character(0))
    updateSelectInput(session = session, inputId = "selyvar", choices = " ", selected = character(0))
    updateSelectInput(session = session, inputId = "selzvar", choices = " ", selected = character(0))
    
    
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
                     "Heat Map      " = "4",
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
                     "Heat Map      " = "4",
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
    mutate(PARAMCD = "ADSL", PARAM = "Subject-Level Data", AVISIT = "Baseline", AVISITN = 0) %>%
    var_labels(AVISIT = "Analysis Visit")
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
      select("USUBJID","AVISIT","AVISITN",ends_with("DY"),"PARAMCD",
             any_of(c("PARAM","AVAL","BASE","CHG","data_from"))) %>%
             # one_of("PARAM","AVAL","BASE","CHG","data_from")) %>%
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
    # ADSL <- datakeep()[names(datakeep()) == "ADSL" ]
    all_data <- bind_rows(ADSL, .id = "data_from")
    all_data$data_from <- "ADSL" # set to ADSL, defaults to "1" here???
    # all_data <- bind_rows(ADSL, .id = "data_from") %>%
    #   mutate(PARAMCD = "ADSL", PARAM = "Subject-Level Data", AVISIT = "Baseline", AVISITN = 0) %>%
    #   var_labels(AVISIT = "Analysis Visit")
    
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
  
  # function for creating factors and preserving variable label
  # makefac <- function(data, varn, varc){
  #   # create ordered factor and preserve the label
  # 
  #   varcs <- substitute(varc)
  #   labl <- unname(get_label(data[[varcs]]))
  # 
  #   # You can use the "curly curly" method now if you have rlang >= 0.4.0
  #   # !! mean_name = mean(!! expr) isn't valid R code, so we need to use the := helper provided by rlang.
  #   data <- data %>%
  #     filter(!is.na(!!enquo(varn)) ) %>%
  #     arrange({{varn}}) %>%
  #     mutate({{varc}} := factor({{varc}}, unique({{varc}}), ordered = TRUE) ) %>%
  #     var_labels({{varc}} := !!sym(labl))
  # 
  # }
  # Now this is more generic, not specific to one study
  if ("STUDYID" %in% colnames(all_data)) {

    if ("TRT01P" %in% colnames(all_data)) {
      # all_data <- makefac(all_data, TRT01PN, TRT01P)
      # set_label(all_data$TRT01P) <- unname(get_label(ADSL, TRT01P))
      # Hmisc::label(all_data$TRT01P) = get_label(ADSL$TRT01P)
    }
    if ("TRT01A" %in% colnames(all_data)) {
      # all_data <- makefac(all_data, TRT01AN, TRT01A)
      # set_label(all_data$TRT01A) <- unname(get_label(ADSL, TRT01A))
      # Hmisc::label(all_data$TRT01A) = get_label(ADSL$TRT01A)
    }
    if ("AGEGR" %in% colnames(all_data)) {
      # all_data <- makefac(all_data, AGEGRN, AGEGR)
      # set_label(all_data$AGEGR) <- unname(get_label(ADSL, AGEGR))
      # Hmisc::label(all_data$TRT01A) = get_label(ADSL$TRT01A)
    }
    if ("AVISIT" %in% colnames(all_data)) {
      # all_data <- makefac(all_data, AVISITN, AVISIT)
      # Hmisc::label(all_data$AVISIT) = get_label(ADSL$AVISIT)
    } 
  }

  values$popudata <- all_data
  # assign("all_data", all_data, envir = .GlobalEnv)
  
  waiter_hide()
  
}, ignoreNULL = FALSE) # observeEvent input$done

  observeEvent(input$radio,{
    
    req(values$popudata)
    all_data <- values$popudata
    
    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    }) 
    
    # Update Parameter Code choices
    updateSelectizeInput(
      session = session,
      inputId = "selPrmCode",
      choices = sort(unique(all_data$PARAMCD)),
      options = list(maxItems = 1),
      selected = "")

    # reset key widgets to original values
    # shinyjs::reset("input$selxvar")
    # shinyjs::reset("input$selyvar")
    # shinyjs::reset("input$groupbyvar")
    # shinyjs::reset("input$responsevar")

    # hide all the widgets
    widgets <- c("selPrmCode","groupbox","groupbyvar","selxvar","selyvar","selzvar","seltimevar",
                 "responsevar","AddPoints","animate","animateby","numBins","AddLine","AddSmooth",
                 "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill")
    
    # hide all the widgets using an anonymous function
    map(widgets, function(x) shinyjs::hide(x))
    
    switch(input$radio, # use swtich() instead of if/else
           "0" = {
             # print("radio button is zero.")
           },
           "1" = {
             # scatter plot module
             dataset <- reactive({ all_data })
             # Update Parameter Code choices
             # allow users to select up to two PARAMCDs here
             updateSelectizeInput(
               session = session,
               inputId = "selPrmCode",
               choices = sort(unique(all_data$PARAMCD)),
               options = list(maxItems = 2),
               selected = "")
             
             callModule(PopuExpl1Scat, id = NULL, dataset)
           },
           "2" = {
             # spaghetti plot module
             # if ADSL is in data_from then no BDS datasets were selected
             if (!"ADSL" %in% unique(all_data$data_from)) {
               # message("Running Spaghetti Plot")
               dataset <- reactive({ all_data }) 
               callModule(PopuExpl2Spag, id = NULL, dataselected, dataset)
             } else {
               message("Spaghetti Plot needs a BDS dataset, not ADSL")
               shinyjs::alert("An ADaM BDS dataset is required for spaghetti plot")
             }
           },
           "3" = {
             # box plot module
             dataset <- reactive({ all_data })
             callModule(PopuExpl3Boxp, id = NULL, dataset)
           }, 
           "4" = {
             # heat map module
             updateAwesomeRadio(session=session, inputId = "fillType", selected = "Fill Variable")
             dataset <- reactive({ all_data })
             callModule(PopuExpl4Heat, id = NULL, dataset)
           },
           "5" = {
             # histogram module
             dataset <- reactive({ all_data })
             callModule(PopuExpl5Hist, id = NULL, dataset)
           },
           # This should not happen
           stop("invalid radio button: ",input$radio)
    )
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # observeEvent input$radio
  
}   # PopuExplor