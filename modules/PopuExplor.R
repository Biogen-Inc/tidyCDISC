PopuExplor <- function(input, output, session, datafile){
  
  ns <- session$ns
  
  waiting_screen <- tagList(
    spin_folding_cube(),
    h4("Hold on a bit while we merge datasets...")
  ) 

  dataselected <- callModule(selectData, id = NULL, datafile)

# show/hide checkboxes depending on radiobutton selection
  observeEvent(input$done,{
    
    # make sure selectData has been run
    req(!is.null(dataselected()))    
    
    waiter_show(html = waiting_screen, color = "lightblue")
    Sys.sleep(0.5) # wait 1/2 second
    
    updateSelectInput(session = session, inputId = "responsevar", choices = " ", selected = " ")
    updateSelectInput(session = session, inputId = "selyvar", choices = " ", selected = " ")
    
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
  # (1) one or more BDS datasets joined ("pancaked") together (with or without ADSL data)
  # (2) ADSL data alone
  # (3) A custom dataset (with a PARAMCD but without a USUBJID)
  # 
  # Also, build fake PARAMCDs for ADAE and ADCM, if you want to look at them here.
  
  # Isolate ADSL 
  if ("ADSL" %in% names(datakeep())) {
  ADSL <- zap_label(datakeep()$ADSL) %>%
    mutate(PARAMCD = "NA", AVISIT = "Baseline", AVISITN = 0)
  }
  # add a PARAMCD and PARAM to ADAE, if it exists and put it back in the list
  if ("ADAE" %in% names(datakeep())) {
  ADAE <- zap_label(datakeep()$ADAE) %>%
    mutate(PARAMCD = "ADAE", PARAM = "Adverse Events", AVISIT = "Baseline", AVISITN = 0)
  
  datazz <- append(datakeep()[!names(datakeep()) %in% "ADAE"],list("ADAE" = ADAE)) 
  datakeep <- reactive({ datazz })
  }
  # add a PARAMCD and PARAM to ADCM, if it exists and put it back in the list
  if ("ADCM" %in% names(datakeep())) {
    ADCM <- zap_label(datakeep()$ADCM) %>%
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

    # Bind all the PARAMCD files
    all_USUBJ <- bind_rows(BDSUSUBJ, .id = "data_from")  %>%
      distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE)

    # # Join ADSL and all_PARAMCD, if it exsits
    if (exists("ADSL")) {
      # take the names that are unique to ADSL
      ADSL.1 <- ADSL[, !names(ADSL) %in% names(all_USUBJ)]
      # add USUBJID
      ADSL.2 <- cbind(select(ADSL,USUBJID),ADSL.1,stringsAsFactors = FALSE)

      all_data <- inner_join(ADSL.2, all_USUBJ, by = "USUBJID")
      rm(ADSL.1,ADSL.2)

    } else {
      
      # all BDS files without ADSL variables
      all_data <- all_USUBJ
      
    }
    
  } else {
    
    # just ADSL by itself
    ISADSL <- datakeep()[names(datakeep()) == "ADSL" ]
    all_data <- bind_rows(ISADSL, .id = "data_from") %>%
      mutate(PARAMCD = "ADSL", AVISIT = "Baseline", AVISITN = 0)
  }
  
  # SAS data uses blanks as character missing; replace blanks with NAs for chr columns
  chr <- sort(names(all_data[ , which(sapply(all_data,is.character ))])) # all chr
  chrdat <- all_data[, chr]
  oth <- sort(names(all_data[ , !names(all_data) %in% chr])) # all oth
  othdat <- all_data[, oth]
  chrdat <- as.data.frame(apply(chrdat, 2, function(x) gsub("^$|^ $", NA, x)),stringsAsFactors = FALSE)
  # chrdat <- drop_na(chrdat)
  all_data <- cbind(othdat,chrdat)
  
  # This is to create ordered factors of TRT01A, TRT01P if they exist, and if STUDYID 105MS301 is used
  if ("STUDYID" %in% colnames(all_data)) {
    if (unique(all_data$STUDYID) == "105MS301") {
      if ("TRT01A" %in% colnames(all_data)) {
        all_data <- all_data %>%
        mutate(TRT01A = factor(TRT01A, ordered = TRUE,
        levels = c("Placebo", "BIIB017 125 mcg every 4 weeks", "BIIB017 125 mcg every 2 weeks")))
      }
      if ("TRT01P" %in% colnames(all_data)) {
        all_data <- all_data %>%
        mutate(TRT01P = factor(TRT01P, ordered = TRUE,
        levels = c("Placebo", "BIIB017 125 mcg every 4 weeks", "BIIB017 125 mcg every 2 weeks")))
      }
    }
  }
  
  assign("all_data", all_data, envir = .GlobalEnv)
  
  waiter_hide()

  # hide all the widgets
  shinyjs::hide(id="selPrmCode")
  shinyjs::hide(id="splitbox")
  shinyjs::hide(id="splitbyvar")
  shinyjs::hide(id="selxvar")
  shinyjs::hide(id="selyvar")
  shinyjs::hide(id="selzvar")
  shinyjs::hide(id="seltimevar")
  shinyjs::hide(id="responsevar")
  shinyjs::hide(id="AddPoints")
  shinyjs::hide(id="animate")
  shinyjs::hide(id="animateby")
  shinyjs::hide(id="numBins")
  shinyjs::hide(id="AddLine")
  shinyjs::hide(id="AddSmooth")
  shinyjs::hide(id="DiscrXaxis")
  shinyjs::hide(id="UseCounts")
  
}, ignoreNULL = FALSE) # observeEvent input$done

  observeEvent(input$radio,{
    
    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    }) 
    
    # Update Paramer Code choices
    updateSelectInput(
      session = session,
      inputId = "selPrmCode",
      choices = c(" ",sort(unique(all_data$PARAMCD))),
      selected = sort(unique(all_data$PARAMCD))[[1]])
    
    switch(input$radio, # use swtich() instead of if/else
           "0" = {
             # hide all the widgets
             shinyjs::hide(id="selPrmCode")
             shinyjs::hide(id="splitbox")
             shinyjs::hide(id="splitbyvar")
             shinyjs::hide(id="selxvar")
             shinyjs::hide(id="selyvar")
             shinyjs::hide(id="selzvar")
             shinyjs::hide(id="seltimevar")
             shinyjs::hide(id="responsevar")
             shinyjs::hide(id="AddPoints")
             shinyjs::hide(id="animate")
             shinyjs::hide(id="animateby")
             shinyjs::hide(id="numBins")
             shinyjs::hide(id="AddLine")
             shinyjs::hide(id="AddSmooth")
             shinyjs::hide(id="DiscrXaxis")
             shinyjs::hide(id="UseCounts")
           },
           "1" = {
             # scatter plot module
             dataset <- reactive({ all_data })
             callModule(PopuExpl1Scat, id = NULL, dataset)
           },
           "2" = {
             # spaghetti plot module
             # if ADSL is in data_from then no BDS datasets were selected
             if (!"ADSL" %in% unique(all_data$data_from)) {
               message("Running Spaghetti Plot")
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