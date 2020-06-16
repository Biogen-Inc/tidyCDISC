#' popExp Server Function
#' 
#' Prepare Individual Explorer Tab with some of the basics
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param datafile A list of dataframes

#'   DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import IDEAFilter
#' @import waiter
#' @importFrom rlang sym
#' @importFrom haven zap_label
#' @importFrom purrr map 
#' @importFrom shinyjs show hide
#' 
#' @return character vector of loaded adams and a filtered dataframe to populate mod_indvExpPat module
#' 
#' @noRd
mod_popExp_server <- function(input, output, session, datafile){
  ns <- session$ns
 
  # function for updating the main plot radio buttons
  RadioUpdate <- function() {
    
    # I think what we are up against here is the UI design principle that 
    # there shouldn't be such a thing as radio button with nothing selected. 
    # To represent that state you need to add an option called 'None selected' as one of the buttons. 
    # So here we do it twice, the second time without the 'None selected' choice
    
    updatePrettyRadioButtons(
      session = session,
      inputId = "radio",
      choices = list("Scatter Plot  " = "1",
                     "Spaghetti Plot" = "2",
                     "Box Plot      " = "3",
                     # "Heat Map      " = "4",
                     "Histogram     " = "5",
                     "Means Plot    " = "6",  
                     "Hbar Plot     " = "7",
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
                     "Histogram     " = "5",
                     "Means Plot    " = "6",
                     "Hbar Plot     " = "7"
                     
      ),
      selected = character(0)
    )
  }
  
  waiting_screen <- tagList(
    spin_folding_cube(),
    h4("Hold on a bit while we merge datasets...")
  ) 
  
  # hide all the widgets
  widgets <- c("adv_filtering","clearplot","Parmstag","radio","selPrmCode","groupbox","groupbyvar","selxvar","selyvar","selzvar","seltimevar",
               "responsevar","AddPoints","animate","animateby","numBins","AddLine","AddSmooth",
               "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill","errorBars","hbarOptions")
  
  # hide all the widgets using an anonymous function
  map(widgets, function(x) shinyjs::hide(x))
  
  # select the data sets
  # dataselected <- callModule(mod_selectData_server, id = NULL, datafile) # ac golem: why would id = NULL for dataSelected?
  dataselected <- callModule(mod_selectData_server, "selectData_ui_1", datafile)
  
  rv <- reactiveValues(all_data = NULL)
  
  # show/hide checkboxes depending on radiobutton selection
  observeEvent(input$done,{
    
    # make sure selectData has been run
    req(!is.null(dataselected()))    
    
    waiter_show(html = waiting_screen, color = "lightblue")
    Sys.sleep(0.5) # wait 1/2 second
    
    # set adv_filtering checkbox to FALSE; select Plot tab panel
    updateCheckboxInput(session = session, "adv_filtering", value = F)
    updateTabsetPanel(session = session, "tabset", selected = "Plot")
    
    inputids <- c("groupbyvar","responsevar","seltimevar","selxvar","selyvar","selzvar")
    # set these to un-selected using map() and an anonymous function
    map(inputids, function(x) updateSelectInput(session = session, x, choices = " ", selected = character(0)) )
    
    # run the radioUpdate function above
    RadioUpdate()
    
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
      sjlabelled::set_label(ADSL$AVISIT) <- "Analysis Visit"
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
        select("STUDYID","USUBJID","PARAMCD",
               everything() ) %>%
        # ends_with("DY"),
        # any_of(c("PARAM","AVAL","BASE","CHG","data_from","AVISIT","AVISITN","VISIT","VISITNUM",
        #          "ITTFL", "BRTHDT", "SMN2COPY", "AGEFDOSE", "AGELEFD",
        #          "CNSR","LBCAT","LBSTRESN","LBSTNRLO","LBSTNRHI"))) %>%
        arrange(USUBJID, PARAMCD) 
      
      # Join ADSL and all_PARAMCD, if it exsits
      if (exists("ADSL")) {
        # take the names that are unique to ADSL
        ADSL.1 <- ADSL[, !names(ADSL) %in% names(all_USUBJ)]
        # add USUBJID
        # ADSL.2 <- cbind(select(ADSL,USUBJID),ADSL.1,stringsAsFactors = FALSE)
        ADSL.2 <- bind_cols(select(ADSL,USUBJID),ADSL.1)
        # remove Warning: Column `USUBJID` has different attributes on LHS and RHS of join
        ADSL.2 <- haven::zap_label(ADSL.2)  
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
      sjlabelled::set_label(all_data$AVISIT) <- "Analysis Visit"
    }
    
    # SAS data uses blanks as character missing; replace blanks with NAs for chr columns
    # na_if can also be used with scoped variants of mutate
    # like mutate_if to mutate multiple columns
    all_data <- all_data %>%
      mutate_if(is.character, list(~na_if(., "")))
    
    # copy SAS labels back into data
    for (i in seq_along(datakeep())) {
      # print(names(datakeep()[i]))
      all_data <- sjlabelled::copy_labels(all_data, as.data.frame(datakeep()[[i]]))
    }
    
    # Now this is more generic, not specific to one study
    if ("STUDYID" %in% colnames(all_data)) {
      if ("CHG" %in% colnames(all_data) ) {
        # set CHG to zero instead of NA at Baseline
        all_data <- mutate(all_data, CHG = replace_na(CHG, 0))
      }
      
      if ("TRT01A" %in% colnames(all_data)) {
        # dummy section
      }
      if ("TRT01P" %in% colnames(all_data)) {
        # dummy section
      }
      if ("AGEGR" %in% colnames(all_data) && "AGEGRN" %in% colnames(all_data)) {
        tmplabl <- sjlabelled::get_label(all_data$AGEGR)
        all_data <- all_data %>%
          mutate(AGEGR = fct_reorder(AGEGR, AGEGRN)) %>%
          sjlabelled::var_labels(AGEGR = "Age Group")
        # var_labels(AGEGR = !!sym(tmplabl))
      } 
      if ("AVISIT" %in% colnames(all_data) && "AVISITN" %in% colnames(all_data)) {
        tmplabl <- sjlabelled::get_label(all_data$AVISIT)
        all_data <- all_data %>%
          mutate(AVISIT = fct_reorder(AVISIT, AVISITN)) %>%
          sjlabelled::var_labels(AVISIT = "Analysis Visit")
        # var_labels(AVISIT = !!sym(tmplabl))
      } 
    }
    
    rv$all_data <- reactive({ all_data })
    
    waiter_hide()
    
  }, ignoreNULL = FALSE) # observeEvent input$done
  
  #
  # section for filtering
  #
  
  # set adv_filtering checkbox to TRUE if user selects the Filter tab panel
  observeEvent(input$tabset, {
    # print(paste("You are Viewing tab panel",input$tabset))
    if (input$tabset == "Filter") {
      # maybe I'm being too clever here  
      # updateCheckboxInput(session = session, "adv_filtering", value = T)
    }
  }, ignoreInit = TRUE)
  
  # If adv_filtering is checked, switch to Filter tab panel; otherwise, Plot tab panel
  observeEvent(input$adv_filtering, {
    if (input$adv_filtering == T) {
      updateTabsetPanel(session = session, "tabset", selected = "Filter")
      # updateSelectInput("filter_df", session = session, choices = as.list(unique(rv$all_data()$data_from)), selected = "ADSL") 
    } else {
      
      updateTabsetPanel(session = session, "tabset", selected = "Plot")
    }
  })
  
  output$hide_panel <- eventReactive(input$adv_filtering, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  
  feed_filter <- reactive({ rv$all_data() })
  
  # IDEAFilter
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
  # clear the plot area, table, and radio buttons if clear plot has been checked    
  observeEvent(input$clearplot,{
    req(input$clearplot == TRUE)
    
    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    })
    
    RadioUpdate()
    
  }, ignoreInit = TRUE)  
  
  #
  # input$radio button processing
  #
  observeEvent(input$radio,{
    
    req(!is.null(rv$all_data))
    
    dataset <- eventReactive(input$adv_filtering, {
      if (!is.null(filtered_data()) && input$adv_filtering == T ) {
        filtered_data() 
      } else {
        rv$all_data() 
      }
    })
    # if (!is.null(filtered_data()) && input$adv_filtering == T ) {
    #   dataset <- reactive({ filtered_data() })
    # } else {
    #   dataset <- reactive({ rv$all_data() })
    # }
    
    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    }) 
    
    # set clear plot to FALSE, since we just cleared the plot and table
    updateCheckboxInput(session = session, "clearplot", value = F)
    # select Plot tab panel
    updateTabsetPanel(session = session, "tabset", selected = "Plot")
    
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
                 "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill","errorBars","hbarOptions")
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
             
             callModule(mod_popExpScat_server, id = NULL, dataset) # ac golem: left id = NULL
           },
           "2" = {
             # spaghetti plot module
             # if ADSL is in data_from then no BDS datasets were selected
             if (!"ADSL" %in% unique(dataset()$data_from)) {
               callModule(mod_popExpSpag_server, id = NULL, dataset) 
               # ac golem: left id = NULL. Why does this module have the extra "dataSelected" arg?
               # rk duh! relic from the past.  Not needed.  Removed.
             } else {
               message("Spaghetti Plot needs a BDS dataset, not ADSL")
               shinyjs::alert("An ADaM BDS dataset is required for spaghetti plot")
             }
           },
           "3" = {
             # box plot module
             callModule(mod_popExpBoxp_server, id = NULL, dataset) # ac golem: left id = NULL
           }, 
           "4" = {
             # heat map module
             # commented out for now
             message("heat map is commented out for now")
             # updateAwesomeRadio(session=session, inputId = "fillType", selected = "Fill Variable")
             # callModule(mod_popExpHeat_server, id = NULL, dataset) # ac golem: left id = NULL
           },
           "5" = {
             # histogram module
             callModule(mod_popExpHist_server, id = NULL, dataset) # ac golem: left id = NULL
           },
           "6" = {
             # Means Plot module
             callModule(mod_popExpMeans_server, id = NULL, dataset) # ac golem: left id = NULL
           },
           "7" = {
             # Horizontal Bar Plot module
             callModule(mod_popExpHBar_server, id = NULL, dataset)
           },
           
           # This should not happen
           stop("invalid radio button: ",input$radio)
    )
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # observeEvent input$radio
  
}
    

    
## To be copied in the server -- done
# callModule(mod_popExp_server, "popExp_ui_1")
 
