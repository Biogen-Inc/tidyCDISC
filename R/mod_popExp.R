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
#' @importFrom haven zap_label zap_formats
#' @importFrom purrr map walk2
#' @importFrom shinyjs show hide
#' @importFrom shinyWidgets updatePrettyRadioButtons
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
  
  # waiting_screen <- tagList(
  #   spin_folding_cube(),
  #   h4("Hold on a bit while we merge datasets...")
  # ) 
  
  # hide all the widgets
  widgets <- c("Parmstag","radio","selPrmCode","groupbox","groupbyvar","selxvar","selyvar","selzvar","seltimevar",
               "responsevar","AddPoints","animate","animateby","numBins","AddLine","AddSmooth",
               "DiscrXaxis","fillType","selectvars","runCorr","heatMapFill","errorBars","hbarOptions")
  
  # hide all the widgets using an anonymous function
  map(widgets, function(x) shinyjs::hide(x))
  
  # select the data sets
  # dataselected <- callModule(mod_selectData_server, id = NULL, datafile)
  
  rv <- reactiveValues(all_data = NULL, df = NULL)
  
  # show/hide checkboxes depending on radiobutton selection
  # observeEvent(input$done,{
  observeEvent(datafile(), {
    
    # make sure selectData has been run
    req(!is.null(datafile()))
    
    # wait until ADSL has been selected
    req("ADSL" %in% names(datafile()) )
    
    # waiter_show(html = waiting_screen, color = "lightblue")
    # Sys.sleep(0.5) # wait 1/2 second
    
    # set adv_filtering checkbox to FALSE; select Plot tab panel
    updateCheckboxInput(session = session, "adv_filtering", value = F)
    updateTabsetPanel(session = session, "tabset", selected = "Plot")
    
    inputids <- c("groupbyvar","responsevar","seltimevar","selxvar","selyvar","selzvar")
    # set these to un-selected using map() and an anonymous function
    map(inputids, function(x) updateSelectInput(session = session, x, choices = " ", selected = character(0)) )
    
    # run the radioUpdate function above
    RadioUpdate()
    
    # datakeep <- reactive({ datafile()[dataselected()] })
    # datakeep <- reactive({ datafile() })
    
    # The data used by the population explorer is going to be one of:
    # (1) one or more BDS datasets row-joined ("pancaked") together 
    #     and ADSL will be column-joined with the BDS data
    # (2) ADSL data alone
    # 
    # Also, build fake PARAMCDs for ADAE and ADCM, if they were selected.
    
    rv$df <- datafile()
    
    # Isolate ADSL 
    if ("ADSL" %in% names(datafile())) {
      
      ADSL <- datafile()$ADSL %>%
        haven::zap_formats() %>%
        mutate(PARAMCD = "ADSL", PARAM = "Subject-Level Data", AVISIT = "Baseline", AVISITN = 0) 
      sjlabelled::set_label(ADSL$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADSL$PARAM)   <- "Parameter"
      sjlabelled::set_label(ADSL$AVISIT)  <- "Analysis Visit"
      sjlabelled::set_label(ADSL$AVISITN) <- "Analysis Visit (N)"
    }
    
    # add a PARAMCD and PARAM to ADAE, if it exists and put it back in the list
    if ("ADAE" %in% names(datafile())) {
      ADAE <- datafile()$ADAE %>%
        haven::zap_formats() %>%
        filter(!AETERM %in% c(""," ",".")) %>%  # drop records where AETERM is missing
        mutate_if(is.character, list(~na_if(., ""))) %>%
        mutate(PARAMCD = "ADAE", PARAM = "Adverse Events", AVISIT = "Baseline", AVISITN = 0)
      
      sjlabelled::set_label(ADAE$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADAE$PARAM)   <- "Parameter"
      sjlabelled::set_label(ADAE$AVISIT)  <- "Analysis Visit"
      sjlabelled::set_label(ADAE$AVISITN) <- "Analysis Visit (N)"
      
      rv$df <-  append(isolate(rv$df[!names(rv$df) %in% "ADAE"]),list("ADAE" = ADAE)) 
      datafile <- reactive({ rv$df })
      

    }
    # add a PARAMCD and PARAM to ADCM, if it exists and put it back in the list
    if ("ADCM" %in% names(datafile())) {
      ADCM <- datafile()$ADCM %>%
        haven::zap_formats() %>%
        mutate_if(is.character, list(~na_if(., ""))) %>%
        mutate(PARAMCD = "ADCM", PARAM = "Concomitant Meds", AVISIT = "Baseline", AVISITN = 0)
      
      sjlabelled::set_label(ADCM$PARAMCD) <- "Parameter Code"
      sjlabelled::set_label(ADCM$PARAM)   <- "Parameter"
      sjlabelled::set_label(ADCM$AVISIT)  <- "Analysis Visit"
      sjlabelled::set_label(ADCM$AVISITN) <- "Analysis Visit (N)"

      rv$df <-  append(isolate(rv$df[!names(rv$df) %in% "ADAE"]),list("ADAE" = ADAE)) 
      datafile <- reactive({ rv$df })
      
    }
    
    # split the non-ADSL data into those which have a USUBJID or not
    NOTADSL <- datafile()[names(datafile()) != "ADSL" ]
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
      # Warning: Column `USUBJID` has different attributes on LHS and RHS of join
      all_data <- suppressWarnings(left_join(all_BDSDATA, ADSL.1, by = "USUBJID"))
      rm(ADSL.1)

    } else {
      # just ADSL by itself
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
      
      str(all_data$AGEGR)
      str(all_data$TRT01P)
      
    }
    
    rv$all_data <- all_data 
    
    # waiter_hide()
  
  }, ignoreNULL = FALSE) # observeEvent datafile()
  
  #
  # section for filtering
  #
  
  output$hide_panel <- eventReactive(input$adv_filtering, TRUE, ignoreInit = TRUE)
  outputOptions(output, "hide_panel", suspendWhenHidden = FALSE)
  
  feed_filter <- reactive({ rv$all_data })

  # IDEAFilter
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
  # Update datset, depending on adv_filtering or filtered_data() changing
  dataset <- eventReactive(list(input$adv_filtering,filtered_data()), {
    if (!is.null(filtered_data()) && input$adv_filtering == TRUE ) {
      rv$all_data  %>% semi_join(filtered_data()) 
    } else {
      rv$all_data
    }
  }) 
  
  # Update datset, depending on adv_filtering or filtered_data() changing
  # dataset <- eventReactive(list(input$adv_filtering,filtered_data()), {
  #   if (!is.null(filtered_data()) && input$adv_filtering == TRUE ) {
  #     filtered_data() 
  #   } else {
  #     feed_filter()
  #   }
  # })
    
  #
  # input$radio button processing
  #
  observeEvent(input$radio,{
    
    req(!is.null(rv$all_data))
    
    # Clear plotoutput
    output$PlotlyOut <- renderPlotly({
      NULL
    })
    # Clear datatable
    output$DataTable <- DT::renderDataTable({
      NULL
    }) 
    
    # set clear plot to FALSE, since we just cleared the plot and table
    # updateCheckboxInput(session = session, "clearplot", value = F)
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
    widgets <- c("Parmstag","radio")
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