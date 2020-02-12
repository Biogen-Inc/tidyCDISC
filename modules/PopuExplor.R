PopuExplor <- function(input, output, session, datafile){
  
  ns <- session$ns
  
  dataselected <- callModule(selectData, id = NULL, datafile)

# show/hide checkboxes depending on radiobutton selection
observeEvent(input$radio,{
  
  req(!is.null(datafile()))

  datakeep <- reactive({ datafile()[dataselected()] })
  
  # shiny::validate(
  #     need("ADSL" %in% names(datakeep()), "ADSL is required. Go back and select it.")
  # )

  # Guard against user forgetting to select an ADSL dataset
  if (!"ADSL" %in% names(datakeep())) {
    shinyjs::alert("An ADSL dataset is required.")
    return()
   }

  # Isolate ADSL 
  ADSL <- zap_label(datakeep()$ADSL) %>%
    select(USUBJID, AGE, AGEGR, ARM, BMIBL, COUNTRY, COUNTRYC, DIAGYRS, EDSSBL, HEIGHTBL, 
           RANDDT, RANDFL, RACE, RANDDT, RANDFL, SAFFL, SEX, SEXN, SITEID, TRT01P, WEIGHTBL)
  # Remove ADSL from list of data frames
  PARAMCD <- datakeep()[names(datakeep()) != "ADSL" ]
  
  if (!is_empty(PARAMCD)) {
    
    vars <- c("USUBJID","SUBJID","PARAMCD","AVISITN","AVISIT","AVAL","BASE","CHG","data_from")
    # Bind all the PARAMCD files 
    all_PARAMCD <- bind_rows(PARAMCD, .id = "data_from")  %>% 
      select(ends_with("DY"), starts_with("AVIS"), which(names(.) %in% vars)) %>%
      filter(!is.na(AVISITN)) %>% # drop missing AVISITN/AVISIT
      distinct(USUBJID, PARAMCD, AVISITN, .keep_all = TRUE)
    
    # Join ADSL and all_PARAMCD
    all_data <- inner_join(ADSL, all_PARAMCD, by = "USUBJID")
  } else {
    all_data <- ADSL %>%
      mutate(data_from = "ADSL", PARAMCD = "NA", AVISIT = "Baseline", AVISITN = 0)
  }
  
  # get numeric vs char data   
  chrcols <- sort(names(all_data[ , which(sapply(all_data,is.character))])) # all chr columns
  numcols <- sort(names(all_data[ , which(sapply(all_data,is.numeric))]))   # all num columns
  
  # print(chrcols)
  # Clear plotoutput
  output$PlotlyOut <- renderPlotly({
    NULL
  })
  # Clear datatable
  output$DataTable <- DT::renderDataTable({
    NULL
  })  
  
  switch(input$radio, # use swtich() instead of if/else
         "1" = {
           # scatter plot module
           dataset <- reactive({ all_data })
           callModule(PopuExpl1Scat, id = NULL, dataset, numcols = numcols, chrcols = chrcols)
         },
         "2" = {
           # spaghetti plot module
           # if ADSL is in data_from then no BDS datasets were selected
           if (!"ADSL" %in% unique(all_data$data_from)) {
             dataset <- reactive({ all_data }) 
             callModule(PopuExpl2Spag, id = NULL, dataselected, dataset)
           } else {
             shinyjs::alert("An ADaM BDS dataset is required for spaghetti plot")
           }
         },
         "3" = {
           # box plot module
           dataset <- reactive({ all_data })
           callModule(PopuExpl3Boxp, id = NULL, dataset, numcols = numcols, chrcols = chrcols)
         },
         "4" = {
           # heat map module
           dataset <- reactive({ all_data })
           callModule(PopuExpl4Heat, id = NULL, dataset, numcols = numcols, chrcols = chrcols)
         },
         "5" = {
           # histogram module
           dataset <- reactive({ all_data })
           callModule(PopuExpl5Hist, id = NULL, dataset, numcols = numcols, chrcols = chrcols)
         },
         # This should not happen
         stop("invalid",input$radio,"value")
         )
  
  updateRadioButtons(session,"radio",selected = character(0))
  
}, ignoreInit = TRUE)
}   # PopuExplor