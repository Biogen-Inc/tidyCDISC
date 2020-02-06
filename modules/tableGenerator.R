tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
  observe({
    req(input$recipe)
    x <- input$recipe
    if (x == "DEMOGRAPHY") {
      updateRadioGroupButtons(session, "COLUMN", "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "none"), selected = "TRT01P")
    } else {
      updateRadioGroupButtons(session, "COLUMN", "Group Data By:", choices = c("TRT01P", "SEX", "RACE", "none"), selected = "none")
    }
  })
  
  AGGREGATE <- reactive({
    req(length(input$agg_drop_zone) > 0 & !(is.na(input$agg_drop_zone)))
    as.data.frame(read_html(input$agg_drop_zone) %>% html_table(fill=TRUE)) %>%
      separate(1, into = c("Aggregate", "Select"), sep=":")
  })
  
  ROWS <- reactive({
    req(length(input$block_drop_zone) > 0 & !(is.na(input$block_drop_zone)))
    as.data.frame(read_html(input$block_drop_zone) %>% html_table(fill=TRUE))
  })
  
  ALL <- reactive({
    validate(
      need(nrow(AGGREGATE()) == nrow(ROWS()),  
           if (nrow(AGGREGATE()) < nrow(ROWS())) {
             "Missing Aggregate Block"
           } else {
             "Missing Row Block"
           }))
    t <- AGGREGATE()
    p <- ROWS()
    t$Row <- p$X1
    return(t)
  })
  
  
  ######################################################################
  # Data Preperation
  ######################################################################
  
  all_data <- reactive({ 
    # Seperate ADSL and the PArAMCD dataframes
    ADSL <- datafile()$ADSL
    PARAMCD_files <- datafile()[names(datafile()) != "ADSL" ]
    
    # The pancake method: binding all rows in an rbind 
    all_PARAMCD <-
      bind_rows(PARAMCD_files, .id = "data_from")  %>% 
        arrange(SUBJID, AVISITN, PARAMCD) %>% 
        select(USUBJID, SUBJID, AVISITN, AVISIT, PARAMCD, AVAL, CHG) %>% 
        distinct(USUBJID, AVISITN, AVISIT, PARAMCD, .keep_all = TRUE) 
    
    inner_join(ADSL, all_PARAMCD, by = "USUBJID") 
    
    })
  
  # get the list of PARAMCDs
  PARAMCD_names <- reactive({
    all_data() %>% 
      select(PARAMCD) %>% 
      distinct() %>%
      pull(PARAMCD)
  })
  
  output$all <- renderTable({
    head(all_data())
  })
  
  #####################################################################
  # Block Preperation
  #####################################################################
  
  p <- reactive({
    data_for_blocks <- list()
    for (i in 1:length(datafile())) {
      ifelse((length(grep("PARAMCD", names(datafile()[[i]]))) == 0) ,
             data_for_blocks[[i]] <- names(datafile()[[i]]),
             data_for_blocks[[i]] <- block_names(datafile()[[i]]))
    }
    names(data_for_blocks) <- names(datafile())
    rowArea(data_for_blocks)
    })
  
  return(p)
}