IndvExpl3CheckGroup <- function(input, output, session, datafile, loaded_adams, usubjid){ #, dataselected
  
  ns <- session$ns
  
observeEvent(input$checkGroup, {
  
  req(usubjid() != " ") # selPatNo cannot be blank - ac: not sure if Robert expects this to work like "validate(need())"
  
  # Clear eventsTable
  output$eventsTable <- DT::renderDataTable({
    NULL
  })
  # Here we collect data for adae, ds (from adsl), adcm and adlb
  # and then combine the ones selected in input$checkGroup
  # DOMAIN is used to match the input$checkGroup string
  
  if ("ADAE" %in% loaded_adams() ) { # ac: first part not needed?
    ae_rec <- datafile()[["ADAE"]] %>%
      filter(USUBJID == usubjid()) %>%
      filter(!is.na(AESTDT)) %>%
      mutate(EVENTTYP = "Adverse Event", DOMAIN = "AE") %>%
      select(USUBJID, EVENTTYP, AESTDT, AEDECOD, AESEV, AESER, DOMAIN) %>%
      mutate(
        START = AESTDT,
        DECODE = paste(AEDECOD, "AESEV:", AESEV, "AESER:", AESER)
      ) %>%
      select(-starts_with("AE")) %>%
      distinct(.keep_all = TRUE)
  } else {
    ae_rec <- NULL
  }
  
  if ("ADSL" %in% loaded_adams() ) {
    ds_rec <- datafile()[["ADSL"]] %>%
      filter(USUBJID == usubjid()) %>%
      mutate(EVENTTYP = "Subject Status", DOMAIN = "DS") %>%
      select(USUBJID, EVENTTYP, LAST1SDT, STUREA1, DOMAIN) %>%
      rename(START = LAST1SDT, DECODE = STUREA1) %>%
      select(-starts_with("DS"))
  } else {
    ds_rec <- NULL
  }
  
  if ("ADCM" %in% loaded_adams() ) {
    cm_rec <- datafile()[["ADCM"]] %>%
      filter(USUBJID == usubjid()) %>%
      filter(CMDECOD != "") %>%
      mutate(EVENTTYP = "Concomitant Medications", DOMAIN = "CM") %>%
      select(USUBJID, EVENTTYP, CMSTDT, CMDECOD, DOMAIN) %>%
      mutate(START = CMSTDT, DECODE = CMDECOD) %>%
      select(-starts_with("CM")) %>%
      distinct(.keep_all = TRUE)
  } else {
    cm_rec <- NULL
  }
  
  if ("ADLB" %in% loaded_adams() ) {
    lb_rec <- datafile()[["ADLB"]] %>%
      filter(USUBJID == usubjid()) %>%
      mutate(EVENTTYP = "Lab Results", DOMAIN = "LB") %>%
      select(USUBJID, EVENTTYP, LBDT, DOMAIN) %>%
      mutate(START = LBDT, DECODE = "Labs Drawn") %>%
      select(-starts_with("LB")) %>%
      distinct(.keep_all = TRUE)
  } else {
    lb_rec <- NULL
  }
  
  strng <- input$checkGroup
  
  # Remove NULLs from the list
  uni_list <- list(ds_rec, ae_rec, cm_rec, lb_rec)
  uni_list <- uni_list[!sapply(uni_list,is.null)]
  
  # print some stuff
  # cat(paste("uni_list:", uni_list))
  # cat(paste("\nADSL in datafile?", "ADSL" %in% loaded_adams()))
  # cat(paste("\nChoices list:", choices))
  # cat(paste("\nChoices unlisted:", unlist(choices)))
  
  uni_rec <-
    do.call("rbind", uni_list) %>%
    mutate(ord = ifelse(EVENTTYP == "DS", 1, 0)) %>% # for ties, show DS last
    arrange(START, ord, EVENTTYP) %>%
    filter(DOMAIN %in% c(strng)) %>%
    select(-USUBJID,-ord,-DOMAIN)
  
  # Try to process a data table with 0 records but with column information DT will throw exception.
  if (!is.null(uni_rec) && nrow(uni_rec) > 0)
  {
    output$eventsTable <- DT::renderDataTable({
      DT::datatable(uni_rec, colnames = c("Type of Event","Date of Event","Comments"),options = list(dom = 'ftp', pageLength = 15))
    })
  } else {
    if (!is.null(input$checkGroup)) {
      shinyjs::alert(paste("No data available for this subject!")) 
    }
  }
}, ignoreNULL=FALSE) # clearing all checkboxes qualifies for an event
} # IndvExpl3CheckGroup