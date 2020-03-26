IndvExpl3CheckGroup <- function(input, output, session, datafile, loaded_adams, usubjid){ #, dataselected
  
  ns <- session$ns
  
  # Initialize Waiter
  # w_events <- Waiter$new(id = c("eventsTable","eventsPlot"))
  
  output$events_header <- renderText({
    req(!is.null(datafile()))
    paste0("Patient Events by Date") #'", usubjid(), "'
  })
    
  observeEvent(input$checkGroup, {
    
    req(usubjid() != " ") # selPatNo cannot be blank - ac: not sure if Robert expects this to work like "validate(need())"

    # Clear outputs if nothing is selected
    if(is.null(input$checkGroup)){
      output$eventsTable <- DT::renderDataTable({NULL})
      output$eventsPlot <- renderTimevis({NULL})
      shinyjs::hide(id = "eventsPlot")
      shinyjs::hide(id = "eventsTable")
    }
    else{
      
      # turn on waiter screen on event elements (table and plot)
      # w_events$show()
      
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
        
        # organizing our ADSL labels for merging below
        adsl <- data.frame(datafile()[["ADSL"]])
        n <- ncol(adsl)
        labs <- data.frame(  event_var = colnames(adsl)
                             , DECODE = map_chr(1:n, function(x) attr(adsl[[x]], "label") )
        ) %>%
          mutate(event_var = as.character(event_var))
        
        ds_rec <- adsl %>%
          filter(USUBJID == usubjid()) %>%
          select(USUBJID,ends_with("DT")) %>%
          pivot_longer(-USUBJID, names_to = "event_var", values_to = "START") %>%
          subset(!is.na(START)) %>%
          left_join(labs, by = "event_var") %>% #DECODE variable exists in here
          arrange(START)%>%
          mutate(EVENTTYP = "Milestones", DOMAIN = "DS") %>%
          select(USUBJID, EVENTTYP, START, DECODE, DOMAIN)%>%
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
          select(USUBJID, EVENTTYP, LBDT, DOMAIN) %>% # Chris suggested: ADT ANALYSIS DATE, 
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
      
      
      uni_rec <-
        do.call("rbind", uni_list) %>%
        mutate(ord = ifelse(EVENTTYP == "DS", 1, 0)) %>% # for ties, show DS last
        arrange(START, ord, EVENTTYP) %>%
        filter(DOMAIN %in% c(strng)) %>%
        select(-USUBJID,-ord)
      
      # turn off waiter
      # w_events$hide()
      
      # Try to process a data table with 0 records but with column information DT will throw exception.
      if (!is.null(uni_rec) && nrow(uni_rec) > 0)
      {
        shinyjs::show(id = "eventsTable")
        shinyjs::show(id = "eventsPlot")
        output$eventsTable <- DT::renderDataTable({
          DT::datatable(uni_rec %>% select(-DOMAIN)
                        , colnames = c("Type of Event","Date of Event","Event Description")
                        , options = list(  dom = 'lftpr'
                                           , pageLength = 15
                                           , lengthMenu = list(c(15, 50, 100, -1),c('15', '50', '100', "All"))
                        )
                        , style="default"
                        # , class="compact"
          )
        })
        
        # output$eventsPlot <- renderPlotly({
        output$eventsPlot <- renderTimevis({
          # req(usubjid() != " " )
          
          plot_dat <- 
            uni_rec %>%
            mutate(
              start = START,
              content = DECODE,
              group = EVENTTYP,
              className = DOMAIN
            ) %>%
            select(start, content, group, className)
          grp_dat <- 
            uni_rec %>%
            mutate(id = EVENTTYP,
                   content = EVENTTYP,
                   className = DOMAIN,
                   style = rep("font-wieght: bold;",nrow(uni_rec))
                   ) %>%
            distinct(id, content, className)
          
            
          # number of non-MH categories
          nonMH_dat <- 
            plot_dat %>%
            filter(className != "MH")
          nonMH_n <- nonMH_dat %>% distinct(className) %>% pull() %>% length
          
          # if only 1 selected, do nothing.
          # if n_nonMH selected, then zoom to 1st portion of 1/nonMH timespan + 10% space on front
          # For example: If 2 selected and total timespan is 3 years, then zoom to 1st 1/2 the nonMH timespan + start
          
          tv <- timevis(plot_dat,
                  groups = grp_dat
                  # ,options = list(maxHeight = "400px")
                  )
          
          if(nonMH_n > 1){
            # nonMH_n <- 2
            s <- min(as.Date(nonMH_dat$start))
            e <- max(as.Date(nonMH_dat$start))
            # s<-as.Date("2019-03-26")
            # e<-as.Date("2022-01-01")
            
            old_span <- e - s
            new_span <- old_span / nonMH_n
            new_s <- as.character(s - round(new_span*.10))
            new_e <- as.character(s + new_span)
            
            tv <- tv %>%
              setOptions(list(start = new_s, end = new_e))
          }
          tv
          
        })
        
      } else {
        if (!is.null(input$checkGroup)) {
          shinyjs::alert(paste("No data available for this subject!")) 
        }
      }
    }
    
  }, ignoreNULL=FALSE) # clearing all checkboxes qualifies for an event

} # IndvExpl3CheckGroup