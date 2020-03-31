IndvExpl3CheckGroup <- function(input, output, session, datafile, loaded_adams, usubjid){ #, dataselected
  
  ns <- session$ns
  
  # Initialize Waiter
  # w_events <- Waiter$new(id = c("eventsTable","eventsPlot"))
  
  output$events_header <- renderText({
    req(!is.null(datafile()))
    paste0("Patient Events by Date") #'", usubjid(), "'
    # paste(input$checkGroup)
  })
    
  
  observeEvent(input$checkGroup, {
    
    req(usubjid() != " ") # selPatNo cannot be blank - ac: not sure if Robert expects this to work like "validate(need())"

    # Clear outputs if nothing is selected
    if(is.null(input$checkGroup)){
      output$eventsTable <- DT::renderDataTable({NULL})
      output$eventsPlot <- renderTimevis({NULL})
      output$events_tv_caption1 <- renderText({NULL})
      output$events_tv_caption2 <- renderText({NULL})
      shinyjs::hide(id = "events_tv_caption1")
      shinyjs::hide(id = "events_tv_caption2")
      shinyjs::hide(id = "eventsPlot")
      shinyjs::hide(id = "eventsTable")
    }
    else{
      
      # turn on waiter screen on event elements (table and plot)
      # w_events$show()
      
      # Here we collect data for adae, ds (from adsl), adcm and adlb
      # and then combine the ones selected in input$checkGroup
      # DOMAIN is used to match the input$checkGroup string
      cat(paste("\n","AE" %in% c(input$checkbox)))
      
      if ("ADAE" %in% loaded_adams() ) { # ac: first part not needed?
        if("AESTDT" %in% colnames(datafile()[["ADAE"]])){
          ae_rec <- datafile()[["ADAE"]] %>%
            filter(USUBJID == usubjid()) %>%
            filter(!is.na(AESTDT)) %>%
            mutate(EVENTTYP = "Adverse Event", DOMAIN = "AE") %>%
            select(USUBJID, EVENTTYP, AESTDT, AEDECOD, AESEV, AESER, DOMAIN) %>%
            mutate(
              START = AESTDT,
              END = AEENDT,
              tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)),
              tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END)),
              DECODE = paste(AEDECOD, "AESEV:", AESEV, "AESER:", AESER)
            ) %>%
            select(-starts_with("AE")) %>%
            distinct(.keep_all = TRUE)
        } else{
          # if("AE" %in% c(input$checkbox)){
          #   shinyjs::alert(paste("Cannot add Adverse Events: no AESTDT variable exists in the loaded ADAE.")) 
          # }
          ae_rec <- NULL
        }
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
          mutate(EVENTTYP = "Milestones", DOMAIN = "DS",
                 END = NA,
                 tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)),
                 tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END))
                 ) %>%
          select(USUBJID, EVENTTYP, START, END,
                 tab_st,
                 tab_en,
                 DECODE, DOMAIN)%>%
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
          mutate(START = CMSTDT,
                 END = NA, 
                 tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)),
                 tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END)),
                 DECODE = CMDECOD) %>%
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
          mutate(START = LBDT,
                 END = NA,
                 tab_st = ifelse(as.character(START) == "", NA_character_, as.character(START)),
                 tab_en = ifelse(as.character(END) == "", NA_character_, as.character(END)),
                 DECODE = "Labs Drawn") %>%
          select(-starts_with("LB")) %>%
          distinct(.keep_all = TRUE)
      } else {
        lb_rec <- NULL
      }
      if ("ADMH" %in% loaded_adams() ) {
        mh_rec <- datafile()[["ADMH"]] %>%
          filter(USUBJID == usubjid()) %>%
          mutate(EVENTTYP = "Medical History",
                 DOMAIN = "MH",
                 START = as.Date(case_when(nchar(MHSTDTC) == 10 ~ MHSTDTC,
                                     nchar(MHSTDTC) == 7 ~ paste0(MHSTDTC,"-15"),
                                     nchar(MHSTDTC) == 4 ~ paste0(MHSTDTC,"-07-15"),
                                     # nchar(MHSTDTC) == 0 ~ "",
                                     TRUE ~ NA_character_)),
                 END = as.Date(case_when(nchar(MHENDTC) == 10 ~ MHENDTC,
                                 nchar(MHENDTC) == 7 ~ paste0(MHENDTC,"-15"),
                                 nchar(MHENDTC) == 4 ~ paste0(MHENDTC,"-07-15"),
                                 # nchar(MHENDTC) == 0 ~ "",
                                 TRUE ~ NA_character_)),
                 tab_st = ifelse(MHSTDTC == "", NA_character_, MHSTDTC),
                 tab_en = ifelse(MHENDTC == "", NA_character_, MHENDTC),
                 DECODE = MHTERM
          ) %>%
          select(USUBJID, EVENTTYP, START, END, tab_st, tab_en, DECODE, DOMAIN) %>%
          distinct(.keep_all = TRUE)
      } else {
        mh_rec <- NULL
      }
      strng <- input$checkGroup
      
      # Remove NULLs from the list
      uni_list <- list(ds_rec, ae_rec, cm_rec, lb_rec, mh_rec)
      uni_list <- uni_list[!sapply(uni_list,is.null)]
      
      
      uni_rec <-
        do.call("rbind", uni_list) %>%
        mutate(ord = ifelse(EVENTTYP == "DS", 1, 0),
               sort_start = if_else(is.na(START), as.Date("1900-01-01"), START), # If start is null, show at beginning of table
               END = as.Date(END, origin="1970-01-01")
               ) %>% # for ties, show DS last
        arrange(sort_start, ord, EVENTTYP) %>%
        filter(DOMAIN %in% c(strng)) %>%
        select(-USUBJID, -ord, -sort_start)
      
      
      # turn off waiter
      # w_events$hide()
      
      # Try to process a data table with 0 records but with column information DT will throw exception.
      if (!is.null(uni_rec) && nrow(uni_rec) > 0)
      {
        shinyjs::show(id = "eventsTable")
        shinyjs::show(id = "eventsPlot")
        
        if("MH" %in% uni_rec$DOMAIN){
          tab <- uni_rec %>% select(-START, -END, -DOMAIN)
          date_cols <- c("Start of Event","End of Event")
        }
        else{
          date_cols <- "Date of Event"
          tab <- uni_rec %>% select(-END, -tab_st, -tab_en, -DOMAIN)
        }
        
        output$eventsTable <- DT::renderDataTable({
          DT::datatable(tab
                        , colnames = c("Type of Event", date_cols, "Event Description")
                        , options = list(  dom = 'lftpr'
                                           , pageLength = 15
                                           , lengthMenu = list(c(15, 50, 100, -1),c('15', '50', '100', "All"))
                        )
                        , style="default"
                        # , class="compact"
          )
          # DT::datatable(mh_rec, options = list(  dom = 'lftpr'
          #                                        , pageLength = 15
          #                                        , lengthMenu = list(c(15, 50, 100, -1),c('15', '50', '100', "All"))
          # ))
        })
        
        # Create timevis object for interactive timeline
        output$eventsPlot <- renderTimevis({
        
          plot_dat <- 
            uni_rec %>%
            subset(!is.na(START)) %>%
            mutate(
              start = START,
              end = END,
              content = DECODE,
              group = EVENTTYP,
              className = DOMAIN
            ) %>%
            select(start, end, content, group, className)
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
            s <- min(as.Date(nonMH_dat$start))
            e <- max(as.Date(nonMH_dat$start))
            
            old_span <- e - s
            new_span <- old_span / nonMH_n
            new_s <- as.character(s - round(new_span*.10))
            new_e <- as.character(s + new_span)
            
            tv <- tv %>%
              setOptions(list(start = new_s, end = new_e))
          }
          tv
          
        }) # end of Render_timevis

        # Add caption if there are events not shown in the timeline due to missing dates
        
        if (uni_rec %>% subset(is.na(START)) %>% nrow() > 0){
          shinyjs::show(id = "events_tv_caption1")
          output$events_tv_caption1 <- renderText({
            "Note: Some patient event dates were not plotted since the loaded data contained missing dates. These events are still included in the table below."
          })
        }else {
          shinyjs::hide(id = "events_tv_caption1")
          output$events_tv_caption1 <- renderText({NULL})
        }
        
        # cat(paste("\n",uni_rec %>% subset(is.na(START)) %>% nrow() > 0))
        # Add caption if some dates were imputed 
        if (!identical(as.character(uni_rec$START),uni_rec$tab_st) | !identical(as.character(uni_rec$END),uni_rec$tab_en)){
          shinyjs::show(id = "events_tv_caption2")
          output$events_tv_caption2 <- renderText({
            "Note: Some patient event dates plotted were imputed when vague. If only a year was provided, July 15 was appeneded for the year & month. If Year-Month was provided, the day of the 15th was imputed."
          })
        }else {
          shinyjs::hide(id = "events_tv_caption2")
          output$events_tv_caption2 <- renderText({NULL})
        }
        
        
      } else {
        if (!is.null(input$checkGroup)) {
          shinyjs::alert(paste("No data available for this subject!")) 
        }
      }
    }
    
  }, ignoreNULL=FALSE) # clearing all checkboxes qualifies for an event

} # IndvExpl3CheckGroup