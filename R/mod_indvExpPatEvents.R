#' indvExpPatEvents Server Function
#' 
#' Prepare Individual Explorer Tab Events subtab with content
#'
#' @param input,output,session Internal parameters for {shiny}. 
#' @param datafile A list of dataframes
#' @param loaded_adams a character vector of loaded adam datasets
#' @param usubjid A Character string containing a USUBJID
#' @param filtered_dat a filtered dataframe containing USUBJID

#'   DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom shinyjs show hide
#' @importFrom timevis timevis  renderTimevis setOptions
#' @importFrom stringr str_replace_all str_replace
#' @noRd
#' 
mod_indvExpPatEvents_server <- function(input, output, session, datafile, loaded_adams, usubjid, filtered_dat){
  ns <- session$ns
  
  # Initialize Waiter
  # w_events <- Waiter$new(id = c("eventsTable","eventsPlot"))
  
  output$events_header <- renderText({
    req(!is.null(datafile()))
    paste0("Patient Events by Date") #'", usubjid(), "'
    # paste(input$checkGroup)
  })
  
  
  
  
  
  # if any filter is selected in IDEAFilter, then we should show the "events_apply_filter" checkbox,
  # which defaults to TRUE everytime a new patient is selected
  observeEvent(list(input$checkGroup), {
    
    req(usubjid() != "")
    
    if(any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0) & !is.null(input$checkGroup)){
      shinyjs::show(id = "events_apply_filter")
    } else {
      shinyjs::hide(id = "events_apply_filter")
    }
  })
  
  output$applied_filters <- renderUI({
    req(
      usubjid() != ""
      # & !is.null(filtered_dat())
      & any(regexpr("%>%",capture.output(attr(filtered_dat(), "code"))) > 0)
      & !is.null(input$checkGroup)
      & input$events_apply_filter == T
    )
    
    filters_in_english(filtered_dat())
    
  })
  
  
  
  
  observeEvent(list(input$checkGroup, input$events_apply_filter), {
    
    req(usubjid() != "") # selPatNo cannot be blank - ac: not sure if Robert expects this to work like "validate(need())"
    
    
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
      
      
      # See build_events_df.R
      uni_rec <- build_events(input_checkbox = input$checkGroup
                              , input_apply_filter = input$events_apply_filter
                              , my_usubjid = usubjid()
                              , my_loaded_adams = loaded_adams()
                              , my_datafile = datafile()
                              , my_filtered_dat = filtered_dat())
      
      # turn off waiter
      # w_events$hide()
      
      # Tried to process a data table with 0 records but with column information DT will throw exception.
      if (!is.null(uni_rec) && nrow(uni_rec) > 0)
      {
        shinyjs::show(id = "eventsTable")
        shinyjs::show(id = "eventsPlot")
        
        if("MH_" %in% substr(uni_rec$DOMAIN,1,3)){
          tab <- uni_rec %>% select(-START, -END, -DOMAIN)
          date_cols <- c("Start of Event","End of Event")
        }
        else{
          date_cols <- "Date of Event"
          tab <- uni_rec %>% select(-END, -tab_st, -tab_en, -DOMAIN)
        }
        
        output$eventsTable <- DT::renderDataTable(server = FALSE, {  # This allows for downloading entire data set
          DT::datatable(tab
                        , colnames = c("Type of Event", date_cols, "Event Description")
                        , extensions = "Buttons"
                        , options = list(  
                          dom = 'Blftpr'
                          , pageLength = 15
                          , lengthMenu = list(c(15, 50, 100, -1),c('15', '50', '100', "All"))
                          , buttons = list(list(
                            extend = "excel", 
                            filename = paste("PatEvents", usubjid(), "dwnd",str_replace_all(str_replace(Sys.time(), " ", "_"),":", "-"), sep = "_")
                          ))
                        )
                        , style="default"
                        # , class="compact"
          )
          # DT::datatable(ae_rec, options = list(  dom = 'lftpr'
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
            filter(substr(className, 1, 3) != "MH_")
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
        
        # cat(paste("\nSTART:",as.character(uni_rec$START), collapse = ", "))
        # cat(paste("\ntb_st:",uni_rec$tab_st, collapse = ", "))
        # Add caption if some dates were imputed 
        if ("MH_" %in% substr(input$checkGroup, 1, 3) & (
          !identical(as.character(uni_rec$START),uni_rec$tab_st) | !identical(as.character(uni_rec$END),uni_rec$tab_en)
        )){
          shinyjs::show(id = "events_tv_caption2")
          output$events_tv_caption2 <- renderText({
            "Note: Some vague & missing patient event dates were imputed in plot above. Original start and end dates from ADMH are displayed in table below."
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
  
  
}

## To be copied in the server
# callModule(mod_indvExpPatEvents_server, "indvExpPatEvents_ui_1")