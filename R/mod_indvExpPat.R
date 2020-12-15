#' indvExpPat Server Function
#'
#' Prepare Individual Explorer Tab with content post selection of a UBSUBJID.
#' Specifically, show and hide certain widgets, create header outputs, populate
#' demographic info in a table, Generating checkboxes for the Events and Visits
#' tabs
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param datafile A list of dataframes
#' @param loaded_adams A character vector of loaded adam datasets
#' @param filtered_dat A IDEAFilter output data frame containing USUBJID
#'
#' @import shiny
#' @import dplyr
#' @importFrom purrr map
#' @importFrom shinyjs show hide
#' @importFrom timevis renderTimevis
#' @importFrom plotly renderPlotly
#' @importFrom tidyselect any_of
#' @importFrom stringr str_to_title
#'
#' @return character string containing a USUBJID
#'
#' @family indvExp Functions
#'   
mod_indvExpPat_server <- function(input, output, session, datafile, loaded_adams, filtered_dat){
  ns <- session$ns
  
  # Header that depends on a few items existing
  output$demog_header <- renderText({
    req(!is.null(datafile()) & input$selPatNo != "")
    paste0("Patient Demographic Info")
  })
  
  # Patient USUBJID subtitle that depends on a few items existing. Assigned 3
  # times because shiny won't allow  the same ui object to be used more than
  # once
  output$subjid_subtitle1 <- output$subjid_subtitle2 <- output$subjid_subtitle3 <- renderText({
    req(!is.null(datafile()) & input$selPatNo != "")
    paste0("USUBJID: '",input$selPatNo,"'")
  })
  
  
  # Set up a bunch of objects once we have a selection for input selPatno 
  observeEvent(input$selPatNo, {
    req(input$selPatNo != "")
    
    # Show the bulk of the IndvExpl widgets once a patient number was selected
    show_em <- c("demog_header", "subjid_subtitle1", "demogInfo", "mytabs", "events_header",
                 "subjid_subtitle2","checkGroup","plot_header", "subjid_subtitle3", "plot_adam")
    purrr::map(show_em, ~shinyjs::show(.x))
    
    
    # Clear datatables and plots 
    output$DataTable<- DT::renderDataTable({NULL})
    output$PlotChart <- plotly::renderPlotly({NULL})
    output$eventsTable <- DT::renderDataTable({NULL})
    output$eventsPlot <- timevis::renderTimevis({NULL})
    output$events_tv_caption1 <- renderText({NULL})
    output$events_tv_caption2 <- renderText({NULL})
    hide_em <- c("events_tv_caption1", "events_tv_caption2", "eventsPlot", "eventsTable", "display_dy",
                 "overlay_events","overlay_event_vals")
    purrr::map(hide_em, ~shinyjs::hide(.x))
    output$display_dy <- renderText({NULL})
    
    
    
    output$demogInfo <- DT::renderDataTable({
      
      # grab demographic variables of interest. Notice if COUNTRYC doesn't exist, we'll grab country. If neither exists, one_of() will throw a
      # warning, but it the select() will still execute
      adsl <- datafile()[["ADSL"]]
      adsl_rec <- datafile()[["ADSL"]] %>%
        filter(USUBJID == input$selPatNo) %>%
        select(any_of("RGNGR1")
               , any_of(ifelse("COUNTRYC" %in% colnames(adsl),"COUNTRYC","COUNTRY"))
               , any_of("RACE") 
               , any_of("SEX")
               , any_of("AGE"), any_of(ifelse("AGEGR" %in% colnames(adsl),"AGEGR","AGEGR1"))
               , any_of("HEIGHTBL"), any_of("HEIGHTU")
               , any_of("WEIGHTBL"), any_of("WEIGHTU")
               , any_of("SITEID"), any_of("TRTP"), any_of("TRT01P")) 
      
      adsl_rec <- as.data.frame((adsl_rec)) # 'data' must be 2-dimensional (e.g. data frame or matrix)
      
      # col position below depends on if country exists in ADSL (it is not required to exist), but it is always the last column
      DT::datatable(adsl_rec,
                    style="default",
                    class="compact",
                    options = list(bSort = FALSE,dom = 't'),
                    rownames = FALSE,
                    colnames = if(any(c("TRT01P","TRTP") %in% colnames(adsl_rec))) c('Planned Treatment Group' = ncol(adsl_rec)) else colnames(adsl_rec) 
      )
    })
    
    
    # update checkboxes on both Events and Visits Tabs
    # Initialize
    checked1 <- NA
    checked2 <- NA
    checked3 <- NA
    checked4 <- NA
    checked5 <- NA
    mh_names <- NA
    checked6 <- NA

    # check for "adsl" (required), "adae", "adcm", and "adlb"
    if ("ADSL" %in% loaded_adams()) { checked1 <- "DS" }
    if ("ADAE" %in% loaded_adams()) {
      # if the chose patient has any adverse events, then include that AE checkbox
      if(datafile()[["ADAE"]] %>% filter(USUBJID == input$selPatNo) %>% nrow()) {
        checked2 <- "AE"
      }
    }
    if ("ADCM" %in% loaded_adams()) { 
      if(datafile()[["ADCM"]] %>% filter(USUBJID == input$selPatNo) %>% nrow()) {
        checked3 <- "CM"
      }
    }
    if ("ADLB" %in% loaded_adams()) { 
      if(datafile()[["ADLB"]] %>% filter(USUBJID == input$selPatNo) %>% nrow()) {
        checked4 <- "LB" 
      }
    }
    if ("ADMH" %in% loaded_adams()) {
      # For ADMH, we want to create separate checkboxes for each type of 
      # Medical History Category that exist in the ADMH for the selected patient.
      if(datafile()[["ADMH"]] %>% filter(USUBJID == input$selPatNo) %>% nrow()) {
        mh_names <-
          datafile()[["ADMH"]] %>%
          filter(USUBJID == input$selPatNo) %>%
          distinct(MHCAT) %>%
          pull()%>%
          stringr::str_to_title()
        checked5 <- paste0("MH_",sapply(strsplit(mh_names, " "), function(x){
          toupper(paste(substring(x, 1, 1), collapse = ""))}))
      }
    }
    if ("ADLBC" %in% loaded_adams()) {
      if(datafile()[["ADLBC"]] %>% filter(USUBJID == input$selPatNo) %>% nrow()) {
        checked6 <- "LC" 
      }
    }
    
    # Combine all into a list
    choices <- as.list(unlist(c(list(checked1,checked2,checked3,checked4,as.list(checked5),checked6))))
    names <- c("Milestones","Adverse Events","Concomitant Meds","Labs",mh_names,"Chem Labs") # ac: labels
    
    # build a named list & Remove NULLs from the list
    choices <- setNames(choices,names)
    choices <- choices[!sapply(choices,is.na)]
    
    # update the checkbox group
    updateCheckboxGroupInput(
      session = session,
      inputId = "checkGroup",
      choices = unlist(choices),
      selected = NULL,
      inline = TRUE)
    
    #######################################
    # Version for vlines on Visits Graph
    #######################################
    # You can only overlay Milestones, Adverse Events, and Con Meds
    choices2 <- as.list(unlist(c(list(checked1,checked2,checked3))))
    names2 <- names[1:3]
    
    # Setting up colors too
    vline_eventtype_cols <- my_cols[1:3] # my_cols defined in utils_strObjs.R
    v_event_cols <- setNames(vline_eventtype_cols,names2)
    dashes <- c("solid","dotted","dashed")
    v_event_lines <- setNames(dashes,names2)
    
    # build a named list & Remove NULLs from the list
    choices2 <- setNames(choices2,names2)
    choices2 <- choices2[!sapply(choices2,is.na)]
    
    updateCheckboxGroupInput(
      session = session,
      inputId = "overlay_events",
      choices = unlist(choices2), # optionally convert list to array
      selected = NULL)
    
  }) # observeEvent
  
  
  # return selected patient USUBJID from module
  return(reactive({ input$selPatNo }))
  
}

## To be copied in the server -- done
# callModule(mod_indvExpPat_server, "indvExpPat_ui_1")
