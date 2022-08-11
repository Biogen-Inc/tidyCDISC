
#' indvExp Server Function
#'
#' Prepare Individual Explorer Tab with server side code which sets up some
#' initial properties on the tab. Specifically, the user will be able to
#' pre-filter the loaded data in order to select a subject using IDEAFilter.
#' That filtered data will be sent to additional modules for ingestion.
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param datafile A list of dataframes

#' @import shiny
#' @import dplyr
#' @importFrom IDEAFilter shiny_data_filter
#' @importFrom stringr str_detect 
#' @importFrom purrr map update_list reduce
#' @importFrom shinyjs show hide
#' @importFrom rlang is_empty
#' 
#' @return character vector of loaded adams and a filtered dataframe to populate mod_indvExpPat module
#' 
#' @family indvExp Functions
#' @noRd
#' 
mod_indvExp_server <- function(input, output, session, datafile){
  ns <- session$ns
  
  # Initialize reactive vals df called 'processed_data'
  rv = reactiveValues(processed_data = NULL)
  
  
  
  # When the user asks for help, guide them through the UI
  observeEvent( input$help_sel, {
    if(input$adv_filtering == TRUE){
      guide_ind_exp_sel_adv$init()$start() # guide includes IDEAFilter
    } else {
      guide_ind_exp_sel$init()$start()
    }
  })
  
  # Only select data that starts with AD followed by one or more alphanumerics or underscore
  my_loaded_adams <- reactive({
    req(!is.null(datafile()))
    sasdata0 <- toupper(names(datafile()))
    sasdata <- names(which(sapply(sasdata0,function(df) { return(stringr::str_detect(toupper(df),"^AD[A-Z0-9\\_]+")) })))
    return(sasdata)
  })
  
  
  # If User wants to perform advance filtering, update drop down of data frames they can filter on
  observe({
    req(input$adv_filtering == TRUE)
    updateSelectInput("filter_df", session = session, choices = as.list(my_loaded_adams()), selected = "ADSL") #
  })
  
  
  # upon selection of data set(s) to filter above, combine to feed
  # shiny_data_filter module. If nothing is selected, use ADSL by default
  pre_processed_data <- eventReactive(input$filter_df, {
    req(input$adv_filtering == TRUE)
    
    # grab only df's included in the filter
    select_dfs <- datafile()[input$filter_df]
    
    # Separate out non BDS and BDS data frames. Note: join may throw some
    # warnings if labels are different between two datasets, which is fine!
    # Ignore
    non_bds <- select_dfs[sapply(select_dfs, function(x) !("PARAMCD" %in% colnames(x)) )] 
    BDS <- select_dfs[sapply(select_dfs, function(x) "PARAMCD" %in% colnames(x) )]
    
    # Make CHG var doesn't exist, create the column and populate with NA
    PARAMCD_dat <- purrr::map(BDS, ~ if(!"CHG" %in% names(.)) {purrr::update_list(., CHG = NA)} else {.})
    
    # Combine selected data into a 1 usable data frame
    if (!rlang::is_empty(PARAMCD_dat)) {
      all_PARAMCD <- bind_rows(PARAMCD_dat, .id = "data_from") %>% distinct(.keep_all = TRUE)
      
      if (!rlang::is_empty(non_bds)){
        combined_data <- inner_join(non_bds %>% purrr::reduce(inner_join), all_PARAMCD)
      } else {
        combined_data <-all_PARAMCD
      }
    } else {
      combined_data <- non_bds %>% reduce(inner_join)
    }
    
    return(
      if(!rlang::is_empty(input$filter_df)){
        combined_data
      } else{
        datafile()$ADSL
      }
    )
  })
  
  # If not pre-filtering, use ADSL to feed to IDEAFilter
  observe({
    req(!is.null(datafile()))
    if(input$adv_filtering){
      rv$processed_data <- pre_processed_data()
    } else {
      rv$processed_data <- datafile()$ADSL
    }
  })
  feed_filter <- reactive({rv$processed_data}) # must make reactive
  
  
  # Feed IDEAFilter! Returns data frame to use down stream... May be filtered or not
  filtered_data <- callModule(
    IDEAFilter::shiny_data_filter,     # Module name
    "data_filter",         # whatever you named the widget
    data = feed_filter,    # the name of your pre-processed data
    verbose = FALSE)
  
  
  
  observe({
    req(!is.null(filtered_data())) # make sure we have an output data frame from IDEAFilter
    
    subj <- unique(filtered_data()$USUBJID) # get list of unique USUBJIDs
    
    # Update USUBJIDs based on IDEAFilter output
    updateSelectInput(
      session = session,
      inputId = "selPatNo",
      choices = c("",subj),
      selected = ""
    )
    shinyjs::show(id = "selPatNo") # show widget
    
    # Hide widgets until the input file has been selected 
    hide_init <- c("demog_header", "subjid_subtitle1", "demogInfo", "mytabs", "events_header",
                   "subjid_subtitle2", "events_apply_filter","checkGroup","eventsPlot",
                   "events_tv_caption1","events_tv_caption2","eventsTable",
                   "plot_header", "subjid_subtitle3", "plot_adam", "event_type_filter", "plot_param",
                   "visit_var", "plot_hor", "display_dy", "overlay_events", "overlay_event_vals", ""
    )
    purrr::map(hide_init, ~ shinyjs::hide(.x))
    output$display_dy <- renderUI({NULL}) # make NULL
    
  })
  
  # pass the filtered data from above as a reactive
  return_filtered <- reactive({
    if(input$selPatNo != ""){
      filtered_data() %>% filter(USUBJID == input$selPatNo)
    } else {
      filtered_data()
    }
  })
  
  
  return(list(my_loaded_adams = my_loaded_adams, all_data = return_filtered))
}



## To be copied in the server -- Done
# callModule(mod_indvExp_server, "indvExp_ui_1")
