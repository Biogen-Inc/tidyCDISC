dataUpload <- function(input, output, session, stringsAsFactors) {
  
  ns <- session$ns
  
  # initiate reactive values - list of uploaded data files
  # standard to imitate output of detectStandard.R
  dd <- reactiveValues()
  # dd <- reactiveValues(data = preload_data_list$data,
  #                      current = preload_data_list$current,
  #                      standard = preload_data_list$standard)
  
  
  # modify reactive values when data is uploaded
  observeEvent(input$file, {
  
    data_list <- list()
    
    ## data list
    for (i in 1:nrow(input$file)){
      if(length(grep(".sas7bdat", input$file$name[i], ignore.case = TRUE)) > 0){
        data_list[[i]] <- haven::zap_formats(haven::read_sas(input$file$datapath[i]))
      }else{
        data_list[[i]] <- NULL
      }
    }
    # names
    names(data_list) <- toupper(str_remove(input$file$name, ".sas7bdat"))
    
    # append to existing reactiveValues list
    dd$data <- c(dd$data, data_list)
    
    # set dd$current to FALSE for previous & TRUE for current uploads
    dd$current <- c(rep(FALSE, length(dd$current)), rep(TRUE, length(data_list)))
    
    # run detectStandard on new data and save to dd$standard
    
    standard_list <- lapply(data_list, function(x){ detectStandard(x) })
    
    #standard_list <- lapply(data_list, function(x){ detectStandard(x)$standard })
    
    dd$standard <- c(dd$standard, standard_list)
  })
  
  
  ### make a reactive combining dd$data & standard
  data_choices <- reactive({
    
    req(dd$data)
    req(dd$standard)
    
    choices  <- list()
    for (i in 1:length(dd$data)){
      choices[[i]] <- names(dd$data)[i]
    }
    
    for (i in 1:length(dd$data)){
      
      temp_standard <- dd$standard[[i]]$standard
      standard_label <- ifelse(temp_standard=="adam","AdAM",ifelse(temp_standard=="sdtm","SDTM",temp_standard))
      if(temp_standard == "none") {
        names(choices)[i] <- paste0("<p>", names(dd$data)[i], " - <em style='font-size:12px;'>No Standard Detected</em></p>")
      } else if (dd$standard[[i]]$details[[temp_standard]]$match == "full") {
        names(choices)[i] <- paste0("<p>", names(dd$data)[i], " - <em style='color:green; font-size:12px;'>", standard_label, "</em></p>")
        # If partial data spec match - give the fraction of variables matched
      } else {
        
        valid_count <- dd$standard[[i]]$details[[temp_standard]]$valid_count
        total_count <- dd$standard[[i]]$details[[temp_standard]]$invalid_count + valid_count
        
        fraction_cols  <- paste0(valid_count, "/" ,total_count)
        
        names(choices)[i] <- paste0("<p>", names(dd$data)[i], " - <em style='color:green; font-size:12px;'>", "Partial ",
                                    standard_label, " (", fraction_cols, " data settings)",  "</em></p>")
      }
    }
    return(choices)
  })
  
  
  observeEvent(input$file, {
    req(data_choices())
    vals <- data_choices()
    names(vals) <- NULL
    names <- lapply(names(data_choices()), HTML)
    
    prev_sel <- lapply(reactiveValuesToList(input), unclass)$select_file  # retain previous selection
    
    output$radio_test <- renderUI(
      radioButtons(session$ns("select_file"), label = "Inspect Uploaded Data",
      choiceNames = names, choiceValues = vals, selected = prev_sel))
    
  })
  
  # get selected dataset when selection changes
  data_selected <- eventReactive(input$select_file, {
    isolate({index <- which(names(dd$data)==input$select_file)[1]})
    dd$data[[index]]
  })
  
  # upon a dataset being uploaded and selected, generate data preview
  output$datapreview_header <- renderUI({
    data_selected()
    isolate(data_name <- input$select_file)
    h3(paste("Data Preview for", data_name))
  })
  
  output$data_preview <- DT::renderDataTable({
    DT::datatable(data = data_selected(),
                  style="bootstrap",
                  class="compact",
                  extensions = "Scroller", options = list(scrollY=400, scrollX=TRUE))
  })
  
  
  ### return all data
  return(reactive(dd$data))
}
