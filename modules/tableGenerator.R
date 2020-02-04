tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
  observe({
    req(input$recipe)
    x <- input$recipe
    print(x)
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
  
  output$all <- renderTable({
    ALL()
  })
  
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