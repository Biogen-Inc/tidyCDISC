tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
  test <- reactive({ print(input$agg_drop_zone )})
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
    return(t, tags$script("script.js"))
  })
  
  output$all <- renderTable({
    ALL()
  })
  
  p <- reactive({ rowArea(datafile()) })
  return(p)
}