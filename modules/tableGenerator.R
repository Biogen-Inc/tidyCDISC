tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  
  
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
    ALL()$t
  })
  
  p <- reactive({
    rowArea(datafile())
    })
  return(p)
}