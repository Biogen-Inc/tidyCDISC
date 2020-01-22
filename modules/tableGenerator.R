tableGenerator <- function(input, output, session, datafile = reactive(NULL)) {
  p <- reactive({ rowArea(datafile()) })
  return(p)
}