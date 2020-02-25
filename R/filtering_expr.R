filtering_expr <- function(input, datafile) {
  column <- rlang::sym(input$filtering)
  operator <- allowed_operators[[input$condition]]
  if (is.null(operator)) {
    rlang::abort(glue::glue("Can't use operator `{input$condition}`"))
  }
  
  if (grepl("[A-Za-z]", datafile$ADSL[[input$filtering]][1])) {
    value <- input$filt_grp
  } else {
    value <- as.numeric(input$filt_grp)
  }
  
  call <- rlang::call2(operator, column, value)
  rlang::as_quosure(call, env = emptyenv())
}