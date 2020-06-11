#' function to change the selected input of input$COLUMN based on the recipe block
#' 
#' @param column the column to select the group by 
#' based on the selected recipe dropdown
#' 
recipe_column <- function(column) {
  selectInput(session$ns("COLUMN"), "Group Data By:", choices = c("NONE", colnames(ADSL())), selected = column)
}