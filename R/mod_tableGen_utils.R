#' function to change the selected input of input$COLUMN based on the recipe block
#' 
#' @param input the shiny input id to change based on the recipe dropdown
#' @param column the specific column to select within the shiny input
#' @param dataset the data to use for all choices - ADSL reactive
#' @import shiny
#' 
recipe_column <- function(input, dataset, column) {
  selectInput(input, "Group Data By:", choices = c("NONE", colnames(dataset)), selected = column)
}