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

#' Table Generator Cicerone R6 Object 
#' 
#' This object is used within the table generator module
#' to add help text to the various fields using 
#' a help buttom
#' 
#' @importFrom cicerone Cicerone

tg_guide <- cicerone::Cicerone$
  new()$
  step(
    "all-column-blocks",
    "Uploaded Data Variables",
    "Variables are grouped by uploaded datasets where you can drag any column from an ADSL into the Variables drop zone, and PARAMCDs from BDS files"
  )$
  step(
    "sortable_agg",
    "Summay Statistic Blocks",
    "Drag one of these blocks into the Stats drop zone to be paired with a corresponding column block.
    Some statistc blocks will have a dropdown once dragged to allow you to choose the AVIST."
  )$
  step(
    "all-output-blocks",
    "Drop Zones",
    "Blocks are read in rows, where you can apply a statistic block on the right to the variable on the left."
  )$
  step(
    el = "COLUMN-wrapper",
    title = "Grouping Data",
    description = "Subdivide the table's summary statistics into selected groups"
  )$
  step(
    "filter-accordion",
    "Filter Data",
    "You can apply filters to your data by selecting the column to filter by, then supplying the range or categories to include"
  )$
  step(
    "RECIPE",
    "Auto-Generated Tables",
    "This dropdown will create tables using commonly combined blocks rather than starting from scratch. If you select NONE you can clear your workspace."
  )