#' Table Generator Pretty Block lookup table
#' 
#' This object is used within the table generator module
#' to add pretty names for each stat block when displayed in the table
#' 
#' @importFrom tibble tibble
#' 
pretty_blocks <- tibble::tibble(
  Pattern = c("MEAN", "FREQ", "CHG", "Y_FREQ", "MAX_FREQ", "NON_MISSING"),
  Replacement = c("Descriptive Statistics", 
                  "Summary Counts", 
                  "Descriptive Statistics of Change from Baseline",
                  "Subject Count for those with 'Y' values",
                  "Subject Count for those at maximum value",
                  "Subject Count for those with Non Missing values")
)

#' Table Generator Cicerone R6 Object 
#' 
#' This object is used within the table generator module
#' to add help text to the various fields using 
#' a help buttom
#' 
#' @importFrom cicerone Cicerone
#' 
#' @family tableGen Functions

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
    "You can apply filters to your data by first selecting the datasets you want to filter by. This will populate a dropdown menu of
    specific columns to filter by which you can then select and filter based on specific values to include"
  )$
  step(
    "RECIPE",
    "Auto-Generated Tables",
    "This dropdown will create tables using commonly combined blocks rather than starting from scratch. If you select NONE you can clear your workspace."
  )