#' @importFrom cicerone Cicerone

tg_guide <- cicerone::Cicerone$
  new()$ 
  step(
    el = "col_ADSL",
    title = "Text Input",
    description = "This is where you enter the text you want to print."
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
  )$
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
  )