
#' Population Explorer Cicerone Guide
#'
#' This object is used within the Population Explorer to add
#' help text to the various fields upon click of a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_popex <- 
  cicerone::Cicerone$
    new()$
    step(
      "pop_cic_adv_filtering",
      "Show / Hide Filter Criteria",
      "Expand to filter data based on variable(s) from any uploaded data set. For example, subset population to those who've had a certain adverse event. Collapse to save space on screen."
    )$
    step(
      "pop_cic_chart_type",
      "Choose Desired Plot",
      "Upon selection, chart-specific options will display on the left-hand pane"
    )$
    step(
      "pop_cic_chart_inputs",
      "Provide Inputs",
      "Follow the prompts which will define what is rendered on the plot"
    )$
    step(
      "pop_cic_plot",
      "Interact with the Plot",
      "Zoom in & out, hover over points/lines to gain additional insights about the population through the study"
      
    )


#' Population Explorer Cicerone Guide with Advanced Filtering
#'
#' This object is used within the Population Explorer to add help text to the
#' various fields upon click of a help buttom
#'
#' @importFrom cicerone Cicerone
#'
#' @return a cicerone r6 object
#'   
guide_popex_sel_adv <-
  cicerone::Cicerone$
  new()$
  step(
    "pop_cic_adv_filtering",
    "Show / Hide Filter Criteria",
    "When expanded, filter data based on variable(s) from any uploaded data set. Collapse to save space on screen."
  )$
  step(
    "pop_cic_apply_filters",
    "Toggle filters ON or OFF",
    "When ON, filters subset population data using data set(s) & variable(s) defined below."
  )$
  step(
    "pop_cic_filter_df",
    "Filtering Data Set(s)",
    "Select which data set(s) contain the variable(s) you want to filter on."
  )$
  step(
    "pop_cic_data_filter",
    "Filtering Variables",
    "Select which variable(s) from the data set(s) selected above to filter on. You can add as many filters as you like!"
  )$
  step(
    "pop_cic_chart_type",
    "Choose Desired Plot",
    "Upon selection, chart-specific options will display on the left-hand pane"
  )$
  step(
    "pop_cic_chart_inputs",
    "Provide Inputs",
    "Follow the prompts which will define what is rendered on the plot"
  )$
  step(
    "pop_cic_plot",
    "Interact with the Plot",
    "Zoom in & out, hover over points/lines to gain additional insights about the population through the study"
    
  )

  

  





