
#' Individual Explorer Cicerone Guide For Selecting a Patient
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_sel <- 
  cicerone::Cicerone$
    new()$
    step(
      "cic_adv_filtering",
      "Optional: Filter list of USUBJID's",
      "Filter subjects based on variable(s) from any uploaded data set. For example, subset subject list to those who've had a certain adverse event."
    )$
    step(
      "cic_selPatNo",
      "Choose Patient (USUBJID)",
      "Upon selection, the tab will focus solely on this patient's journey through the study."
    )



#' Individual Explorer Cicerone Guide For Selecting a Patient with Advanced
#' Filtering
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_sel_adv <-
  cicerone::Cicerone$
    new()$
    step(
      "cic_adv_filtering",
      "Optional: Filter list of USUBJID's",
      "Filter subjects based on variable(s) from any uploaded data set. For example, subset subject list to those who've had a certain adverse event."
    )$
    step(
      "cic_filter_df",
      "Filtering Data Set(s)",
      "Select which data set(s) contain the variable(s) you want to filter on."
    )$
    step(
      "cic_data_filter",
      "Filtering Variables",
      "Select which variable(s) from the data set(s) selected above to filter on. You can add as many filters as you like!"
    )$
    step(
      "cic_selPatNo",
      "Choose Patient (USUBJID)",
      "Upon selection, the tab will focus solely on this patient's journey through the study."
    )

  






#' Individual Explorer Cicerone Guide For Patient Events tab when no Events selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_events_blank <-
  cicerone::Cicerone$
  new()$
  step(
    "cic_EventsTab",
    "Events vs. Visits",
    "'Events' investigates important study dates  from OCCDs files on an interactive timeline vs 'Visits' plots PARAMs by Study Visit from BDS files"
  )$
  step(
    "cic_checkGroup",
    "Plot a Timeline",
    "Upload & select more data sets to explore more dates on a interactive timeline plot"
  )


#' Individual Explorer Cicerone Guide For Patient Events tab
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_events <-
  cicerone::Cicerone$
  new()$
  step(
    "cic_EventsTab",
    "Events vs. Visits",
    "'Events' investigates important study dates  from OCCDs files on an interactive timeline vs 'Visits' plots PARAMs by Study Visit from BDS files"
  )$
  step(
    "cic_checkGroup",
    "Plot a Timeline",
    "Upload & select more data sets to explore more dates on a interactive timeline plot"
  )$
  step(
    "cic_eventsPlot",
    "Visualize Dates",
    "Zoom in & out, Pan left and right to establish a useful patient narrative"
  )$
  step(
    "cic_eventsTable",
    "Inspect Dates",
    "View, sort, and download dates data mined for plotting the timeline visual"
  )




#' Individual Explorer Cicerone Guide For Patient Events tab w/ Advanced Filtering
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_events_adv <-
  cicerone::Cicerone$
  new()$
  step(
    "cic_EventsTab",
    "Events vs. Visits",
    "'Events' investigates important study dates  from OCCDs files on an interactive timeline vs 'Visits' plots PARAMs by Study Visit from BDS files"
  )$
  step(
    "cic_checkGroup",
    "Plot a Timeline",
    "Upload & select more data sets to explore more dates on a interactive timeline plot"
  )$
  step(
    "cic_events_apply_filter",
    "Apply Advanced Filters",
    "You subsetted the USUBJID list at the top of the Individual Explorer tab. If desireable, feel free to toggle those same filters on or off for this timeline plot!"
  )$
  step(
    "cic_eventsPlot",
    "Visualize Dates",
    "Zoom in & out, Pan left and right to establish a useful patient narrative"
  )$
  step(
    "cic_eventsTable",
    "Inspect Dates",
    "View, sort, and download dates data mined for plotting the timeline visual"
  )





#' Individual Explorer Cicerone Guide For Patient Visits tab when no ADAMs selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_visits_blank <-
  cicerone::Cicerone$
  new()$
  step(
    "cic_VisitsTab",
    "Visits vs. Events",
    "'Visits' plots PARAMs by Study Visit from BDS files vs. 'Events' investigates important study dates from OCCDs files on an interactive timeline"
  )$
  step(
    "cic_plot_adam",
    "Choose a BDS",
    "Upload & select BDS files to plot AVAL by PARAMCD and visit"
  )


#' Individual Explorer Cicerone Guide For Patient Visits tab when no ADAMs selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' 
guide_ind_exp_visits <-
  cicerone::Cicerone$
  new()$
  step(
    "cic_VisitsTab",
    "Visits vs. Events",
    "'Visits' plots PARAMs by Study Visit from BDS files vs. 'Events' investigates important study dates from OCCDs files on an interactive timeline"
  )$
  step(
    "cic_plot_adam",
    "Choose a BDS",
    "Upload & select BDS files to plot AVAL by PARAMCD and visit"
  )$
  step(
    "cic_plot_param",
    "Plot by PARAMCD",
    "test text"
  )$
  step(
    "cic_visit_var",
    "And by a Visit Variable",
    "test text"
  )$
  step(
    "cic_plot_hor",
    "Include a Horizontal Line",
    "test text"
  )$
  step(
    "cic_PlotlyChart",
    "Interative Plot",
    "test text"
  )$
  step(
    "cic_DataTable",
    "Inspect Data",
    "test text"
  )$
  step(
    "cic_batchDwnld",
    "Create a Report",
    "test text"
  )

