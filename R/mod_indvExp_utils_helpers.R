
#' Individual Explorer Cicerone Guide For Selecting a Patient
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' @noRd
#' 
guide_ind_exp_sel <- 
  cicerone::Cicerone$
    new()$
    step(
      "indv_cic_adv_filtering",
      "Optional: Filter list of USUBJID's",
      "Filter subjects based on variable(s) from any uploaded data set. For example, subset subject list to those who've had a certain adverse event."
    )$
    step(
      "indv_cic_selPatNo",
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
#' @noRd
#' 
guide_ind_exp_sel_adv <-
  cicerone::Cicerone$
    new()$
    step(
      "indv_cic_adv_filtering",
      "Optional: Filter list of USUBJID's",
      "Filter subjects based on variable(s) from any uploaded data set. For example, subset subject list to those who've had a certain adverse event."
    )$
    step(
      "indv_cic_filter_df",
      "Filtering Data Set(s)",
      "Select which data set(s) contain the variable(s) you want to filter on."
    )$
    step(
      "indv_cic_data_filter",
      "Filtering Variables",
      "Select which variable(s) from the data set(s) selected above to filter on. You can add as many filters as you like!"
    )$
    step(
      "indv_cic_selPatNo",
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
#' @noRd
#' 
guide_ind_exp_events_blank <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_EventsTab",
    "Events vs. Visits",
    "The 'Events' tab which plots dates on an interactive timeline (using OCCDs files) vs the 'Visits' tab plots PARAMs by Study Visit (using BDS files)"
  )$
  step(
    "indv_cic_checkGroup",
    "Plot a Timeline",
    "Upload & select data sets to explore more pertinent dates on a interactive timeline plot. 'Milestones' come from the ADSL."
  )


#' Individual Explorer Cicerone Guide For Patient Events tab
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' @noRd
#' 
guide_ind_exp_events <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_EventsTab",
    "Events vs. Visits",
    "The 'Events' tab which plots dates on an interactive timeline (using OCCDs files) vs the 'Visits' tab plots PARAMs by Study Visit (using BDS files)"
  )$
  step(
    "indv_cic_checkGroup",
    "Plot a Timeline",
    "Upload & select data sets to explore more pertinent dates on a interactive timeline plot. 'Milestones' come from the ADSL."
  )$
  step(
    "indv_cic_eventsPlot",
    "Visualize Dates",
    "Zoom in & out, Pan left and right to establish a useful patient narrative"
  )$
  step(
    "indv_cic_eventsTable",
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
#' @noRd
#' 
guide_ind_exp_events_adv <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_EventsTab",
    "Events vs. Visits",
    "The 'Events' tab which plots dates on an interactive timeline (using OCCDs files) vs the 'Visits' tab plots PARAMs by Study Visit (using BDS files)"
  )$
  step(
    "indv_cic_checkGroup",
    "Plot a Timeline",
    "Upload & select data sets to explore more pertinent dates on a interactive timeline plot. 'Milestones' come from the ADSL."
  )$
  step(
    "indv_cic_events_apply_filter",
    "Apply Advanced Filters",
    "You subsetted the USUBJID list at the top of the Individual Explorer tab. If desireable, feel free to toggle those same filters on or off for this timeline plot!"
  )$
  step(
    "indv_cic_eventsPlot",
    "Visualize Dates",
    "Zoom in & out, Pan left and right to establish a useful patient narrative"
  )$
  step(
    "indv_cic_eventsTable",
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
#' @noRd
#' 
guide_ind_exp_visits_blank <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_VisitsTab",
    "Visits vs. Events",
    "The 'Visits' tab plots PARAMs by Study Visit (using BDS files) vs. the 'Events' tab which plots dates on an interactive timeline (using OCCDs files)"
  )$
  step(
    "indv_cic_plot_adam",
    "Choose a BDS",
    "Upload & select BDS files to plot AVAL by PARAMCD and visit"
  )


#' Individual Explorer Cicerone Guide For Patient Visits tab when ADAM selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#' 
#' @return a cicerone r6 object
#' @noRd
#' 
guide_ind_exp_visits <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_VisitsTab",
    "Visits vs. Events",
    "The 'Visits' tab plots PARAMs by Study Visit (using BDS files) vs. the 'Events' tab which plots dates on an interactive timeline (using OCCDs files)"
  )$
  step(
    "indv_cic_plot_adam",
    "Choose a BDS",
    "Upload & select the BDS file which contains the PARAMCDs to plot by visit."
  )$
  step(
    "indv_cic_plot_param",
    "Plot by PARAMCD",
    "Select one PARAMCD and the graph below will plot the corresponding AVALs on the y-axis."
  )$
  step(
    "indv_cic_visit_var",
    "Plot by a Visit Variable",
    "Visit variable goes on the x-axis and must be numeric. If available in data, you may choose from the following visit variables: AVISITN, VISITNUM, and any variable ending in 'DY'."
  )$
  step(
    "indv_cic_plot_hor",
    "Include a Horizontal Line",
    "Visually compare results from each visit against the screening or baseline visit."
  )$
  step(
    "indv_cic_PlotlyChart",
    "Interact with the Plot",
    "Zoom in & out, hover over points/lines to gain additional insights and establish a useful patient narrative through the study"
  )$
  step(
    "indv_cic_DataTable",
    "Inspect Data",
    "View, sort, and download the 'PARAM by visit' data mined to plot the visual"
  )$
  step(
    "indv_cic_batchDwnld",
    "Create a Report & Save Time",
    "Avoid clicking through every PARAMCD by downloading a report with every plot you could generate for the selected ADaM. Create an HTML report (which maintains interactivity) or PDF. Regardless, the plots will inherit all the options you've selected here. Optionally, add your unique notes to be included in the final report."
  )


#' Individual Explorer Cicerone Guide For Patient Visits tab when ADLB loaded &
#' 'dy' visit var selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#'
#' @return a cicerone r6 object
#' @noRd
#'   
guide_ind_exp_visits_adlb <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_VisitsTab",
    "Visits vs. Events",
    "The 'Visits' tab plots PARAMs by Study Visit (using BDS files) vs. the 'Events' tab which plots dates on an interactive timeline (using OCCDs files)"
  )$
  step(
    "indv_cic_plot_adam",
    "Choose a BDS",
    "Upload & select the BDS file which contains the PARAMCDs to plot by visit."
  )$
  step(
    "indv_cic_plot_param",
    "Plot by PARAMCD",
    "Select one PARAMCD and the graph below will plot the corresponding AVALs on the y-axis."
  )$
  step(
    "indv_cic_visit_var",
    "Plot by a Visit Variable",
    "Visit variable goes on the x-axis and must be numeric. If available in data, you may choose from the following visit variables: AVISITN, VISITNUM, and any variable ending in 'DY'."
  )$
  step(
    "indv_cic_plot_hor",
    "Include a Horizontal Line",
    "Visually compare results from each visit against the screening or baseline visit."
  )$
  step(
    "indv_cic_overlay_events",
    "Include Vertical Line(s) for Events",
    "Visually compare various OCCD events in relation to each visit by overlaying them on the plot."
  )$
  step(
    "indv_cic_PlotlyChart",
    "Interact with the Plot",
    "Zoom in & out, hover over points/lines to gain additional insights and establish a useful patient narrative through the study"
  )$
  step(
    "indv_cic_DataTable",
    "Inspect Data",
    "View, sort, and download the 'PARAM by visit' data mined to plot the visual"
  )$
  step(
    "indv_cic_batchDwnld",
    "Create a Report & Save Time",
    "Avoid clicking through every PARAMCD by downloading a report with every plot you could generate for the selected ADaM. Create an HTML report (which maintains interactivity) or PDF. Regardless, the plots will inherit all the options you've selected here. Optionally, add your unique notes to be included in the final report."
  )


#' Individual Explorer Cicerone Guide For Patient Visits tab when ADLB loaded &
#' 'dy' visit var selected, plus overlain events selected
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
#'
#' @return a cicerone r6 object
#' @noRd
#'   
guide_ind_exp_visits_adlb_olay <-
  cicerone::Cicerone$
  new()$
  step(
    "indv_cic_VisitsTab",
    "Visits vs. Events",
    "The 'Visits' tab plots PARAMs by Study Visit (using BDS files) vs. the 'Events' tab which plots dates on an interactive timeline (using OCCDs files)"
  )$
  step(
    "indv_cic_plot_adam",
    "Choose a BDS",
    "Upload & select the BDS file which contains the PARAMCDs to plot by visit."
  )$
  step(
    "indv_cic_plot_param",
    "Plot by PARAMCD",
    "Select one PARAMCD and the graph below will plot the corresponding AVALs on the y-axis."
  )$
  step(
    "indv_cic_visit_var",
    "Plot by a Visit Variable",
    "Visit variable goes on the x-axis and must be numeric. If available in data, you may choose from the following visit variables: AVISITN, VISITNUM, and any variable ending in 'DY'."
  )$
  step(
    "indv_cic_plot_hor",
    "Include a Horizontal Line",
    "Visually compare results from each visit against the screening or baseline visit."
  )$
  step(
    "indv_cic_overlay_events",
    "Include Vertical Line(s) for Events",
    "Visually compare various OCCD events in relation to each visit by overlaying them on the plot."
  )$
  step(
    "indv_cic_event_type_filter",
    "Optional: Subset Displayed Events",
    "User can show 'All' events, 'Manually Filter' events by hand, or when advanced pre-filtering was used on USUBJID list, inherit those filters."
  )$
  step(
    "indv_cic_overlay_event_vals",
    "Choose which Events to Display",
    "If 'Manually Filter' was chosen, then sort through the list of events selected for display."
  )$
  step(
    "indv_cic_PlotlyChart",
    "Interact with the Plot",
    "Zoom in & out, hover over points/lines to gain additional insights and establish a useful patient narrative through the study"
  )$
  step(
    "indv_cic_DataTable",
    "Inspect Data",
    "View, sort, and download the 'PARAM by visit' data mined to plot the visual"
  )$
  step(
    "indv_cic_batchDwnld",
    "Create a Report & Save Time",
    "Avoid clicking through every PARAMCD by downloading a report with every plot you could generate for the selected ADaM. Create an HTML report (which maintains interactivity) or PDF. Regardless, the plots will inherit all the options you've selected here. Optionally, add your unique notes to be included in the final report."
  )


