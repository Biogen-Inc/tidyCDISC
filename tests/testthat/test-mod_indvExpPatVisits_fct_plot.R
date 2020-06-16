

context("Module: Indvidual Explorer Patient Visits Tab, plot function")


# Function returns a Data frame standardized to include events from vaious OCCDs
# files
# filenames <- c("adsl")
filenames <- c("adsl","advs")
named_list <- setNames(filenames, toupper(filenames))
datafile <- map(.x = named_list, function(x) eval(as.name(tolower(x))) )


# test_that("fnIndvExplVisits fun", {
#   
#   expect_equal(
#     # fnIndvExplVisits(
#     #   watermark = FALSE,
#     #   graph_output = "plotly",
#     #   bds_data = lb_data,
#     #   usubjid = usubjid(),
#     #   input_plot_hor = input$plot_hor,
#     #   input_visit_var = input$visit_var,
#     #   input_plot_param = input$plot_param,
#     #   input_plot_adam = input$plot_adam,
#     #   input_overlay_events = input$overlay_events,
#     #   vline_dat = vline_dat(),
#     #   vv_dy_name = vv_dy_name()
#     # )
#     ,
#     
#   )
#   
# })







