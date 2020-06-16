

context("Module: Indvidual Explorer Patient Visits Tab, plot function")

########
# Create some data to use for testing, using cdisc pilot ADaMs
########
# filenames <- c("adsl")
filenames <- c("adsl","advs")
named_list <- setNames(filenames, toupper(filenames))
datafile <- map(.x = named_list, function(x) eval(as.name(tolower(x))) )
input <- list(
  plot_hor = NULL,
  visit_var = "ADY",
  plot_param = "WEIGHT",
  plot_adam = "ADVS",
  overlay_events = NULL
)
usubjid <- datafile[[input$plot_adam]]$USUBJID[1]
lb_data <- 
  datafile[[input$plot_adam]] %>%
  subset(USUBJID == usubjid) %>%
  distinct()
vline_dat <- lb_data
vv_dy_name <- input$visit_var



# Create plots
gg <- fnIndvExplVisits(
  watermark = T,
  graph_output = "ggplot",
  bds_data = lb_data,
  usubjid = usubjid,
  input_plot_hor = input$plot_hor,
  input_visit_var = input$visit_var,
  input_plot_param = input$plot_param,
  input_plot_adam = input$plot_adam,
  input_overlay_events = input$overlay_events,
  vline_dat = vline_dat,
  vv_dy_name = vv_dy_name
)
p <- fnIndvExplVisits(
  watermark = FALSE,
  graph_output = "plotly",
  bds_data = lb_data,
  usubjid = usubjid,
  input_plot_hor = input$plot_hor,
  input_visit_var = input$visit_var,
  input_plot_param = input$plot_param,
  input_plot_adam = input$plot_adam,
  input_overlay_events = input$overlay_events,
  vline_dat = vline_dat,
  vv_dy_name = vv_dy_name
)

test_that("fnIndvExplVisits fun", {

  
  # initial plotly test(s)
  expect_equal(class(p),c("plotly","htmlwidget"))
  
  # Initial ggplot test(s)
  expect_is(gg$layers[[1]], "ggproto")
  expect_identical(class(gg$layers[[1]]$geom)[1], "GeomLine")
  expect_identical(class(gg$layers[[2]]$geom)[1], "GeomPoint")
  expect_identical(class(gg$layers[[3]]$geom)[1], "GeomText")
  expect_identical(gg$layers[[1]]$geom$required_aes, c("x","y"))
  expect_equal(gg$labels$y, "Weight (kg)")
  

})







