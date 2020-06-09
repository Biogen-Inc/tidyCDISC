context("table generator ui")

test_that("row block", {
  ui <- app_ui()
  expect_shinytaglist(ui)
})

test_that("row area", {
  server <- app_server
  expect_is(server, "function")
})

test_that("row palette", {
  server <- app_server
  expect_is(server, "function")
})


test_that("drop area", {
  server <- app_server
  expect_is(server, "function")
})