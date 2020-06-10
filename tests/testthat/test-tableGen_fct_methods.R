context("table generator server")

agg <- list(numbers = list(list(txt = "MEAN",val = "NONE")))
block <- list(numbers = list(list(txt = "\n  AGE\n  ",df = "ADSL")))

test_that("create custom class based on dataframe", {
  test_block <- custom_class("AGE", "ADSL")
  expect_equal(class(test_block)[2], "ADSL")
})

test_that("convertTGOutput creates a dataframe row from input blocks", {
  output <- convertTGOutput(agg, block)
  
  expect_equal(output$agg, "MEAN")
  expect_equal(output$block, "AGE")
  expect_equal(output$dataset, "ADSL")
  expect_equal(class(output$S3[[1]])[2], "ADSL")
  expect_equal(output$gt_group, "MEAN of AGE")
})

test_that("combining all idea functions as IDEA_methods", {
  output <- convertTGOutput(agg, block)
  IDEA_methods(output$agg, output$S3[[1]], output$dropdown, NULL, tg_data)
})