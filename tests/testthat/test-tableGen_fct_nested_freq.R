context("table generator NESTED FREQ stat block")

test_that("NESTED FREQ block on ADSL", {
  app_nested_freq.ADSL("SEX", "COUNTRY", NULL, tg_data)
  app_nested_freq.ADSL("SEX", "NONE", NULL, tg_data)
  app_nested_freq.ADSL("SEX", "COUNTRY", "AVISIT", tg_data)
  app_nested_freq.ADSL("SEX", "NONE", "AVISIT", tg_data)
})

test_that("NESTED FREQ block ADSL group", {
  app_nested_freq.ADSL("ITTFL", "NONE", "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("NESTED FREQ MUST have differnt var and grouping variables", {
  expect_error(app_nested_freq.ADSL("SEX", "NONE", "SEX", data = tg_data))
})
test_that("NESTED FREQ block on BDS fails", {
  expect_error(app_nested_freq.BDS("AGE", "NONE", NULL, tg_data))
})
test_that("NESTED FREQ block on NUMERIC column fails", {
  expect_error(app_nested_freq.ADSL("AGE", "NONE", NULL, tg_data))
})
test_that("NESTED FREQ block will return a data frame with num rows equal to each var's unique combinations PLUS the unique levels of the var dragged into left drop zone", {
  expect_equal(
    nrow(app_nested_freq.ADSL("SEX", "COUNTRY", NULL, tg_data)),
    nrow(unique(tg_data[,c("SEX","COUNTRY")])) + length(unique(tg_data$SEX))
    )
})



