context("table generator non missing stat block")

test_that("non missing block on ADSL", {
  IDEA_non_missing.ADSL("DTHDT", NULL, tg_data)
  IDEA_non_missing.ADSL("ITTFL", NULL, tg_data)
  IDEA_non_missing.ADSL("SEX", NULL, tg_data)
})

test_that("non missing block ADSL group", {
  IDEA_non_missing.ADSL("ITTFL", "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("non missing block on BDS fails", {
  expect_error(IDEA_non_missing.BDS("AGE", NULL, tg_data))
})

test_that("non missing MUST have differnt var and grouping variables", {
  expect_error(IDEA_non_missing.ADSL("SEX", "SEX", data = tg_data))
})

