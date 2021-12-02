context("table generator freq block")

test_that("freq block ADSL", {
  app_freq.ADSL("SEX", NULL, tg_data)
})

test_that("freq block ADSL group", {
  app_freq.ADSL("SEX", "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("freq block numeric fails", {
  expect_error(app_freq.ADSL("AGE", NULL, tg_data))
})

test_that("freq block BDS exits", {
  expect_error(app_freq.BDS("SEX", tg_data))
})

test_that("freq block OCCDS exits", {
  expect_error(app_freq.OCCDS("SEX", tg_data))
})

test_that("freq block default exits", {
  expect_error(app_freq.default("SEX", tg_data))
})
