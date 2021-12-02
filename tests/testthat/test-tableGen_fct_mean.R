context("table generator mean block")

test_that("mean block ADSL", {
  app_mean.ADSL("AGE", NULL, NULL, tg_data)
})

test_that("mean block ADSL group", {
  app_mean.ADSL("AGE", NULL, "SEX", tg_data)
})

test_that("mean block BDS", {
  app_mean.BDS("DIABP", "Week 2", NULL, tg_data)
})

test_that("mean block BDS group", {
  app_mean.BDS("DIABP", "Week 2", "SEX", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("mean block categorical fails", {
  expect_error(app_mean.BDS("SEX", "Week 2", "SEX", tg_data))
})

test_that("mean block OCCDS exits", {
  expect_error(app_mean.OCCDS("DIABP", "Week 2", "SEX", tg_data))
})

test_that("mean block default exits", {
  expect_error(app_mean.default("DIABP", "Week 2", "SEX", tg_data))
})
