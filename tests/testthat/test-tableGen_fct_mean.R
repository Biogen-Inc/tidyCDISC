context("table generator mean block")

test_that("mean block ADSL", {
  CDISC_mean.ADSL("AGE", NULL, NULL, tg_data)
})

test_that("mean block ADSL group", {
  CDISC_mean.ADSL("AGE", NULL, "SEX", tg_data)
})

test_that("mean block BDS", {
  CDISC_mean.BDS("DIABP", "Week 2", NULL, tg_data)
})

test_that("mean block BDS group", {
  CDISC_mean.BDS("DIABP", "Week 2", "SEX", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("mean block categorical fails", {
  expect_error(CDISC_mean.BDS("SEX", "Week 2", "SEX", tg_data))
})

test_that("mean block OCCDS exits", {
  expect_error(CDISC_mean.OCCDS("DIABP", "Week 2", "SEX", tg_data))
})

test_that("mean block default exits", {
  expect_error(CDISC_mean.default("DIABP", "Week 2", "SEX", tg_data))
})
