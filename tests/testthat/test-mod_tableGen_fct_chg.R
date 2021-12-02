context("table generator chg block")

test_that("chg block BDS", {
  app_chg.BDS("DIABP", "Week 2", NULL, tg_data)
})

test_that("chg block BDS group", {
  app_chg.BDS("DIABP", "Week 2", "SEX", tg_data)
})


# -------------------------------------------
# Expected Test Failures
# -------------------------------------------


test_that("chg block without week fails", {
  expect_error(app_chg.BDS("DIABP", "NONE", "SEX", tg_data))
})

test_that("chg block ADSL fails", {
  expect_error(app_chg.ADSL("AGE", NULL, NULL, tg_data))
})

test_that("chg block OCCDS exits", {
  expect_error(app_chg.OCCDS("AGE", NULL, NULL, tg_data))
})

test_that("chg block default exits", {
  expect_error(app_chg.default("AGE", NULL, NULL, tg_data))
})
