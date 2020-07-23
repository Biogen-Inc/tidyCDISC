context("table generator freq block")

test_that("freq block ADSL", {
  CDISC_freq.ADSL("SEX", NULL, NULL, tg_data)
})

test_that("freq block ADSL group", {
  CDISC_freq.ADSL("SEX", NULL, "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("freq block numeric fails", {
  expect_error(CDISC_freq.ADSL("AGE", NULL, NULL, tg_data))
})

test_that("freq block BDS exits", {
  expect_error(CDISC_freq.BDS("SEX", tg_data))
})

test_that("freq block OCCDS exits", {
  expect_error(CDISC_freq.OCCDS("SEX", tg_data))
})

test_that("freq block default exits", {
  expect_error(CDISC_freq.default("SEX", tg_data))
})
