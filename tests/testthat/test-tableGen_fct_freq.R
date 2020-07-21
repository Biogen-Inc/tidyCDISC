context("table generator freq block")

test_that("freq block ADSL", {
  IDEA_freq.ADSL("SEX", NULL, NULL, tg_data)
})

test_that("freq block ADSL group", {
  IDEA_freq.ADSL("SEX", NULL, "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("freq block numeric fails", {
  expect_error(IDEA_freq.ADSL("AGE", NULL, NULL, tg_data))
})

test_that("freq block BDS exits", {
  expect_error(IDEA_freq.BDS("SEX", tg_data))
})

test_that("freq block OCCDS exits", {
  expect_error(IDEA_freq.OCCDS("SEX", tg_data))
})

test_that("freq block default exits", {
  expect_error(IDEA_freq.default("SEX", tg_data))
})
