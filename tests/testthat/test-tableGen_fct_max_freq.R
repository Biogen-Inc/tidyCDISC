context("table generator max FREQ stat block")

test_that("max FREQ block on ADSL", {
  app_max_freq.ADSL("AVISITf2", NULL, tg_data)
})


# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("max FREQ MUST have differnt var and grouping variables", {
  expect_error(app_max_freq.ADSL("SEX", "SEX", data = tg_data))
})
test_that("max FREQ block on BDS fails", {
  expect_error(app_max_freq.BDS("AGE", NULL, tg_data))
})
test_that("max FREQ block on character variable fails", {
  expect_error(app_max_freq.ADSL("AVISIT", NULL, tg_data))
})
test_that("max FREQ block on factor fails if missing VARN in data", {
  expect_error(app_max_freq.ADSL("AVISITf1", NULL, tg_data))
})
test_that("max FREQ block will be ordered by factor levels in data", {
  expect_equal(
    as.character(app_max_freq.ADSL("AVISITf2", NULL, tg_data)$AVISITf2[1]),
    "Week 2"
    )
})



