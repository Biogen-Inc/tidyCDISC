context("table generator y freq block")

test_that("Y freq block ADSL", {
  IDEA_y.ADSL("ITTFL", NULL, tg_data)
  IDEA_y.ADAE("ITTFL", NULL, tg_data)
  IDEA_y.ADSL("SAFFL", NULL, tg_data)
  IDEA_y.ADAE("SAFFL", NULL, tg_data)
})

test_that("Y freq block ADSL group", {
  IDEA_y.ADSL("ITTFL", "COUNTRY", tg_data)
})

# -------------------------------------------
# Expected Test Failures
# -------------------------------------------

test_that("Y freq block numeric fails", {
  expect_error(IDEA_y.ADSL("AGE", NULL, tg_data))
})

# test_that("Y freq must be applied to flag variable", {
#   expect_error(IDEA_y.ADSL("SEX", data = tg_data))
# })

