context("Create popExp Boxplot")

test_that("numeric response variable works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX")
  expect_equal(plot$mapping$x[[2]], sym("SEX"))
  expect_equal(plot$mapping$y[[2]], sym("AGE"))
})

test_that("PARAMCD response variable works", {
  plot <- IDEA_boxplot(tg_data, "DIABP", "SEX", value = "AVAL")
  expect_equal(plot$mapping$x[[2]], sym("SEX"))
  expect_equal(plot$mapping$y[[2]], sym("AVAL"))
})

test_that("adding jitter works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX", points = TRUE)
  expect_equal("PositionJitter", class(plot$layers[[2]]$position)[1])
})
