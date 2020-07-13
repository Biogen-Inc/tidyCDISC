context("Create popExp Boxplot")

test_that("numeric response variable works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX")
  expect_equal(quo_get_expr(plot$mapping$x), sym("SEX"))
  expect_equal(quo_get_expr(plot$mapping$y), sym("AGE"))
})

test_that("PARAMCD response variable works", {
  plot <- IDEA_boxplot(tg_data, "DIABP", "SEX", value = "AVAL")
  expect_equal(quo_get_expr(plot$mapping$x), sym("SEX"))
  expect_equal(quo_get_expr(plot$mapping$y), sym("AVAL"))
})

test_that("adding jitter works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX", points = TRUE)
  expect_equal("PositionJitter", class(plot$layers[[2]]$position)[1])
})
