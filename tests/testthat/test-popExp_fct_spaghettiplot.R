context("Create popExp Spaghetti plot")

test_that("numeric response variable works", {
  plot <- IDEA_spaghettiplot(tg_data, "AGE", "AVISIT")
  expect_equal(rlang::quo_get_expr(plot$mapping$x), rlang::sym("AVISIT"))
  expect_equal(rlang::quo_get_expr(plot$mapping$y), rlang::sym("AGE"))
})

test_that("PARAMCD response variable works", {
  plot <- IDEA_spaghettiplot(tg_data, "DIABP", "AVISIT", value = "AVAL")
  expect_equal(rlang::quo_get_expr(plot$mapping$x), rlang::sym("AVISIT"))
  expect_equal(rlang::quo_get_expr(plot$mapping$y), rlang::sym("AVAL"))
})

test_that("adding jitter works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX", points = TRUE)
  expect_equal("PositionJitter", class(plot$layers[[2]]$position)[1])
})

