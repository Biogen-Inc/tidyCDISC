context("Create popExp Spaghetti plot")

test_that("numeric response variable works", {
  plot <- IDEA_spaghettiplot(tg_data, "AGE", "AVISIT")
  expect_equal(plot$mapping$x[[2]], sym("AVISIT"))
  expect_equal(plot$mapping$y[[2]], sym("AGE"))
})

test_that("PARAMCD response variable works", {
  plot <- IDEA_spaghettiplot(tg_data, "DIABP", "AVISIT", value = "AVAL")
  expect_equal(plot$mapping$x[[2]], sym("AVISIT"))
  expect_equal(plot$mapping$y[[2]], sym("AVAL"))
})

test_that("adding jitter works", {
  plot <- IDEA_boxplot(tg_data, "AGE", "SEX", points = TRUE)
  expect_equal("PositionJitter", class(plot$layers[[2]]$position)[1])
})
