context("Create popExp Boxplot")

data, yvar, group, value, points = FALSE

test_that("numeric response variable works", {
  IDEA_boxplot(tg_data, "AGE", "SEX")
})

test_that("PARAMCD response variable works", {
  
})

test_that("adding jitter works", {
  
})