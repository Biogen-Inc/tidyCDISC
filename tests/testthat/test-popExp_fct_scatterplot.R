context("Create popExp Scatter plot")


test_that("numeric x-axis and numeric y-axis", {
  plot <- IDEA_scatterplot(tg_data, "AGE", "CHG")
  expect_equal(quo_get_expr(plot$mapping$y), sym("AGE"))
  expect_equal(quo_get_expr(plot$mapping$x), sym("CHG"))
})

test_that("PARAMCD x-axis and numeric y-axis", {
  plot <- IDEA_scatterplot(tg_data, yvar = "AGE", xvar = "DIABP", week_x = "Week 2", value_x = "AVAL")
  expect_equal(quo_get_expr(plot$mapping$x), sym("AVAL"))
  expect_equal(quo_get_expr(plot$mapping$y), sym("AGE"))
})

test_that("numeric y-axis and PARAMCD y-axis", {
  plot <- IDEA_scatterplot(tg_data, xvar = "AGE", yvar = "DIABP", week_y = "Week 2", value_y = "AVAL")
  expect_equal(quo_get_expr(plot$mapping$y), sym("AVAL"))
  expect_equal(quo_get_expr(plot$mapping$x), sym("AGE"))
})

test_that("PARAMCD x-axis and PARAMCD y-axis", {
  plot <- IDEA_scatterplot(tg_data, 
                           yvar = "DIABP", week_y = "Week 2", value_y = "AVAL",
                           xvar = "DIABP", week_x = "Week 2", value_x = "AVAL")
  expect_equal(quo_get_expr(plot$mapping$y), sym("AVAL"))
  expect_equal(quo_get_expr(plot$mapping$x), sym("AVAL"))
})

test_that("facet by group", {
  plot <- IDEA_scatterplot(tg_data, "AGE", "CHG", separate = "SEX")
  expect_equal("FacetWrap", class(plot$facet)[1])
})

test_that("facet by group", {
  plot <- IDEA_scatterplot(tg_data, "AGE", "CHG", color = "SEX")
  expect_equal(sym("SEX"), quo_get_expr(plot$mapping$colour))
})
