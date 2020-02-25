library(shinytest)
library(testthat)
library(rvest)

source("test_data")

context("Test")

app <- ShinyDriver$new(".")

test_that("Sepal Length filtered by num_output", {
  
  # set num_input to 30
  # app$setInputs(agg_drop_zone = "<table><tr><td>\n  PCTDRUG\n  </td></tr></table>")
  # app$setInputs(block_drop_zone = "<table><tr><td>MEAN: Year 1</td></tr></table>")
  
  # set num_input to 30
  ROW <- sym("SEX")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  test_data_filtered <- test_data %>%
    filter(X == "X")
  
  all_dat <- test_data_filtered %>%  filter(PARAMCD == ROW & AVISIT == WEEK)
  ttest <- janitor::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat))
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()