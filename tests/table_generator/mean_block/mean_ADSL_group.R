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
  ROW <- sym("DIABP")
  AGGREGATE <- sym("MEAN")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")

  
  block_df <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
              Median = median(AVAL),
              `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
              `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()