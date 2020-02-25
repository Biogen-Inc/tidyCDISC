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
  WEEK <- "Week 12"
  
  intermediate <- all_data() %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(mean = round(mean(CHG), 3))
  # use the mean in wide format for table
  # add column name showing the total N
  d <- data.frame(CHG = intermediate$mean)
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()
