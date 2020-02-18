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
  ROW <- sym("AGE")
  AGGREGATE <- sym("MEAN")
  
  test_data_filtered <- test_data %>%
    filter(X == "X")
  
  block_df <-   test_data_filtered %>%
    filter(!is.na(!!ROW)) %>%
    group_by(!!COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(!!ROW), 2), " (", round(sd(!!ROW), 2), ")"),
              Median = median(!!ROW),
              `Q1 | Q3` = paste(round(quantile(!!ROW, 0.25),2) , "|", (round(quantile(!!ROW, 0.75),2))),
              `Min | Max` = paste0(round(min(!!ROW), 2), " | ", round(max(!!ROW), 2)))
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()