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
  
  block_df <- test_data %>% 
    distinct(USUBJID, !!ROW) %>%
    count(!!ROW) %>%
    group_by(!!ROW) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(prop = n/sum(n)) %>%
    mutate(x = paste0(n, " (", round(prop, 2), ")")) %>%
    select(!!ROW, x)
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()
