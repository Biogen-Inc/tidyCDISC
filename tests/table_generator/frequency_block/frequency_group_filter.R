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
  COLUMN <- sym("TRT01P")
  
  test_data_filtered <- test_data %>%
    filter(X == "X")
  
  block_df <- test_data_filtered %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    count(!!ROW, !!COLUMN) %>%
    group_by(!!ROW) %>%
    mutate(prop = prop.table(n)) %>%
    mutate(v1 = paste0(n, ' (', round(prop, 2), ')')) %>%
    select(-n, -prop) %>% 
    spread(!!COLUMN, v1)
  
  test_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(test_mean, block_df)
})

app$stop()
