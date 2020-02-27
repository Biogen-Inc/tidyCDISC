library(shinytest)
library(testthat)
library(rvest)

source("tests/data/test_data.R")

context("Change from Baseline tests")

app <- ShinyDriver$new(".")

# 22
test_that("Change from Baseline", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  
  tg_chg <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = n(),
              CHG_mean = mean(CHG),
              CHG_StdDev = sd(CHG),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG))
  
  sas_chg <- read_sas("tests/data/test_outputs/test22.sas7bdat")
    
  expect_identical(saS_chg, tg_chg)
})

app$stop()

# 23
test_that("Change from Baseline  with filter", {
  
  tg_chg <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = n(),
              CHG_mean = mean(CHG),
              CHG_StdDev = sd(CHG),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG))
  
  sas_chg <- read_sas("tests/data/test_outputs/test23.sas7bdat")
  
  expect_identical(saS_chg, tg_chg)
})

app$stop()

# 24
test_that("Change from Baseline grouped", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  intermediate <- test_data %>%
    group_by(!!COLUMN) %>% 
    filter(AVISIT == WEEK & PARAMCD == ROW) %>%
    summarise(mean = round(mean(CHG), 3))
  d <- data.frame(CHG = intermediate$mean)
  
  test_mean <- read_sas("tests/data/test_outputs/test18.sas7bdat")
  
  expect_identical(test_mean, block_df)
})

app$stop()

# 25
test_that("Change from Baseline grouped and filtered", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  intermediate <- test_data %>%
    group_by(!!COLUMN) %>% 
    filter(AVISIT == WEEK & PARAMCD == ROW) %>%
    summarise(mean = round(mean(CHG), 3))
  d <- data.frame(CHG = intermediate$mean)
  
  test_mean <- read_sas("tests/data/test_outputs/test18.sas7bdat")
  
  expect_identical(test_mean, block_df)
})

app$stop()