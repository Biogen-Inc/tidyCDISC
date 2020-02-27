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
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  
  tg_chg <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = as.numeric(n()),
              CHG_Mean = mean(CHG),
              CHG_StdDev = sd(CHG),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG))
  
    sas_chg <- read_sas("tests/data/test_outputs/test23.sas7bdat")
  
  expect_equal(sas_chg, tg_chg)
})

app$stop()

# 24
test_that("Change from Baseline grouped", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  tg_chg <- test_data %>%
    group_by(!!COLUMN) %>% 
    filter(AVISIT == WEEK & PARAMCD == ROW) %>%
    summarise(CHG_N = n(),
              CHG_mean = mean(CHG),
              CHG_StdDev = sd(CHG),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG))
  
  sas_chg <- read_sas("tests/data/test_outputs/test24.sas7bdat") %>% 
    select(-NObs)
  
  expect_identical(tg_chg, sas_chg)
})

app$stop()

# 25
test_that("Change from Baseline grouped and filtered", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  tg_chg <- test_data_filtered %>%
    group_by(!!COLUMN) %>% 
    filter(AVISIT == WEEK & PARAMCD == ROW) %>%
    summarise(CHG_N = as.numeric(n()),
              CHG_Mean = mean(CHG),
              CHG_StdDev = sd(CHG),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG))
  
  sas_chg <- read_sas("tests/data/test_outputs/test25.sas7bdat") %>%
    select(-NObs)
  
  expect_equal(tg_chg, sas_chg)
})