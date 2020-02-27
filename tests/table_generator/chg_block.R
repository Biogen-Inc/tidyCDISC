library(testthat)
library(rvest)

source("tests/data/test_data.R")

context("Change from Baseline tests")


# 22
test_that("Change from Baseline", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  
  tg_chg <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = as.numeric(n()),
              CHG_Mean = mean(CHG),
              CHG_StdDev = round(sd(CHG), 4),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG),
              CHG_Q1 = quantile(CHG, 0.25, type = 2),
              CHG_Q3 = quantile(CHG, 0.75, type = 2))
  
  sas_chg <- read_sas("tests/data/test_outputs/test22.sas7bdat") %>%
    mutate(CHG_StdDev = round(CHG_StdDev, 4))
    
  expect_equal(sas_chg, tg_chg)
})


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
              CHG_Max = max(CHG),
              CHG_Q1 = quantile(CHG, 0.25, type = 2),
              CHG_Q3 = quantile(CHG, 0.75, type = 2))
  
    sas_chg <- read_sas("tests/data/test_outputs/test23.sas7bdat")
  
  expect_equal(sas_chg, tg_chg)
})

# 24
test_that("Change from Baseline grouped", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  tg_chg <- test_data %>%
    group_by(!!COLUMN) %>% 
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = as.numeric(n()),
              CHG_Mean = mean(CHG),
              CHG_StdDev = round(sd(CHG), 4),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG),
              CHG_Q1 = quantile(CHG, 0.25, type = 2),
              CHG_Q3 = quantile(CHG, 0.75, type = 2))
  
  sas_chg <- read_sas("tests/data/test_outputs/test24.sas7bdat") %>%
    select(-NObs) %>%
    mutate(CHG_StdDev = round(CHG_StdDev, 4))
  
  expect_equal(tg_chg, sas_chg)
})


# 25
test_that("Change from Baseline grouped and filtered", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  tg_chg <- test_data_filtered %>%
    group_by(!!COLUMN) %>% 
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(CHG_N = as.numeric(n()),
              CHG_Mean = mean(CHG),
              CHG_StdDev = round(sd(CHG), 4),
              CHG_Min = min(CHG),
              CHG_Max = max(CHG),
              CHG_Q1 = quantile(CHG, 0.25, type = 2),
              CHG_Q3 = quantile(CHG, 0.75, type = 2))
  
  sas_chg <- read_sas("tests/data/test_outputs/test25.sas7bdat") %>%
    select(-NObs) %>%
    mutate(CHG_StdDev = round(CHG_StdDev, 4))
  
  expect_equal(tg_chg, sas_chg)
})
