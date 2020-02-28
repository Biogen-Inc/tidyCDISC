library(testthat)
library(rvest)

source("tests/data/test_data.R")

context("Mean Block Functions")

# 6
test_that("Mean PARAMCD", {
  
  ROW <- sym("DIABP")
  WEEK <- sym("Week 12")
  
  tg_mean <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(AVAL_N = as.numeric(n()),
              AVAL_Mean = mean(AVAL),
              AVAL_StdDev = round(sd(AVAL), 4),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL),
              AVAL_Q1 = quantile(AVAL, 0.25, type = 2),
              AVAL_Q3 = quantile(AVAL, 0.75, type = 2))
  
  # we need to round the stdev to make the dataframes equal
  sas_mean <- read_sas("tests/data/test_outputs/test6.sas7bdat") %>%
    mutate(AVAL_StdDev = round(AVAL_StdDev, 4))
    
    # ensure it matches the shiny output
  expect_equal(sas_mean, tg_mean)
})

# 7
test_that("Mean PARAMCD filtered", {
  
  ROW <- sym("DIABP")
  WEEK <- sym("Week 12")
  
  tg_mean <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(AVAL_N = as.numeric(n()),
              AVAL_Mean = mean(AVAL),
              AVAL_StdDev = round(sd(AVAL), 4),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL),
              AVAL_Q1 = quantile(AVAL, 0.25, type = 2),
              AVAL_Q3 = quantile(AVAL, 0.75, type = 2))
  
  sas_mean <- read_sas("tests/data/test_outputs/test7.sas7bdat") %>%
    mutate(AVAL_StdDev = round(AVAL_StdDev, 4))
    
    # ensure it matches the shiny output
    expect_equal(sas_mean, tg_mean)
})

# 8
test_that("mean block ADSL", {
  
  ROW <- sym("AGE")
  COLUMN <- ""
  
  tg_mean <- test_data %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    filter(!is.na(!!ROW)) %>%
    summarise(AGE_N = as.numeric(n()),
              AGE_Mean = mean(!!ROW),
              AGE_StdDev = round(sd(!!ROW), 4),
              AGE_Min = min(!!ROW),
              AGE_Max = max(!!ROW),
              AGE_Q1 = quantile(!!ROW, 0.25, type = 2),
              AGE_Q3 = quantile(!!ROW, 0.75, type = 2))
  
  sas_mean <- read_sas("tests/data/test_outputs/test8.sas7bdat") %>%
    mutate(AGE_StdDev = round(AGE_StdDev, 4))
  
  # ensure it matches the shiny output
  expect_equal(sas_mean, tg_mean, check.attributes = FALSE)
 
})

# 9
test_that("mean block ADSL filtered", {
  
  ROW <- sym("AGE")
  AGGREGATE <- sym("MEAN")
  COLUMN <- ""
  
  tg_mean <- test_data_filtered %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    filter(!is.na(!!ROW)) %>%
    summarise(AGE_N = as.numeric(n()),
              AGE_Mean = mean(!!ROW),
              AGE_StdDev = round(sd(!!ROW), 4),
              AGE_Min = min(!!ROW),
              AGE_Max = max(!!ROW),
              AGE_Q1 = quantile(!!ROW, 0.25, type = 2),
              AGE_Q3 = quantile(!!ROW, 0.75, type = 2))
  
  sas_mean <-  read_sas(zap_label("tests/data/test_outputs/test9.sas7bdat")) %>%
    mutate(AGE_StdDev = round(AGE_StdDev, 4))
    
    # ensure it matches the shiny output
    expect_equal(sas_mean, tg_mean)
})


# 10
test_that("Mean PARAMCD grouped", {
  
  ROW <- sym("DIABP")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(!!COLUMN) %>%
    summarise(AVAL_N = as.numeric(n()),
              AVAL_Mean = mean(AVAL),
              AVAL_StdDev = round(sd(AVAL), 4),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL),
              AVAL_Q1 = quantile(AVAL, 0.25, type = 2),
              AVAL_Q3 = quantile(AVAL, 0.75, type = 2))
  
  sas_mean <- read_sas(zap_label("tests/data/test_outputs/test10.sas7bdat")) %>%
    select(-NObs) %>%
    mutate(AVAL_StdDev = round(AVAL_StdDev, 4))
    
    # ensure it matches the shiny output
    expect_equal(sas_mean, tg_mean)
})

# 11
test_that("Mean PARAMCD grouped", {
  
  ROW <- sym("DIABP")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(!!COLUMN) %>%
    summarise(AVAL_N = as.numeric(n()),
              AVAL_Mean = mean(AVAL),
              AVAL_StdDev = round(sd(AVAL), 4),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL),
              AVAL_Q1 = quantile(AVAL, 0.25, type = 2),
              AVAL_Q3 = quantile(AVAL, 0.75, type = 2))
  
  sas_mean <- read_sas(zap_label("tests/data/test_outputs/test11.sas7bdat")) %>%
    select(-NObs) %>%
    mutate(AVAL_StdDev = round(AVAL_StdDev, 4))
    
    # ensure it matches the shiny output
    expect_equal(sas_mean, tg_mean)
})

# 12
test_that("mean ADSL grouped", {
  
  ROW <- sym("AGE")
  COLUMN <- sym("TRT01P")
  
  
  tg_mean <- test_data %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    filter(!is.na(!!ROW)) %>%
    group_by(!!COLUMN) %>%
    summarise(AGE_N = as.numeric(n()),
              AGE_Mean = mean(!!ROW),
              AGE_StdDev = round(sd(!!ROW), 4),
              AGE_Min = min(!!ROW),
              AGE_Max = max(!!ROW),
              AGE_Q1 = quantile(!!ROW, 0.25, type = 2),
              AGE_Q3 = quantile(!!ROW, 0.75, type = 2))
  
  sas_mean <- read_sas(zap_label("tests/data/test_outputs/test12.sas7bdat")) %>%
    select(-NObs) %>%
    mutate(AGE_StdDev = round(AGE_StdDev, 4))
    
  
    # ensure it matches the shiny output
    expect_equal(tg_mean, sas_mean)
})

# 13
test_that("mean ADSL grouped and filtered", {
  
  ROW <- sym("AGE")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data_filtered %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    filter(!is.na(!!ROW)) %>%
    group_by(!!COLUMN) %>%
    summarise(AGE_N = as.numeric(n()),
              AGE_Mean = mean(!!ROW),
              AGE_StdDev = round(sd(!!ROW), 4),
              AGE_Min = min(!!ROW),
              AGE_Max = max(!!ROW),
              AGE_Q1 = quantile(!!ROW, 0.25, type = 2),
              AGE_Q3 = quantile(!!ROW, 0.75, type = 2))
  
  sas_mean <- read_sas(zap_label("tests/data/test_outputs/test13.sas7bdat")) %>%
    select(-NObs) %>%
    mutate(AGE_StdDev = round(AGE_StdDev, 4))
  
  # ensure it matches the shiny output
  expect_equal(tg_mean, sas_mean)
})
