library(testthat)
library(rvest)

source("test_data")

context("Mean Block Functions")

# 6
test_that("Mean PARAMCD", {
  
  ROW <- sym("DIABP")
  WEEK <- sym("Week 12")
  
  tg_mean <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    summarise(AVAL_N = as.numeric(n()),
              AVAL_Mean = mean(AVAL),
              AVAL_StdDev = sd(AVAL),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL))
  
  test_mean <- read_sas("tests/data/test_outputs/test6.sas7bdat")
    
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
              AVAL_StdDev = sd(AVAL),
              AVAL_Min = min(AVAL),
              AVAL_Max = max(AVAL))
  
  sas_mean <- read_sas("tests/data/test_outputs/test7.sas7bdat")
    
    # ensure it matches the shiny output
    expect_equal(sas_mean, tg_mean)
})

# 8
test_that("mean block ADSL", {
  
  ROW <- sym("AGE")
  COLUMN <- ""
  
  tg_mean <- test_data %>%
    distinct(USUBJID, AGE, !!COLUMN) %>%
    na.omit() %>%
    summarise(AGE_N = as.numeric(n()),
              AGE_Mean = mean(!!ROW),
              AGE_StdDev = sd(!!ROW),
              AGE_Min = min(!!ROW),
              AGE_Max = max(!!ROW))
  
  sas_mean <- read_sas("tests/data/test_outputs/test8.sas7bdat")
  
  # ensure it matches the shiny output
  expect_equal(sas_mean, tg_mean)
})

# 9
test_that("mean block ADSL filtered", {
  
  ROW <- sym("AGE")
  AGGREGATE <- sym("MEAN")
  
  tg_mean <- test_data_filtered %>%
    filter(!is.na(!!ROW)) %>%
    group_by(!!COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(!!ROW), 2), " (", round(sd(!!ROW), 2), ")"),
              Median = median(!!ROW),
              `Q1 | Q3` = paste(round(quantile(!!ROW, 0.25),2) , "|", (round(quantile(!!ROW, 0.75),2))),
              `Min | Max` = paste0(round(min(!!ROW), 2), " | ", round(max(!!ROW), 2)))
  
  sas_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(sas_mean, tg_mean)
})


# 10
test_that("Mean PARAMCD grouped", {
  
  ROW <- sym("DIABP")
  AGGREGATE <- sym("MEAN")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
              Median = median(AVAL),
              `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
              `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
  
  sas_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(sas_mean, tg_mean)
})

# 11
test_that("Mean PARAMCD grouped", {
  
  ROW <- sym("DIABP")
  AGGREGATE <- sym("MEAN")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
              Median = median(AVAL),
              `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
              `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
  
  sas_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(sas_mean, tg_mean)
})

# 12
test_that("mean ADSL grouped", {
  
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

# 13
test_that("mean ADSL grouped and filtered", {
  
  ROW <- sym("DIABP")
  AGGREGATE <- sym("MEAN")
  WEEK <- sym("Week 12")
  COLUMN <- sym("TRT01P")
  
  tg_mean <- test_data_filtered %>%
    filter(PARAMCD == ROW & AVISIT == WEEK) %>%
    group_by(COLUMN) %>%
    summarise(N = n(),
              `Mean (SD)` = paste0(round(mean(AVAL), 2), " (", round(sd(AVAL), 2), ")"),
              Median = median(AVAL),
              `Q1 | Q3` = paste(round(quantile(AVAL, 0.25), 2) , "|", round(quantile(AVAL, 0.75), 2)),
              `Min | Max` = paste(round(min(AVAL), 2), " | ", round(max(AVAL), 2)))
  
  sas_mean <- 
    
    # ensure it matches the shiny output
    expect_identical(sas_mean, tg_mean)
})