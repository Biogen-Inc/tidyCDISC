library(shinytest)
library(testthat)
library(rvest)
library(broom)

source("tests/data/test_data.R")

context("T-Test Block tests")


# 18
test_that("t-test of PARAMCD by group", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  all_dat <- test_data %>%  filter(PARAMCD == "DIABP" & AVISIT == "Week 12")
  ttest <- broom::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat)) %>%
    pull(p.value[1])
  
  sas_ttest <- read_sas("tests/data/test_outputs/test18.sas7bdat") %>%
    pull(ProbF)
    
    # ensure it matches the shiny output
    expect_equal(ttest[1], sas_ttest[1])
})

# 19
test_that("t-test of PARAMCD by group filtered", {
  
  ROW <- sym("DIABP")
  WEEK <- "Week 12"
  COLUMN <- sym("TRT01P")
  
  all_dat <- test_data_filtered %>%  filter(PARAMCD == ROW & AVISIT == WEEK)
  ttest <- broom::tidy(aov(all_dat$AVAL ~ all_dat[[paste(COLUMN)]], data=all_dat)) %>%
    pull(p.value[1])
  
  sas_ttest <- read_sas("tests/data/test_outputs/test19.sas7bdat") %>%
    pull(ProbF)
  
  # ensure it matches the shiny output
  expect_equal(ttest[1], sas_ttest[1])
})

# 20
test_that("t-test of ADSL by group", {
  
  ROW <- sym("AGE")
  COLUMN <- sym("TRT01P")
  WEEK <- "Week 12"
  
  all_dat <- test_data %>% distinct(!!ROW, !!COLUMN, USUBJID)
  ttest <- broom::tidy(aov(all_dat[[paste(ROW)]] ~ all_dat[[paste(COLUMN)]], data=all_dat)) %>%
    pull(p.value[1])
  
  sas_ttest <- read_sas("tests/data/test_outputs/test20.sas7bdat") %>%
    pull(ProbF)
  
  # ensure it matches the shiny output
  expect_equal(ttest[1], sas_ttest[1])
})

# 21
test_that("t-test of ADSL by group filtered", {
  
  ROW <- sym("AGE")
  COLUMN <- sym("TRT01P")
  WEEK <- "Week 12"
  
  all_dat <- test_data %>% distinct(!!ROW, !!COLUMN, USUBJID)
  ttest <- broom::tidy(aov(all_dat[[paste(ROW)]] ~ all_dat[[paste(COLUMN)]], data=all_dat)) %>%
    pull(p.value[1])
  
  sas_ttest <- read_sas("tests/data/test_outputs/test20.sas7bdat") %>%
    pull(ProbF)
  
  # ensure it matches the shiny output
  expect_equal(ttest[1], sas_ttest[1])
  
})
