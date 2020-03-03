library(testthat)
library(rvest)

source("tests/data/test_data.R")

context("Frequency Block tests")


# 14
test_that("Frequency of ADSL", {
  
  ROW <- sym("SEX")
  
  tg_freq <- test_data %>% 
    distinct(USUBJID, !!ROW) %>%
    group_by(!!ROW) %>%
    count(!!ROW) %>%
    summarise(Frequency = as.numeric(sum(n))) %>%
    ungroup() %>%
    mutate(Percent = round(as.numeric(Frequency/sum(Frequency)*100), 4))
  
  sas_freq <- read_sas("tests/data/test_outputs/test14.sas7bdat") %>%
    select(SEX, Frequency, Percent) %>%
    mutate(Percent = round(Percent, 4))
    
    # ensure it matches the shiny output
    expect_equal(tg_freq, sas_freq)
})

# 15
test_that("Frequency of ADSL filtered", {
  
  ROW <- sym("SEX")
  
  tg_freq <- test_data_filtered %>% 
    distinct(USUBJID, !!ROW) %>%
    count(!!ROW) %>%
    group_by(!!ROW) %>%
    summarise(Frequency = as.numeric(sum(n))) %>%
    ungroup() %>%
    mutate(Percent = as.numeric(Frequency/sum(Frequency)*100))
  
  sas_freq <- read_sas("tests/data/test_outputs/test15.sas7bdat") %>%
    select(SEX, Frequency, Percent)
  
  # ensure it matches the shiny output
  expect_equal(tg_freq, sas_freq)
})

# 16
test_that("Frequency of ADSL grouped", {
  
  ROW <- sym("SAFFL")
  COLUMN <- sym("TRT01P")
  
  tg_freq <- test_data %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    count(!!ROW, !!COLUMN) %>%
    group_by(!!ROW) %>%
    mutate(RowPercent = prop.table(n)) %>%
    rename(Frequency = n) %>%
    mutate(Frequency = as.numeric(Frequency)) %>%
    mutate(RowPercent = round(RowPercent*100, 4))
  
  sas_freq <- read_sas("tests/data/test_outputs/test16.sas7bdat") %>%
    select(SEX, TRT01P, Frequency, RowPercent) %>%
    mutate(RowPercent = round(RowPercent, 4))
  
  # ensure it matches the shiny output
  expect_equal(tg_freq, sas_freq)
})

# 17
test_that("Frequency of ADSL grouped and filtered", {
  
  ROW <- sym("SEX")
  COLUMN <- sym("TRT01P")
  
  tg_freq <- test_data_filtered %>%
    distinct(USUBJID, !!ROW, !!COLUMN) %>%
    count(!!ROW, !!COLUMN) %>%
    group_by(!!ROW) %>%
    mutate(RowPercent = prop.table(n)) %>%
    # make output look like sas, not in table gen pipeline
    rename(Frequency = n) %>%
    mutate(Frequency = as.numeric(Frequency)) %>%
    mutate(RowPercent = round(RowPercent*100, 4))
  
  sas_freq <- read_sas("tests/data/test_outputs/test17.sas7bdat") %>%
    select(SEX, TRT01P, Frequency, RowPercent) %>%
    # R doesnt include the 0 value
    filter(RowPercent != 0) %>%
    mutate(RowPercent = round(RowPercent, 4))
  
  # ensure it matches the shiny output
  expect_equal(tg_freq, sas_freq)
})
