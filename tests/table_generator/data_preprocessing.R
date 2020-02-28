library(testthat)
library(rvest)

source("tests/data/test_data.R")

context("Data preprocessing")

# 1
test_that("ADSL and BDS combined properly", {
  
  sas_data <- 
    
    # ensure it matches the shiny output
    expect_identical(test_data, tg)
})


# 2
test_that("Total in header generated correctly", {
  
  # total in TG module
  tg_total <- test_data %>%
      distinct(USUBJID) %>%
      summarise(n = n()) %>%
    pull(n)
  
    sas_total <- read_sas("tests/data/test_outputs/test_2_unique_usub.sas7bdat") %>%
    mutate_all(`attributes<-`, NULL) %>% 
    pull(NLevels)
  
    # ensure it matches the shiny output
    expect_identical(sas_total, as.numeric(tg_total))
})

# 3
test_that("Total in header generated correctly when filtered", {
  
  COLUMN <- sym("COUNTRY")
  
  # header_df in TG module
  tg_total_filtered <- test_data_filtered %>%
      distinct(USUBJID, !!COLUMN) %>%
      group_by(!!COLUMN) %>%
      summarise(count = n()) %>%
    pull(count)
  
  sas_total_filtered <- read_sas("tests/data/test_outputs/test_3_unique_usub_can.sas7bdat") %>%
    mutate_all(`attributes<-`, NULL) %>% 
    pull(NLevels)
    
    # ensure it matches the shiny output
    expect_identical(sas_total_filtered, as.numeric(tg_total_filtered))
})

# 4
test_that("Total in header generated correctly when grouped", {
  
  COLUMN <- sym("TRT01P")
  
  tg_total_grouped <- test_data %>%
    distinct(USUBJID, !!COLUMN) %>%
    group_by(!!COLUMN) %>%
    # changed name to match sas output
    summarise(NLevels = n()) %>%
    mutate(NLevels = as.numeric(NLevels))
  
    sas_total_grouped <- read_sas("tests/data/test_outputs/test_4_unique_usub_trt.sas7bdat") %>%
    mutate_all(`attributes<-`, NULL) %>% 
    select(TRT01P, NLevels)
    
    # ensure it matches the shiny output
    expect_equal(sas_total_grouped, tg_total_grouped)
})

# 5
test_that("Total in header generated correctly when grouped and filtered", {
  
  COLUMN <- sym("TRT01P")
  
  tg_total_grouped_filtered <- test_data_filtered %>%
    distinct(USUBJID, !!COLUMN) %>%
    group_by(!!COLUMN) %>%
    # changed name to match sas output
    summarise(NLevels = n()) %>%
    mutate(NLevels = as.numeric(NLevels))
  
  sas_total_grouped_filtered <- read_sas("tests/data/test_outputs/test_5_unique_usub_trt_can.sas7bdat") %>%
    mutate_all(`attributes<-`, NULL) %>% 
    select(TRT01P, NLevels)
    
    # ensure it matches the shiny output
    expect_equal(sas_total_grouped_filtered, tg_total_grouped_filtered)
})
