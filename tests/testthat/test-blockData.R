table_blocks_tester <- R6::R6Class(
  inherit = table_blocks,
  public = list(
    test_stats = function(expected) {
      expect_equal(private$stats, expected)
    },
    test_my_weeks = function(expected) {
      expect_equal(private$my_weeks, expected)
    },
    test_all_cols = function(expected) {
      expect_equal(sort(private$all_cols), sort(expected))
    },
    test_my_avals = function(expected) {
      expect_equal(private$my_avals, expected)
    },
    test_block_drop = function(expected) {
      expect_equal(private$block_drop, expected)
    },
    test_agg_drop = function(expected) {
      expect_equal(private$agg_drop, expected)
    }
  )
)

test_that("table_blocks is working", {
  datalist <- list(ADSL = tidyCDISC::adsl, ADVS = tidyCDISC::advs)
  
  test_bd <- table_blocks_tester$new(datalist)
  
  # Check initialization values
  expect_equal(nrow(test_bd$blocks), 0)
  
  stats <- c("ANOVA", "CHG", "MEAN", "FREQ", "Y_FREQ", "MAX_FREQ", "NON_MISSING", "NESTED_FREQ_DSC", "NESTED_FREQ_ABC")
  test_bd$test_stats(stats)
  
  weeks <- c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "Week 12", "Week 16", "Week 20", "Week 24", "Week 26", "End of Treatment")
  test_bd$test_my_weeks(weeks)
  
  all_cols <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "SITEGR1", "ARM", "TRT01P", "TRT01A", "AGEGR1", "AGEU", "RACE", "SEX", "ETHNIC", "SAFFL", "ITTFL", "EFFFL", "COMP8FL", "COMP16FL", "COMP24FL", "DISCONFL", "DSRAEFL", "DTHFL", "BMIBLGR1", "DURDSGR1", "RFSTDTC", "RFENDTC", "DCDECOD", "EOSSTT", "DCSREAS", "FASFL", "RANDFL", "EOTSTT", "DCTREAS")
  test_bd$test_all_cols(all_cols)
  
  avals <- list(ADVS = list(DIABP = list(ATPT = list("ALL", "AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE", "AFTER STANDING FOR 3 MINUTES")), 
                            PULSE = list(ATPT = list("ALL", "AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE", "AFTER STANDING FOR 3 MINUTES")), 
                            SYSBP = list(ATPT = list("ALL", "AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE", "AFTER STANDING FOR 3 MINUTES"))))
  test_bd$test_my_avals(avals)
  
  test_bd$test_block_drop(list())
  
  test_bd$test_agg_drop(list())
  
  # Check addition of a block
  test_bd$add_block("DIABP", "MEAN", "ALL", "ALL")
  
  expect_equal(nrow(test_bd$blocks), 33)
  
  expected_blocks <- list(txt = "DIABP", df = "ADVS", grp = "ATPT", val = "ALL", lst = c("AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE", "AFTER STANDING FOR 3 MINUTES"))
  expected_aggs <- list(txt = "MEAN", val = "ALL", lst = weeks)
  expected_drops <- process_droppables(list(list(expected_aggs)), list(list(expected_blocks)))
  test_bd$test_block_drop(expected_drops$blocks)
  test_bd$test_agg_drop(expected_drops$aggs)
  
  # Check removal of lines
  test_bd$remove_block(-(1:3))
  
  expect_equal(nrow(test_bd$blocks), 3)
  
  expected_blocks <- list(txt = "DIABP", df = "ADVS", grp = "ATPT", val = "ALL", lst = c("AFTER LYING DOWN FOR 5 MINUTES", "AFTER STANDING FOR 1 MINUTE", "AFTER STANDING FOR 3 MINUTES"))
  expected_aggs <- list(txt = "MEAN", val = "ALL", lst = weeks[1])
  expected_drops <- process_droppables(list(list(expected_aggs)), list(list(expected_blocks)))
  test_bd$test_block_drop(expected_drops$blocks)
  test_bd$test_agg_drop(expected_drops$aggs)
  
})

test_that("table_block wrappers work", {
  datalist <- list(ADSL = tidyCDISC::adsl)
  
  bd <- createBlockdata(datalist)
  
  expect_equal(nrow(bd$blocks), 0)
  
  bd |>
    addBlock("RANDFL", "Y_FREQ") |>
    addBlock("EOSSTT", "NESTED_FREQ_ABC", "DCSREAS")
  
  block_table <-
    structure(list(agg = c("Y_FREQ", "NESTED_FREQ_ABC"), 
                   block = c("RANDFL", "EOSSTT"), 
                   dataset = c("ADSL", "ADSL"), 
                   dropdown = c(NA, "DCSREAS"), 
                   filter = c(NA_character_, NA_character_), 
                   S3 = list(structure("RANDFL", class = c("character", "ADSL")), 
                            structure("EOSSTT", class = c("character", "ADSL"))), 
                   gt_group = structure(c("Y_FREQ of RANDFL", "NESTED_FREQ_ABC of EOSSTT and DCSREAS"), 
                                        class = c("glue", "character")), 
                   label = c("N/A", "N/A"),
                   label_source = c("N/A", "N/A")), 
              row.names = c(NA, -2L), 
              class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(bd$blocks, block_table)
  
  removeBlock(bd, 1)
  
  block_table <-
    structure(list(agg = "NESTED_FREQ_ABC", 
                   block = "EOSSTT", 
                   dataset = "ADSL", 
                   dropdown = "DCSREAS", 
                   filter = NA_character_, 
                   S3 = list(structure("EOSSTT", class = c("character", "ADSL"))), 
                   gt_group = structure("NESTED_FREQ_ABC of EOSSTT and DCSREAS", class = c("glue", "character")), 
                   label = "N/A", 
                   label_source = "N/A"), 
              row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
  expect_equal(bd$blocks, block_table)
  
  setTitle(bd, "My Title")
  expect_equal(bd$title, "My Title")
  
  setGroup(bd, "TRT01P")
  expect_equal(bd$group_by, "TRT01P")
  
})

test_that("User input is working", {
  con <- file()
  options(tidyCDISC.connection = con)
  
  datalist <- list(ADSL = tidyCDISC::adsl, ADAE = tidyCDISC::adae)
  
  bd <- createBlockdata(datalist)
  
  user_input <- c("AGEGR1", "FREQ")
  write(paste(user_input, collapse = "\n"), con)
  addBlock(bd)
  expect_equal(nrow(bd$blocks), 1)
  
  user_input <- c("AGE", "MEN")
  write(paste(user_input, collapse = "\n"), con)
  expect_error(addBlock(bd))
  expect_equal(nrow(bd$blocks), 1)
  
  user_input <- c("AGE", "MEN", "MEAN")
  write(paste(user_input, collapse = "\n"), con)
  addBlock(bd)
  expect_equal(nrow(bd$blocks), 2)
  
  options(tidyCDISC.connection = stdin())
  close(con)
})
