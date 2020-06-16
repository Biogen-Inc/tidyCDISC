

context("Module: Indvidual Explorer 'Build Events' function")


# Function returns a Data frame standardized to include events from vaious OCCDs
# files
filenames <- c("adsl")
# filenames <- c("adsl","adae")
named_list <- setNames(filenames, toupper(filenames))
datafile <- map(.x = named_list, function(x) eval(as.name(tolower(x))) )


test_that("build_events fun", {
  # nrow should never change
  expect_equal(nrow(
    build_events(input_checkbox = c("DS")
                 , input_apply_filter = F
                 , my_usubjid = datafile$ADSL$USUBJID[1]
                 , my_loaded_adams = names(named_list)
                 , my_datafile = datafile # does this need to be reactive?
                 , my_filtered_dat = datafile$ADSL)
    ),
    5
  )
  # start should be class date & end should be NA
  expect_equal(class(
    build_events(input_checkbox = c("DS")
                 , input_apply_filter = F
                 , my_usubjid = datafile$ADSL$USUBJID[1]
                 , my_loaded_adams = names(named_list)
                 , my_datafile = datafile # does this need to be reactive?
                 , my_filtered_dat = datafile$ADSL)$START
  ),
  "Date"
  )
  expect_equal(all(is.na(
    build_events(input_checkbox = c("DS")
                 , input_apply_filter = F
                 , my_usubjid = datafile$ADSL$USUBJID[1]
                 , my_loaded_adams = names(named_list)
                 , my_datafile = datafile # does this need to be reactive?
                 , my_filtered_dat = datafile$ADSL)$END
  )),
  TRUE
  )
  # 
  expect_equal(unique(
    build_events(input_checkbox = c("DS")
                 , input_apply_filter = F
                 , my_usubjid = datafile$ADSL$USUBJID[1]
                 , my_loaded_adams = names(named_list)
                 , my_datafile = datafile # does this need to be reactive?
                 , my_filtered_dat = datafile$ADSL)$EVENTTYP
  ),
  "Milestones"
  )
  expect_equal(colnames(
    build_events(input_checkbox = c("DS")
                 , input_apply_filter = F
                 , my_usubjid = datafile$ADSL$USUBJID[1]
                 , my_loaded_adams = names(named_list)
                 , my_datafile = datafile # does this need to be reactive?
                 , my_filtered_dat = datafile$ADSL)
  ),
  c("EVENTTYP", "START", "END", "tab_st", "tab_en", "DECODE", "DOMAIN")
  )
})







