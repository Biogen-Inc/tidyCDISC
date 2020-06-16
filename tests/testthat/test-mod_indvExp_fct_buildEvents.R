

context("Module: Indvidual Explorer 'Build Events' function")


# Function returns a Data frame standardized to include events from vaious OCCDs
# files
# filenames <- c("adsl","adae")
# named_list <- setNames(filenames, toupper(filenames))
# datafile <- map(.x = named_list, function(x) eval(as.name(tolower(x))) )


test_that("build_events fun", {
  # Need to add this in as a secondary dt var in case aesdt is missing
  # | !is.na(ASTDT)
  # expect_equal(
  #   build_events(input_checkbox = c("Milesontes")
  #                , input_apply_filter = F
  #                , my_usubjid = datafile$ADSL$USUBJID[1]
  #                , my_loaded_adams = names(named_list)
  #                , my_datafile = datafile # does this need to be reactive?
  #                , my_filtered_dat = datafile$ADSL),
  #   class(shiny::tagList())
  # )
})







