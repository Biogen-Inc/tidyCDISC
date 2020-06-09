library(testthat)
library(IDEA)
library(shinyjs)

# ---------------------------------------------
# Table Generator Setup
# ---------------------------------------------

# create dummy dataframe to use for all tests
tg_data <- tidyr::tibble(
  USUBJID = 1:10,
  AGE = c(20,30,40,30,40,60,20,30,50,40),
  PARAMCD = "DIABP",
  SEX = c(rep("F", 5), rep("M", 5)),
  COUNTRY = c(rep(c("Canada", "USA"), 5)),
  AVISIT = c(rep(c("Week 1", "Week 2"), 5)),
  AVAL = c(20,30,40,30,40,60,20,30,50,40),
  CHG = c(20,30,40,30,40,60,20,30,50,40)
)

# recreate the shiny output of a dragged MEAN and AGE block
agg <- list(numbers = list(list(txt = "MEAN",val = "NONE")))
block <- list(numbers = list(list(txt = "\n  AGE\n  ",df = "ADSL")))


test_check("IDEA")
