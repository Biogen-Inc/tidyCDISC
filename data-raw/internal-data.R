pretty_blocks <- tidyr::tibble(
  Pattern = c(
    "MEAN",
    "FREQ",
    "CHG",
    "Y_FREQ",
    "MAX_FREQ",
    "NON_MISSING",
    "NESTED_FREQ_DSC",
    "NESTED_FREQ_ABC"
  ),
  Replacement = c(
    "Descriptive Statistics",
    "Summary Counts",
    "Descriptive Statistics of Change from Baseline",
    "Subject Count for those with 'Y' values",
    "Subject Count for maximum",
    "Subject Count for those with Non Missing values",
    "Subject Count at each variable level, sorted descending by total counts",
    "Subject Count at each variable level, sorted alphabetically by name"
  )
)


tg_data <- tidyr::tibble(
  USUBJID = 1:10,
  AGE = c(20, 30, 40, 30, 40, 60, 20, 30, 50, 40),
  PARAMCD = "DIABP",
  PARAM = "Diastolic",
  SEX = c(rep("F", 5), rep("M", 5)),
  COUNTRY = c(rep(c("Canada", "USA"), 5)),
  AVISIT = c(rep(c("Week 1", "Week 2"), 5)),
  AVISITN = c(rep(c(1, 2), 5)),
  AVAL = c(20, 30, 40, 30, 40, 60, 20, 30, 50, 40),
  CHG = c(20, 30, 40, 30, 40, 60, 20, 30, 50, 40),
  ITTFL = c('Y', 'Y', rep(NA_character_, 8)),
  SAFFL = c('Y', 'Y', 'N', rep(NA_character_, 7)),
  DTHDT = c(as.Date("2020-01-01"), rep(NA, 9)),
  AVISITf1 = factor(c(rep(c("Week 1", "Week 2"), 5)), c("Week 1", "Week 2")),
  AVISITf2 = factor(c(rep(c("Week 1", "Week 2"), 5)), c("Week 2", "Week 1")),
  AVISITf2N = AVISITN
)

# #' ADaM domain variable requirements
# #'
# #' This internal object defines required and recommended variables for common ADaM datasets.
# #' Used by `suggest_adam_domain()` and `validate_adamish()`.
# #' @keywords internal
# "adam_requirements"

adam_requirements <- list(
  ADSL = list(
    required = c("STUDYID", "USUBJID"),
    recommended = c("AGE", "SEX", "RACE", "ARM", "TRT01P")
  ),
  ADLB = list(
    required = c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "AVISIT"),
    recommended = c("AVISITN", "DTYPE", "ABLFL", "ADTM")
  ),
  ADVS = list(
    required = c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "AVISIT"),
    recommended = c("AVISITN", "ADY", "DTYPE")
  ),
  ADQS = list(
    required = c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "AVISIT"),
    recommended = c("AVISITN", "ADY")
  ),
  ADAE = list(
    required = c("STUDYID", "USUBJID", "AEDECOD", "AESTDTC"),
    recommended = c("AESER", "AESEV", "AEACN", "AEOUT")
  ),
  ADEG = list(
    required = c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL", "AVISIT"),
    recommended = c("AVISITN", "ADTM")
  ),
  ADTTE = list(
    required = c(
      "STUDYID",
      "USUBJID",
      "PARAMCD",
      "PARAM",
      "AVAL",
      "CNSR",
      "ADT"
    ),
    recommended = c("STARTDT", "ADTM", "AVISIT")
  )
)

usethis::use_data(
  pretty_blocks,
  tg_data,
  adam_requirements,
  internal = TRUE,
  overwrite = TRUE
)
