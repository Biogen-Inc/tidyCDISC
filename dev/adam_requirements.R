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

usethis::use_data(adam_requirements, internal = TRUE, overwrite = TRUE)