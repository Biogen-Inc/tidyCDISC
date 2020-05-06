

######################################################################################
# Inputs for Data Compliance Module
#
# Module found in "modules/data_compliance.R"
# Module description:
# a module that will interface with the data import module and either (I) display an
# error if needed variables don't exist and stop them from proceeding or (II) warn the
# user if if some columns are missing that are vital for the app to make sense, but 
# they can continue if they wish.
######################################################################################

hard_rules <- list(
  ADSL = list(need = c("USUBJID","SUGAR"),
              warn = c("USUBJID")),
  ADLB = list(need = c("USUBJID"),
              warn = c("USUBJID", "LBDT", "LBSTNRLO", "LBSTNRHI")),
  ADMH = list(need = c("USUBJID", "MHCAT"),
              warn = c("USUBJID", "MHCAT", "MHSTDTC", "MHENDTC", "MHDECOD", "MHTERM")),
  ADCM = list(need = c("USUBJID"),
              warn = c("USUBJID", "CMSTDT", "CMDECOD")),
  ADAE = list(need = c("USUBJID"),
              warn = c("USUBJID", "AESTDT", "AEDECOD", "AESEV", "AESER"))
)
# idea_hard_rules # peek

dfWith_rules <- list(
  PARAMCD = list(need = c("USUBJID"),
                 warn = c("USUBJID", "AVISITN", "VISIT", "AVISIT", "PARAMCD", "PARAM", "AVAL", "CHG", "BASE"))
)
# idea_dfWith_rules # peek

