
#' Individual Explorer Cicerone Guide For Selecting a Patient
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
guide_ind_exp_sel <- cicerone::Cicerone$
  new()$
  step(
    "adv_filtering",
    "Pre-filter list of USUBJID's",
    "Allow Users to "
  )$
  step(
    "selPatNo",
    "Choose Patient (USUBJID)",
    "Explore a single patient's journey the study"
  )


#' Individual Explorer Cicerone Guide For Selecting a Patient with Advanced
#' Filtering
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
guide_ind_exp_sel_adv <- cicerone::Cicerone$
  new()$
  step(
    "all-column-blocks",
    "Uploaded Data Variables",
    "Variables are grouped by uploaded datasets where you can drag any column from an ADSL into the Variables drop zone, and PARAMCDs from BDS files"
  )



#' Individual Explorer Cicerone Guide For Analyzing Patient Event Data
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
ind_exp_events <- cicerone::Cicerone$
  new()#$
  # step(
  #   "all-column-blocks",
  #   "Uploaded Data Variables",
  #   "Variables are grouped by uploaded datasets where you can drag any column from an ADSL into the Variables drop zone, and PARAMCDs from BDS files"
  # )

#' Individual Explorer Cicerone Guide For Analyzing Patient Visit Data
#'
#' This object is used within the table generator Individual Explorer to add
#' help text to the various fields using a help buttom
#'
#' @importFrom cicerone Cicerone
ind_exp_visits <- cicerone::Cicerone$
  new()#$
  # step(
  #   "all-column-blocks",
  #   "Uploaded Data Variables",
  #   "Variables are grouped by uploaded datasets where you can drag any column from an ADSL into the Variables drop zone, and PARAMCDs from BDS files"
  # )
