## code to prepare `adsl` dataset goes here
library(dplyr)
adsl <- haven::read_xpt("data-raw/adsl.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                       .fns = na_if, y = "")) %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(FASFL = ITTFL,
                RANDFL = ITTFL,
                EOTSTT = EOSSTT,
                DCTREAS = DCSREAS,
                DTHDT = as.Date(ifelse(DTHFL == "Y", 
                        paste(sample(seq.Date(TRTSDT, TRTEDT, by = 1), size = 1))
                        , NA_character_)
                )) %>%
  ungroup()

attr(adsl$FASFL, "label") <- "Full Analysis Set Population Flag"
attr(adsl$RANDFL, "label") <- "Randomized Population Flag"
attr(adsl$EOTSTT, "label") <- "End of Treatment Status"
attr(adsl$DCTREAS, "label") <- "Reason for Discontinuation of Treatment"
attr(adsl$DTHDT, "label") <- "Date of Death"

usethis::use_data(adsl, overwrite = TRUE)
