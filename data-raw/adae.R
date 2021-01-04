## code to prepare `adae` dataset goes here
adae <- haven::read_xpt("data-raw/adae.xpt") %>%
  left_join(
    haven::read_xpt("data-raw/adsl.xpt") %>%
      distinct(USUBJID, TRT01P, TRT01PN)
    ) %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                       .fns = na_if, y = "")) %>%
  dplyr::mutate(AESEVN = case_when(AESEV == "MILD" ~ 1,
                                   AESEV == "MODERATE" ~ 2,
                                   AESEV == "SEVERE" ~ 3,
                                   TRUE ~ NA_real_),
                AREL = if_else(AEREL %in% c('NOT RELATED', 'UNLIKELY RELATED', 'NONE', 'REMOTE'),
                          'NOT RELATED', 'RELATED'),
                ARELN = if_else(AREL == 'NOT RELATED', 0, 1),
                AEACN = case_when(
                  AREL == 'RELATED' & AESER == 'Y' & TRT01P == "Xanomeline High Dose" ~ "DOSE REDUCED",
                  AREL == 'RELATED' & AESER == 'Y' & TRT01P == "Xanomeline Low Dose" ~ "DRUG WITHDRAWN",
                  TRUE ~ "",
                ),
                AEACNOTH = if_else(AEACN == 'DRUG WITHDRAWN', "RESULTED IN WITHDRAWAL FROM STUDY","")
  ) %>%
  select(-TRT01P, -TRT01PN)

attr(adae$AESEVN, "label") <- "Severity/Intensity (N)"
attr(adae$AREL, "label") <- "Analysis Causality"
attr(adae$ARELN, "label") <- "Analysis Causality (N)"
attr(adae$AEACN, "label") <- "Action Taken with Study Treatment"
attr(adae$AEACNOTH, "label") <- "Other Action Taken"
usethis::use_data(adae, overwrite = TRUE)

