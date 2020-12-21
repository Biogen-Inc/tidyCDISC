## code to prepare `adae` dataset goes here
adae <- haven::read_xpt("data-raw/adae.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                       .fns = na_if, y = "")) %>%
  dplyr::mutate(AESEVN = case_when(AESEV == "MILD" ~ 1,
                                   AESEV == "MODERATE" ~ 2,
                                   AESEV == "SEVERE" ~ 3,
                                   TRUE ~ NA_real_),
                AREL = if_else(AEREL %in% c('NOT RELATED', 'UNLIKELY RELATED', 'NONE', 'REMOTE'),
                          'NOT RELATED', 'RELATED')
  )
attr(adae$AESEVN, "label") <- "Severity/Intensity (N)"
usethis::use_data(adae, overwrite = TRUE)

