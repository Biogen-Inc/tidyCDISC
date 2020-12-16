## code to prepare `adsl` dataset goes here
adsl <- haven::read_xpt("data-raw/adsl.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                       .fns = na_if, y = "")) %>%
  group_by(USUBJID) %>%
  mutate(DTHDT = as.Date(ifelse(DTHFL == "Y", 
                        paste(sample(seq.Date(TRTSDT, TRTEDT, by = 1), size = 1))
                        , NA_character_)
                ))
attr(adsl$DTHDT, "label") <- "Date of Death"

usethis::use_data(adsl, overwrite = TRUE)
