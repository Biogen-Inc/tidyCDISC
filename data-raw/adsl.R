## code to prepare `adsl` dataset goes here
adsl <- haven::read_xpt("data-raw/adsl.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                       .fns = na_if, y = ""))

usethis::use_data(adsl, overwrite = TRUE)
