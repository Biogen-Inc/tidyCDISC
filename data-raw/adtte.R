## code to prepare `adtte` dataset goes here
adtte <- haven::read_xpt("data-raw/adtte.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                              .fns = na_if, y = ""))

usethis::use_data(adtte, overwrite = TRUE)
