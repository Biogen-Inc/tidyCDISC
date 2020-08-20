## code to prepare `adae` dataset goes here
adae <- haven::read_xpt("data-raw/adae.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character),
                              .fns = na_if, y = ""))

usethis::use_data(adae, overwrite = TRUE)
