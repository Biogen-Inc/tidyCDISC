## code to prepare `adtte` dataset goes here
advs <- haven::read_xpt("data-raw/advs.xpt") %>%
  dplyr::mutate(dplyr::across(.cols = where(is.character), 
                              .fns = na_if, y = ""))

usethis::use_data(advs, overwrite = TRUE)
