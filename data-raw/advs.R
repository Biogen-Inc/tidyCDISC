## code to prepare `adtte` dataset goes here
advs <- haven::read_xpt("data-raw/advs.xpt")

usethis::use_data(advs, overwrite = TRUE)
