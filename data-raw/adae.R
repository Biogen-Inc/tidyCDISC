## code to prepare `adae` dataset goes here
adae <- haven::read_xpt("data-raw/adae.xpt")

usethis::use_data(adae, overwrite = TRUE)
