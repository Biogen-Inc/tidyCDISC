## code to prepare `adsl` dataset goes here
adsl <- haven::read_xpt("data-raw/adsl.xpt")

usethis::use_data(adsl, overwrite = TRUE)
