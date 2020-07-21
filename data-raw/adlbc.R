## code to prepare `adlbc` dataset goes here
adlbc <- haven::read_xpt("data-raw/adlbc.xpt")

usethis::use_data(adlbc, overwrite = TRUE)
