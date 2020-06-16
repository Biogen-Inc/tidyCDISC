## code to prepare `adsl` dataset goes here
adsl <- haven::read_sas("data-raw/adsl.sas7bdat")

usethis::use_data(adsl, overwrite = TRUE)
