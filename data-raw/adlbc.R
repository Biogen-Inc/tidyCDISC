## code to prepare `adlbc` dataset goes here
adlbc <- haven::read_sas("data-raw/adlbc.sas7bdat")

usethis::use_data(adlbc, overwrite = TRUE)
