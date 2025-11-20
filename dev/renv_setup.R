# Fix renv
Sys.setenv(RENV_CONFIG_PAK_ENABLED=TRUE)
Sys.setenv(RENV_CONFIG_PPM_ENABLED=TRUE)

Sys.setenv(RENV_CONFIG_PAK_ENABLED=FALSE)
Sys.setenv(RENV_CONFIG_PPM_ENABLED=FALSE)

renv::init()
renv::update()
#pak::pak("usethis")
usethis::use_package("ggsurvfit")
#utils::install.packages("https://packagemanager.posit.co/cran/latest/bin/windows/contrib/4.4/jsonlite_2.0.0.zip")
#utils::install.packages("https://packagemanager.posit.co/cran/latest/bin/windows/contrib/4.4/httpuv_1.6.16.zip")
#utils::install.packages("https://packagemanager.posit.co/cran/latest/bin/windows/contrib/4.4/ggsurvfit_1.1.0.zip")
#utils::install.packages("https://packagemanager.posit.co/cran/latest/bin/windows/contrib/4.4/ggplot2_3.5.2.zip")
pak::pak("devtools")
usethis::use_package("devtools", type = "Suggests")
usethis::use_package("arrow")
usethis::use_package("readxl")
usethis::use_package("cli")
usethis::use_package("fs")
usethis::use_package("data.table")
usethis::use_package("gtsummary")
usethis::use_package("broom.helpers")
usethis::use_package("vroom")
usethis::use_package("tidyselect")
# usethis::use_package("arcusTStidy")
usethis::use_package("tibble")


renv::snapshot(type = "explicit")

renv::install("pkgload")
renv::restore()

rsconnect::writeManifest(appMode = "shiny")
