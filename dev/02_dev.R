# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shiny") # already added as a recommended package
usethis::use_package("shinyjs")
usethis::use_dev_package("tippy") #remotes::install_github("JohnCoene/tippy")
usethis::use_package("rvest")
usethis::use_package("haven")
usethis::use_package("DT") # already added as a recommended package
usethis::use_package("shinyWidgets")
usethis::use_package("plotly") 
usethis::use_package("RColorBrewer")
usethis::use_package("gridExtra")
usethis::use_package("grid")
usethis::use_package("janitor")
usethis::use_package("shinythemes")
usethis::use_package("rmarkdown")
usethis::use_package("shinytest")
usethis::use_package("waiter")
usethis::use_package("timevis")
# usethis::use_package("glue") # already added as a recommended package
usethis::use_package("sjlabelled") 
usethis::use_package("data.table")
usethis::use_package("gt")
usethis::use_package("shinyBS")
usethis::use_package("knitr")
usethis::use_package("rlang")
usethis::use_package("stringi")

# Error: 'tidyverse' is a meta-package and it is rarely a good idea to depend on it. Please 
# determine the specific underlying package(s) that offer the function(s) you need and depend 
# on that instead. For data analysis projects that use a package structure but do not implement
# a formal R package, adding 'tidyverse' to Depends is a reasonable compromise. Call 
# `use_package("tidyverse", type = "depends")` to achieve this.
# usethis::use_package("tidyverse")
# ran these:
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("forcats")
usethis::use_package("tidyr")
usethis::use_package("tibble")

# include these too?
# usethis::use_package("readr")

# waiting for this to be confirmed working
# usethis::use_dev_package("IDEAFilter") # devtools::install_github("MayaGans/IDEAFilter")

# don't need anymore
# usethis::use_package("rtf")





## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "name_of_module1" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("IDEA")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

