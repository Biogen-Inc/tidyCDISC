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

## Dependencies ---- ran
## Add one line by package you want to add as dependency
# usethis::use_package("shiny") # already added as a recommended package
# usethis::use_package("DT") # already added as a recommended package
# usethis::use_package("glue") # already added as a recommended package
usethis::use_package("shinyjs")
usethis::use_package("rvest")
usethis::use_package("haven")
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
usethis::use_package("sjlabelled") 
usethis::use_package("data.table")
usethis::use_package("gt")
usethis::use_package("shinyBS")
usethis::use_package("knitr")
usethis::use_package("rlang")
usethis::use_package("stringi")
usethis::use_dev_package("tippy") #remotes::install_github("JohnCoene/tippy")
usethis::use_dev_package("IDEAFilter") # devtools::install_github("MayaGans/IDEAFilter")

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
usethis::use_package("magrittr")
usethis::use_pipe()



## Add modules ---- ran
## Create a module infrastructure in R/
## Only argument is Name of the module
golem::add_module( name = "dataUpload" ) 
golem::add_module( name = "dataComply" )

golem::add_module( name = "tableGen" )

golem::add_module( name = "selectData" )
golem::add_module( name = "popExp" )
golem::add_module( name = "popExpScat" )
golem::add_module( name = "popExpSpag" )
golem::add_module( name = "popExpBoxp" )
golem::add_module( name = "popExpHeat" )
golem::add_module( name = "popExpHist" )
golem::add_module( name = "popExpMeans" )
golem::add_module( name = "popExpHBar" )

golem::add_module( name = "indvExp")
golem::add_module( name = "indvExpPat" )
golem::add_module( name = "indvExpPatEvents" )
golem::add_module( name = "indvExpPatVisits" )


## Add helper functions ---- ran
## Creates fct_* and utils_*

golem::add_utils( "strObjs" )
golem::add_utils( "helpers" ) # old files: css.R, CapStr.R, allowedOperators.R


golem::add_fct( "detectStandard", module = "dataUpload" )
golem::add_fct( "evalStandard", module = "dataUpload" ) 
golem::add_fct( "hasColumn", module = "dataUpload" ) 
golem::add_fct( "hasField", module = "dataUpload" ) 
golem::add_fct( "getSettingsMeta", module = "dataUpload" )

golem::add_fct( "helpers", module = "dataComply" ) 

golem::add_fct( "meanSummary", module = "tableGen" ) 
golem::add_fct( "methods", module = "tableGen" ) 
golem::add_fct( "blocks", module = "tableGen" ) 
golem::add_fct( "mean", module = "tableGen" ) 
golem::add_fct( "chg", module = "tableGen" ) 
golem::add_fct( "freq", module = "tableGen" ) 
golem::add_fct( "anova", module = "tableGen" ) 

golem::add_fct( "buildEvents", module = "indvExp" ) # used in modules: indvExpPatEvents & indvExpPatVisits
golem::add_fct( "plot", module = "indvExpPatVisits" )

## External resources - ran
## Creates .js and .css files at inst/app/www
# golem::add_js_handler( "handlers" ) # ac golem: none
golem::add_js_file( "script" )
golem::add_js_file( "accordian" )
golem::add_js_file( "analytics" )
golem::add_js_file( "recipe" )
golem::add_js_file( "sync_divs" )

golem::add_css_file( "yeti" )
golem::add_css_file( "styles" )
golem::add_css_file( "index" )


###################################################################
# ac golem: Aaron stopped here and pushed code to team on 6/3/2020
###################################################################

## Add internal datasets ---- not run
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ---- not run
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ---- not run
usethis::use_vignette("IDEA")
devtools::build_vignettes()

## Code coverage ---- not run
## (You'll need GitHub there)
# usethis::use_github() # don't need to do this. AC manually created a remote origin in terminal and pushed to github.
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ---- not run
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

