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
# usethis::use_package("rvest")
usethis::use_package("haven")
usethis::use_package("shinyWidgets")
usethis::use_package("plotly") 
# usethis::use_package("RColorBrewer")
# usethis::use_package("gridExtra")
# usethis::use_package("grid")
# usethis::use_package("janitor")
# usethis::use_package("shinythemes")
usethis::use_package("rmarkdown")
# usethis::use_package("shinytest")
# usethis::use_package("waiter")
usethis::use_package("timevis")
usethis::use_package("sjlabelled") 
# usethis::use_package("data.table")
usethis::use_package("gt")
# usethis::use_package("shinyBS")
usethis::use_package("knitr", type = "Suggests")
usethis::use_package("pkgdown", type = "Suggests")#, min_version = "1.6.1")
usethis::use_package("rlang")
# usethis::use_package("stringi")
# usethis::use_package("forcats")
# usethis::use_package("data.table")

# cannot use dev!
# usethis::use_dev_package("tippy") #remotes::install_github("JohnCoene/tippy") #‘1.0.0’
usethis::use_package("tippy")

# install.packages('IDEAFilter', repos='http://cran.us.r-project.org')
usethis::use_package("IDEAFilter") # install.packages("IDEAFilter") 
# old...
# usethis::use_dev_package("IDEAFilter") # devtools::install_github("Biogen-Inc/IDEAFilter")

# usethis::use_dev_package("rsconnect") 
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("tidyr")
# usethis::use_package("tibble")
# usethis::use_package("magrittr")
usethis::use_package("cicerone")
usethis::use_package("glue")
usethis::use_package("lazyeval",type="Suggests")
usethis::use_package("scales",type = "Suggests")
usethis::use_package("GGally")
usethis::use_package("survival")
usethis::use_package("ggcorrplot")
usethis::use_pipe()



## Add modules ---- ran
## Create a module infrastructure in R/
## Only argument is Name of the module

golem::add_module( name = "dataUpload" ) # ran
golem::add_module( name = "dataComply" ) # ran
golem::add_module( name = "dataComplyRules" ) # ran

golem::add_module( name = "tableGen" ) #ran

# golem::add_module( name = "selectData" )  #ran
golem::add_module( name = "popExp" )      #ran
golem::add_module( name = "popExpScat" )  #ran
golem::add_module( name = "popExpSpag" )  #ran
golem::add_module( name = "popExpBoxp" )  #ran
golem::add_module( name = "popExpHeat" )  #ran
golem::add_module( name = "popExpHist" )  #ran
golem::add_module( name = "popExpMeans" ) #ran
golem::add_module( name = "popExpHBar" )  #ran

golem::add_module( name = "indvExp") # ran
golem::add_module( name = "indvExpPat" ) # ran
golem::add_module( name = "indvExpPatEvents" ) # ran
golem::add_module( name = "indvExpPatVisits" ) # ran


## Add helper functions ---- ran
## Creates fct_* and utils_*

golem::add_utils( "strObjs" ) # ran
golem::add_utils( "helpers" ) # ran, but Maya still has to uncomment and document her funtions

golem::add_fct( "helpers", module = "dataComply" ) # ran
golem::add_fct( "helpers", module = "dataComplyRules" ) # ran

golem::add_fct( "meanSummary", module = "tableGen" ) 
golem::add_fct( "methods", module = "tableGen" ) 
golem::add_fct( "blocks", module = "tableGen" ) 
golem::add_fct( "mean", module = "tableGen" ) 
golem::add_fct( "chg", module = "tableGen" ) 
golem::add_fct( "freq", module = "tableGen" ) 
golem::add_fct( "anova", module = "tableGen" ) 

golem::add_utils("helpers", module = "indvExp" )
golem::add_fct( "buildEvents", module = "indvExp" ) # ran: used in modules: indvExpPatEvents & indvExpPatVisits
golem::add_fct( "organizeEvent", module = "indvExp" ) # ran
golem::add_fct( "plot", module = "indvExpPatVisits" ) # ran

golem::add_utils( "helpers" ) # ran

# golem::add_fct( "scttr", module = "popExp") #ran
# golem::add_fct( "bxplt", module = "popExp") #ran
# golem::add_fct( "sumtb", module = "popExp") #ran
# golem::add_fct( "ovrpl", module = "popExp") #ran
# golem::add_fct( "corrm", module = "popExp") #ran
# golem::add_fct( "corrm", module = "popExp") #ran


## External resources - ran
## Creates .js and .css files at inst/app/www
# golem::add_js_handler( "handlers" ) # ac golem: none
golem::add_js_file( "script" )
golem::add_js_file( "accordian" )
golem::add_js_file( "analytics" )
golem::add_js_file( "recipe" )
golem::add_js_file( "sync_divs" )
golem::add_js_file( "detect_browser" )
golem::add_js_file( "test2" )

golem::add_css_file( "yeti" )
golem::add_css_file( "styles" )

###################################################################
# ac golem: Aaron stopped here and pushed code to team on 6/3/2020
###################################################################

## Add internal datasets ---- not run
## If you have data in your package
usethis::use_data_raw( name = "adsl", open = FALSE ) # ran 
usethis::use_data_raw( name = "adlbc", open = FALSE ) # ran
usethis::use_data_raw( name = "advs", open = FALSE ) # ran
usethis::use_data_raw( name = "adae", open = FALSE ) # ran
usethis::use_data_raw( name = "adtte", open = TRUE ) # ran

## Tests ---- not run
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ---- run
# usethis::use_vignette("test") # ran just to add VignetteBuilder to DESCRIPTION
usethis::use_vignette("tidyCDISC")
usethis::use_vignette("x00_Data_Upload")
usethis::use_vignette("x02_Pop_Exp")
usethis::use_vignette("x04_Filtering")
usethis::use_vignette("Blog")
usethis::use_vignette("announcing-tidycdisc-0-0-1-1")
usethis::use_vignette("dev01_Table_Gen")
usethis::use_vignette("dev02_Pop_Exp")
usethis::use_vignette("dev03_Indv_Expl")


# Before submitting a PR, run this code & update NEWS.md
usethis::use_version("patch") #choices: "dev", "patch", "minor", "major"

# Build pkg, including vignettes. Do this before updating documentation.
devtools::build() # calls pkgbuld::build()     # X.X MB
devtools::build(args = "--no-build-vignettes") # X.X MB
# pkgbuild::build() 
# pkgbuild::build()
pkgbuild::build(vignettes = FALSE) # 1.2 MB don't build vignettes to save time on build


# update pkgdown site only if user needs refreshed documentation
devtools::install_version("pkgdown", version = "2.0.3",
                          repos = "http://cran.us.r-project.org")
# usethis::use_pkgdown() # Run once to configure your package to use pkgdown
pkgdown::build_articles() #
pkgdown::build_articles_index() #TRUE
pkgdown::build_home()
pkgdown::build_reference_index() # WAS missing topics
pkgdown::build_reference(preview = TRUE)
pkgdown::build_news()
pkgdown::build_site() # Run to build entire website

# # GitHub Actions
# usethis::use_github_action()
#
usethis::use_github_pages() # failed. Instead run this: https://gist.github.com/ramnathv/2227408
usethis::use_github_action("pkgdown")
# 
# # Chose one of the three
# # See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full() 
# # Add action for PR
# usethis::use_github_action_pr_commands()

# # run R CMD check on CRAN’s servers
# # ?devtools::check_win_release
# devtools::check_win_release() #ran
# devtools::check_win_oldrelease()
# devtools::check_win_devel() #ran but errored before sending:
#   # Error in curl::curl_fetch_memory(url, handle = h) : Access denied: 403

# check for downstream dependencies
usethis::use_revdep()
# devtools::revdep_check() # doesn't exist anymore?
# install.packages("revdepcheck") # doesn't exist for my version of R
# revdepcheck::revdep_check(num_workers = 4)
# Prepare for CRAN ----

############### Thinkr's Prepare for Cran checklist ###################
# https://github.com/ThinkR-open/prepare-for-cran

# Update dependencies in DESCRIPTION
# install.packages("attachment")
attachment::att_amend_desc() 

# # Run tests and examples (usually done with check)
# devtools::test()
# devtools::run_examples()
# # autotest::autotest_package(test = TRUE)

# Check package as CRAN
pkgbuild::build()
pkgbuild::build(vignettes = FALSE) # check build size quickly
devtools::check()
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran")) # , "--no-build-vignettes"

# Check content
# remotes::install_github("ThinkR-open/checkhelper")
tags <- checkhelper::find_missing_tags()
View(tags)

# Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# remotes::install_github("r-lib/urlchecker")
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub
first_chk <- devtools::check_rhub()
cran_chk <- rhub::check_for_cran(check_args = c("--as-cran", "--no-build-vignettes"))

rhub::check_on_windows(check_args = "--force-multiarch")
rhub::check_on_solaris()
# _win devel
devtools::check_win_devel()

# Doesn't really apply to tidyCDISC since it's never been published to CRAN
# Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
usethis::use_git_ignore("revdep/")
usethis::use_build_ignore("revdep/")

devtools::revdep()
library(revdepcheck)
# In another session
id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
rstudioapi::terminalKill(id)
# See outputs
revdep_details(revdep = "pkg")
revdep_summary()                 # table of results by package
revdep_report() # in revdep/
# Clean up when on CRAN
revdep_reset()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

########## end thinkr's checklist #########

# Since this package has a ton of large vignettes, we're use the below function
# to build the vignettes in the doc/ (not docs/) folder. plus a vignette index
# is created in Meta/vignette.rds. Both doc/ and Meta/ are added to the
# .rbuildignore. These files can be checked into version control, so they can be
# viewed with browseVignettes() and vignette() if the package has been loaded
# with load_all() without needing to re-build them locally. Then, below in
# release(), we can pass an argument "--no-vignettes" to not build the vignettes,
# saving the CRAN machines processing time.
# devtools::build_vignettes() # naw, don't use, instead just give them the pkgdown site


# When ready, submit to CRAN for the first time
devtools::release(check = FALSE)
# Re-submit:
# devtools::submit_cran()

# ## Code coverage ---- not run
# ## (You'll need GitHub there)
# # usethis::use_github() # don't need to do this. AC manually created a remote origin in terminal and pushed to GitHub.
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! ---- not run
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

