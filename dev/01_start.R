# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ---- ran
## Add meta data about your application
golem::fill_desc(
  pkg_name = "IDEA", # The Name of the package containing the App 
  pkg_title = "IDEA: Quick Exploratory Data Analyses on ADaM-ish Datasets", # The Title of the package containing the App 
  pkg_description = "The IDEA application relies on sas7bdat files in the ADaM-ish data structure to provide users a quick exploratory dive into common visualizations without writing a sinlge line of code. Prominent modules/ features of the application are the Table Generator, Population Explorer, and the Individual Explorer. The Table Generator allows users to drag and drop variables and desired statistics (frequencies, means, Anova, t-test, and other summary statistics) into bins that automagically create stunning tables with validated information. The Population Explorer offers various plots to visualize general trends in the population from various vantage points. Plot modules currently include scatter plot, spaghetti plot, box plot, historgram, means plot, and bar plot. Each plot type allows the user to plot uploaded variables against one another, and dissect the population by filtering out certain subjects. Last, the Individual Explorer establishes a cohesive patient narrative, allowing the user to interact with patient metrics (params) by visit or plotting important patient events on a timeline. All modules allow for concise filtering & downloading bulk outputs into html or pdf formats to save for later.", 
  author_first_name = "Maya", # Your First Name
  author_last_name = "Gans", # Your Last Name
  author_email = c("maya.gans@biogen.com"), # Your Email
  repo_url = "https://github.com/Biogen-Inc/tidyCDISC" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ---- ran
golem::set_golem_options()

## Create Common Files ---- ran
## See ?usethis for more information
usethis::use_mit_license( name = "Maya Gans; Aaron Clark; Robert Krajcik; Nate Mockler" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" ) #Experimental, Maturing, Stable, Superseded, Archived, Dormant, Questioning
usethis::use_news_md( open = FALSE )

## Use git ---- ran
usethis::use_git("AC Initial Commit")
# $ git remote add origin https://github.com/Biogen-Inc/tidyCDISC.git

## Init Testing Infrastructure ---- ran
## Create a template for tests
# Call `use_test()` to initialize a basic test file and open it for editing.
golem::use_recommended_tests()

## Use Recommended Packages ---- ran slightly altered
# By Default, this will add “shiny”, “DT”, “attempt”, “glue”, “htmltools”, and “golem” as a dependency 
# to our package. Since we don't need all of those, we'll adjusted the recommended vector...
golem::use_recommended_deps(recommended = c("shiny", "DT",  "glue", "golem")) #"attempt", "htmltools",

## Favicon ---- ran
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon("inst/app/www/IDEA_FAVICON.ico") # path = "path/to/ico". Can be an online file. 
# Favicon is automatically linked in app_ui via `golem_add_external_resources()`

## Add helper functions ---- ran. Go checkout new files
golem::use_utils_ui() # File created at ~IDEA/R/golem_utils_ui.R
golem::use_utils_server() # IDEA/R/golem_utils_server.R

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

