
<!-- README.md is generated from README.Rmd. Please edit that file -->
IDEA
====

The purpose of IDEA is a shiny app to easily create custom tables and figures from ADaM datasets.

Installation
============

``` r
# For HPC users, add RSPM's GHE repo:
options(repos = c(
  CRAN = "https://cran.rstudio.com/",
  ghe = "https://cran.rstudio.com")
)
options('repos') # to confirm "ghe" was added
install.packages("IDEA")
```

Use Case
========

Using the dev/run\_dev.R file, you can run the application locally:

``` r
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
options(shiny.sanitize.errors = FALSE)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application 
run_app()
```

Or (equivalently) this works outside the app...

``` r
# Set options here
options( "golem.app.prod" = TRUE)
options(shiny.sanitize.errors = FALSE)

# Run the application 
IDEA::run_app()
```
