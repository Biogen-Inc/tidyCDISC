# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# golem default's to these options
pkgload::load_all(helpers = FALSE, attach_testthat = FALSE) # export_all = FALSE # ac removed: if false, IDEAFilter fails
options( "golem.app.prod" = TRUE)
options(shiny.sanitize.errors = FALSE)
# rsconnect::writeManifest() # ac added
tidyCDISC::run_app() # add parameters here (if any)

# Test IDEAFilter



# # Install internal version from RSPM's GHE repo:
# options(repos = c(
#   CRAN = "https://cran.rstudio.com/",
#   ghe = "http://10.240.22.159:4242/Git-Biogen/latest")
# )
# options('repos') # to confirm "ghe" was added
# install.packages("IDEAFilter")

# # Install external version from Biogen's external repo:
# remotes::install_github("Biogen-Inc/IDEAFilter")
# devtools::install_github("Biogen-Inc/IDEAFilter")


# devtools::install_github("MayaGans/IDEAFilter") # Try for fun
# devtools::install_github("dgkf/shinyDataFilter")

# library(shiny)
# shinyAppFile(system.file("examples", "basic_app", "app.R", package = "IDEAFilter"))
# shinyAppFile(system.file("examples", "basic_app", "app.R", package = "shinyDataFilter"))
