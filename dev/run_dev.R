# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application 
run_app()

# # devtools::install_github("MayaGans/IDEAFilter")
# remotes::install_github("MayaGans/IDEAFilter", force = T)
# remotes::install_github("JohnCoene/tippy", force = T)
# remotes::install_github("rstudio/rsconnect", force = T)
# library(installr)
# install.rstudio() # error
