# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
# options(shiny.trace = F)

#Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package, which runs these three functions...
golem::document_and_reload()

# Run the application 
run_app()



