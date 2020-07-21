# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
options(shiny.sanitize.errors = FALSE)
IDEA::run_app() # add parameters here (if any)
