# Disable autoload, which is TRUE by default since shiny 1.5.0
# (see ?shiny::loadSupport, rstudio/shiny#2659, rstudio/shiny#2547)
#
# For a packaged application (a-la golem) this is not the desired behavior,
# especially when deployed to shinyapps.io via a top-level app.R
# (a-la golem::add_shinyappsio_file()).
# Autoload causes scripts under R/ to be sourced, which is unnecessary and
# potentially breaking if the code assumes objects imported from other packages
# through the NAMESPACE, like palettes defined using RColorBrewer
