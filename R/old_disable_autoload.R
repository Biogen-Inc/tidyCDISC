

# Disable loading files in alphabetical order
# https://shiny.rstudio.com/reference/shiny/1.5.0/loadSupport.html
#
# if you want to disable autoload, this file needs to be named
# "_disable_autoload.R" with an underscore prefix. I removed it 4/26/22
# in favor of using 'options(shiny.autoload.r=FALSE)' in run_dev.R and
# in app.R for deployment.