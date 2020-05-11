source('R/detectStandard.R')
source('R/evaluateStandard.R')
source('R/getSettingsMetadata.R')
source('R/hasColumn.R')
source('R/hasField.R')
source('R/js_strings.R')
source('R/css.R')
source('R/fn_filters_in_english.R')
source('R/build_events_df.R')

source('modules/dataUpload.R')
source('modules/dataUploadUI.R')

# Table Generator
source('modules/tableGenerator.R')
source('modules/tableGeneratorUI.R')

# Block UI and helper functions
source('R/blocks.R')
source('R/CapStr.R')
source('R/allowed_operators.R')
source("R/IDEA_mean_summary.R")

# Block calculations
source("R/IDEA_mean.R")
source("R/IDEA_chg.R")
source("R/IDEA_freq.R")
source("R/IDEA_anova.R")

# select Data
source('modules/selectData.R')
source('modules/selectDataUI.R')

# population explorer
source('modules/PopuExplor.R')
source('modules/PopuExplorUI.R')

# functions written for population explorer
source('R/fnboxplot.R')
source('R/fnscatter.R')
source('R/fnsummtab.R')
source('R/fnoverplot.R')
source('R/fncorrmat.R')

# child modules called by PopuExplor
source('modules/PopuExpl1Scat.R')
source('modules/PopuExpl2Spag.R')
source('modules/PopuExpl3Boxp.R')
source('modules/PopuExpl4Heat.R')
source('modules/PopuExpl5Hist.R')

# individual explorer
source('modules/IndvExplorUI.R')
# server code broken into four sections
source('modules/IndvExpl1Initial.R')
source('modules/IndvExpl2SelPatno.R')
source('modules/IndvExpl3CheckGroup.R')
source('modules/IndvExpl4ChartPlotly.R')