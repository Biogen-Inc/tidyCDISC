## Submission
This is the first ever CRAN submission of `tidyCDISC`: a large, multifaceted application designed to perform many pharma-industry tasks. Many tasks require much documentation (vignettes). As such, the 11 developer and user vignettes takes up ~3 minutes of compute time during the build.

## R CMD check results
There were no ERRORs or WARNINGs, passing on macOS-latest (release), windows-latest (release), ubuntu-latest (devel / release / oldrel-1) using GitHub Actions [here](https://github.com/Biogen-Inc/tidyCDISC/pull/78).

There were 4 NOTEs:

* checking package dependencies ... NOTE

  Imports includes 35 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. I've surgically paired the application down to only packages that are absolutely necessary prior to submission.


* checking installed package size ... NOTE
  
  installed size is 466.5Mb.
  sub-directories of 1Mb or more:
    R      1.2Mb,
    doc  463.4Mb
    
  As noted here, `tidyCDISC` leverages thousands of lines of R code in > 50 shiny modules & R scripts in the `R/` folder. As such, so much code needs an proportionally large amount of documentation. All the modules/functions have been exhaustively documented in great detail using `roxygen2`, even when not exported. Similarly, the package includes 11 vignettes to on-board both developers and app users, bundled together and published on our [GitHub Pages site](https://biogen-inc.github.io/tidyCDISC/).
  
    
* checking dependencies in R code ... NOTE
  
  Namespace in Imports field not imported from: ‘knitr’
  All declared Imports should be used.
  
  `tidyCDISC` leverages `knitr` in the vignettes, and when not included in the Imports field, the package would error/fail using `devtools::build()`.
  
  
* checking R code for possible problems ... NOTE
  
  fnIndvExplVisits: no visible binding for '<<-' assignment to 'avals_by'
  
  (D:/a/tidyCDISC/tidyCDISC/check/tidyCDISC.Rcheck/00_pkg_src/tidyCDISC/R/mod_indvExpPatVisits_fct_plot.R:113)
  
  fnIndvExplVisits: no visible binding for '<<-' assignment to 'avals_by'
  
  (D:/a/tidyCDISC/tidyCDISC/check/tidyCDISC.Rcheck/00_pkg_src/tidyCDISC/R/mod_indvExpPatVisits_fct_plot.R:153)
    
    `tidyCDISC` used `<<-` to assign a value to an R object in a parent environment.
    
## Downstream dependencies
There are currently no downstream dependencies for this package



