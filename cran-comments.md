## Submission
This is the first ever CRAN submission of `tidyCDISC`- a large, multifaceted application designed using the `golem` framework.

The application can perform many typical pharma-industry tasks in the clinical reporting pipeline. Many tasks require much documentation (aka vignettes), especially considering the users are primarily SAS programmers who are tentative around the validation of R outputs in general. As such, there are 11 vignettes (for both users & developer on-boarding) which take ~3 minutes of compute time during the build, so I've passed the argument "--no-build-vignettes" to `devtools::release()`. However, the vignettes are compiled into a nice `pkgdown` site [here](https://biogen-inc.github.io/tidyCDISC/), built using CI/CD (via GitHub Actions), so it's always up to date with each push/pull request.

## R CMD check results
There were no ERRORs or WARNINGs, passing on macOS-latest (release), windows-latest (release), ubuntu-latest (devel / release / oldrel-1) using GitHub Actions [here](https://github.com/Biogen-Inc/tidyCDISC/pull/78).

There were 3 NOTEs:

* checking package dependencies ... NOTE

  Imports includes 35 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. I've surgically paired the application down to only packages that are absolutely necessary prior to submission.


* checking installed package size ... NOTE
  
  installed size is 466.5Mb.
  sub-directories of 1Mb or more:
    doc  463.4Mb
    
  As noted here, `tidyCDISC` leverages thousands of lines of R code in > 50 shiny modules & R scripts in the `R/` folder. As such, so much code needs an proportionally large amount of documentation. All the modules/functions have been exhaustively documented in great detail using `roxygen2`, even when not exported. Similarly, the package includes 11 vignettes to on-board both developers and app users, bundled together and published on our [GitHub Pages site](https://biogen-inc.github.io/tidyCDISC/).
  
  
  
* checking R code for possible problems ... NOTE
  
  fnIndvExplVisits: no visible binding for '<<-' assignment to 'avals_by'
    
  `tidyCDISC` used `<<-` to assign a value to an R object in a parent environment.
    
## Downstream dependencies
There are currently no downstream dependencies for this package



