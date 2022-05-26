## R CMD check results
There were no ERRORs or WARNINGs, passing on macOS-latest (release), windows-latest (release), ubuntu-latest (devel / release / oldrel-1).

There was 4 NOTEs:

* checking package dependencies ... NOTE

  Imports includes 35 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  R6 is a build-time dependency.

* checking installed package size ... NOTE
  
  installed size is 466.5Mb.
  sub-directories of 1Mb or more:
    R      1.2Mb,
    doc  463.4Mb
    
* checking dependencies in R code ... NOTE
  
  Namespace in Imports field not imported from: ‘knitr’
  All declared Imports should be used.
  
  
* checking R code for possible problems ... NOTE
  
  fnIndvExplVisits: no visible binding for '<<-' assignment to ‘avals_by’
    
## Downstream dependencies
There are currently no downstream dependencies for this package
