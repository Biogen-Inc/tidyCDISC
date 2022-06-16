## Submission
This is the first ever CRAN submission of `tidyCDISC`- a large, multifaceted shiny application designed using the `golem` framework.

The application can perform many typical pharma-industry tasks in the clinical reporting pipeline. As such, many tasks require much documentation (aka vignettes), especially considering our users are primarily SAS programmers who are generally tentative around the validation of R outputs. We've added 11 vignettes for users (& on-boarding developers) which take ~3 minutes of compute time build and include lots of gifs and pngs, bloating the package size. Thus, I've chosen to put the entire `vignettes/` folder in the `.Rbuildignore`. However, our `README` is comprehensive, with lots of links to the vignettes in our `pkgdown` site [here](https://biogen-inc.github.io/tidyCDISC/).

## R CMD check results
0 errors | 0 warnings | 2 notes

The 2 `NOTE`s included:
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission

checking package dependencies ... NOTE
  Imports includes 32 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```

To address `NOTE` #2: `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. I've surgically paired the application down to only packages that are absolutely necessary prior to submission.


    
## Downstream dependencies
There are currently no downstream dependencies for this package



