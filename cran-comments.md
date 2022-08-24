## Submission
This is the first ever CRAN submission of `tidyCDISC`- a large, multifaceted shiny application designed using the `golem` framework.

The application can perform many typical pharma-industry tasks in the clinical reporting pipeline. As such, many tasks require much documentation (aka vignettes), especially considering our users are primarily SAS programmers who are tentative around the validation of R outputs. To counter act that, we've added ~11 vignettes for users (& developer on-boarding) which take ~3 minutes of compute time to build. These vignettes include lots of `GIF`s and `PNG`s, bloating the installed package size so we've chosen to put nearly the entire `vignettes/` folder in the `.Rbuildignore`. However, our `README` is comprehensive, with lots of links to the vignettes hosted on our `pkgdown` site [here](https://biogen-inc.github.io/tidyCDISC/).

## R CMD check results
0 errors | 0 warnings | 2 notes

The 2 `NOTE`s included:
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission

checking package dependencies ... NOTE
  Imports includes 25 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```

To address `NOTE` #2: `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. While preparing for CRAN submission, we've surgically paired down the dependency list from 35 packages to 25 that (I feel) are necessary. And those that remain are primarily "mainstream" packages, meaning they are maintained by reputable & well known authors & dev teams in the R community.


    
## Downstream dependencies
There are none.



