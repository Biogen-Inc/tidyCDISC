## Re-submission 2022-08-27
This is a re-submission. In this version I have:

* Explained acronyms in the description text such as 'ADaM' and 'CDISC', providing URL links.
  
* Used `TRUE` & `FALSE` (instead of `T` & `F`) where needed (man/app_heatmap.Rd).

* Added \value to .Rd files that were exported, explaining the results. Also decided to not export a few methods.

* Added small executable examples to .Rd files of exported functions to illustrate use.

* Suppressed some unneeded `print()`/`cat()` messages.

* Added immediate reverting to old options using `on.exit()` after a function changes users settings with `options()` call. Note there is still one instance where we called `options()` without an immediate call to `on.exit()` but it is in a downloadable R script that never get's executed in the application. The user will have to download the R script and run it in an interactive R session.

* Switched from using `installed.packages()` to `find.package()` as the prior can be slow on windows or some network-mounted file systems, especially when thousands of packages are installed.

The tidyCDISC dev team chose to leave one piece of code AS-IS after receiving comments from the CRAN team. Specifically:
```
Please do not install packages in your functions, examples or vignette.
This can make the functions,examples and cran-check very slow. ->
R/mod_tableGen.R
```

We left one instance of `install.packages()` AS-IS because that code is solely included in a function that produces a downloadable R script. Thus, the code never get's executed in the application - the user will have to first download the R script from the application (in their browser) and pull it open in an interactive R session in order to run. So it should never bog down functions, examples, and CRAN-checks. Our app user base is predominantly SAS-programmers with limited R experience, so our goal for the R script to reproduce outputs delivered in the app with 100% automation & no manual intervention.
  
#### R CMD Check
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

## Initial Submission 2022-08-05
This is the first ever CRAN submission of `tidyCDISC`- a large, multifaceted shiny application designed using the `golem` framework.

The application can perform many typical pharma-industry tasks in the clinical reporting pipeline. As such, many tasks require much documentation (aka vignettes), especially considering our users are primarily SAS programmers who are tentative around the validation of R outputs. To counter act that, we've added ~11 vignettes for users (& developer on-boarding) which take ~3 minutes of compute time to build. These vignettes include lots of `GIF`s and `PNG`s, bloating the installed package size so we've chosen to put nearly the entire `vignettes/` folder in the `.Rbuildignore`. However, our `README` is comprehensive, with lots of links to the vignettes hosted on our `pkgdown` site [here](https://biogen-inc.github.io/tidyCDISC/).

#### R CMD check results
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


    
#### Downstream dependencies
There are none.



