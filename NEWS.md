# tidyCDISC 0.2.1 (CRAN Release)

### General
* Fixed bug accessing files in `app/www/` (#166)
* Improved documentation
    * Added vignette announcing release of `v0.2.1`
    * Added highly visible links to Blog, YouTube, and NEWS file

### Table Generator
* Made the html output in the app look slightly more "CSR-like" using minor formatting tricks (#181)
* Arranged visits in chronological order for dropdown list found in Stat blocks  (#154)
* Fixed bug where wrong options were being passed on some lab tables (#169)
* Fixed bug where draggable blocks were not working without inclusion of datasets with ATPT (#173)

### Population Explorer
* Fixed bug where selected filters were not being applied when 'Apply Filters' was toggled on (#175)
* Fixed bug where scatter plot wouldn't display by categorical variable with `NA` values (#192)

# tidyCDISC 0.2.0 (CRAN Release)

* Added RTF export option for download from the Table Generator.
* Cleaned up JS to 'standard analyses' drop down.
* Created "ALL" as dropdown option for 'standard analysis' Table 41 instead of auto populating all available weeks in the drop zone.
* added protocol / study id in prominent location on each tab
* designed `ATPT` variable integration in the Table Generator (when present in data)
* added `OS Health` & `riskmetric` badges to the `README`
* engineered more user-friendly function to create the table generator output from the block data 
* ensured Table Generator output automatically updates the table name when a 'standard analysis' is selected
* Fixed some Population Explorer filtering issues
* Added an `app.R` file so that shiny app can be ran directly from `shiny::runGitHub()` or `runUrl()`


# tidyCDISC 0.1.1

* automated a footnote with **Source** and **Run Date** in table generation
* allowed custom user-defined footnotes
* allowed the selection of 'All' when a time/visit-based statistic is chosen in the table generator
* cleaned up downloadable R Script to replicate table generator output
* fixed bug where `data_from` was erroneously listed as a grouping option in the population explorer


# tidyCDISC 0.1.0 (CRAN Release)
* cleaning up `devtools::check()` & preparing for CRAN release.
* unfortunately, had to revert back to importing `tippy 0.1.0` since it is the latest available version on CRAN and CRAN pkgs cannot depend on package versions under development.
* created a new toggle on the `Data` tab that allows users to select which CDISC pilot study data sources they wish to use in the app.
* Minor bug improvements

# tidyCDISC 0.0.4 
* cleaning up `devtools::check()` & preparing for CRAN release.

# tidyCDISC 0.0.3

* For `tippy` package, added lower bound on to version `1.0.0` since it includes more user friendly options.

# tidyCDISC 0.0.2

This release (PR #65) performs a lot of functions, testing on many non-pilot ADSL files to ensure app's robustness beyond just the CDISC pilot data:

* closes #64: Agg class objects lost "droppability" when only ADSL uploaded
* gets rid of annoying font awesome message about verifying an icon exists
* updates gt code that was deprecated
* Made sure 'PARAM' exists in the data when trying to label a block
* got rid of unnecessary argument in col_for_list() and col_for_list_expr()
* added some code that helped the FREQ block produce the correct results when ran locally (via a reproducibility script)
* a little code clean up for readability

# tidyCDISC 0.0.1

This release adds meat to the previous skeleton version of `tidyCDISC`. The initial development release was intended to be a "bare bones" platform that could serve as a foundation to build a great clinical data exploratory tool. Version `0.0.1` enriches the user experience by adding useful & commonly needed features for analysis. For more on this release, see [the announcement](https://biogen-inc.github.io/tidyCDISC/articles/Blog.html#announcing-tidycdisc-0-0-1-1) on our blog.


* Major Features added, by tab:
  * **Table Generator**
    * New "STAT Blocks", such as `Y FREQ`, `MAX FREQ`, `NON-MISSING`, and a pair of `NESTED FREQ` blocks
    * Interact with `ADAE`
    * 17 new 'standard analysis' tables in the `ADAE` and `ADLB` space.
    * An R Script download-er to reproduce analysis performed in the app
  * **Population Explorer**
    * New Chart types
      * line plot - means over time
      * Heat map - endpoint correlations
      * Kaplan Meier Curve (when `TTE` class data uploaded)
* Upgraded `pkgdown` site documentation to be more robust and hassle-free for developers
* Depends on updated version of IDEAFilter (>= 0.1.0.9000)
* Squashed bugs and other minor improvements

# tidyCDISC 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
