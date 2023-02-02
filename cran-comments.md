## Submission of `v0.2.0`
This is a new CRAN release, it's predecessor being `v0.1.0` which currently has a [failing R Check on CRAN](https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-debian-clang/tidyCDISC-00check.html). This version fixes that (minor) bug.

### R CMD Check
0 errors | 0 warnings | 1 note

The 1 `NOTE`s included:
```
checking CRAN incoming feasibility ... NOTE

checking package dependencies ... NOTE
  Imports includes 25 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```
#### Test Environments

* Github action performing linux check with old, current, and devel release of R
* Github action performing windows and macOS check with current R release
* Local windows `devtools::check()`
* RHub Check

### Downstream dependencies
There are none.



