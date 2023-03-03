# Submission of `v0.2.1`
This is a new CRAN release, it's predecessor being `v0.2.0`. This version fixes some bugs and introduces some minor new features.

## R CMD Check
0 errors | 0 warnings | 1 note

The 1 `NOTE`s included:
```
checking package dependencies ... NOTE
  Imports includes 25 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```

To address the note: there have been no additional dependencies introduced to the package since the last CRAN release (`v0.2.0`). For reference, here was my justification for this note previously:

> `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. While preparing for CRAN submission, we've surgically paired down the dependency list from 35 packages to 25 that (I feel) are necessary. And those that remain are primarily "mainstream" packages, meaning they are maintained by reputable & well known authors & dev teams in the R community.

### Test Environments

* Github action performing linux check with old, current, and devel release of R
* Github action performing windows and macOS check with current R release
* Local windows `devtools::check()`
* RHub Check

## Downstream dependencies
There are none.



