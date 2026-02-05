# Submission of `v0.2.2`
This is a new CRAN release, it's predecessor being `v0.2.1`. This version fixes a small R CMD Check bug that was introduced with the release of `dplyr v1.2.0`, set to release 2026-01-31. It also includes minor improvements to the Kaplan Meier Plot modules of the Shiny app.

## R CMD Check
0 errors | 0 warnings | 1 note

The 1 `NOTE`s included:
```
checking package dependencies ... NOTE
  Imports includes 37 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.
```

To address the note, here was my justification for this note in the previous release:

> `tidyCDISC` is (nearly) a "one stop shop" application for clinical reporting in the pharmaceutical space. As such, it leverages many modules to perform many tasks. Many tasks require many packages. While preparing for CRAN submission, we've surgically paired down the dependency list to 37 packages that (I feel) are necessary. These are primarily "mainstream" packages, meaning they are maintained by reputable & well known authors & dev teams in the R community.

### Test Environments

* RHub Github action performing linux, macOS, and Windows R CMD Check
* Local windows `devtools::check()`

## Downstream dependencies
There are none.



