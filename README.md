
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyCDISC <a href='https://Biogen-Inc.github.io/tidyCDISC/'><img src="man/figures/hex-tidyCDISC-170h.png" align="right"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Biogen-Inc/tidyCDISC/workflows/R-CMD-check/badge.svg)](https://github.com/Biogen-Inc/tidyCDISC/actions)
[![OS
Health](https://img.shields.io/badge/OS%20Health-71-yellowgreen)](https://openpharma.github.io/GithubMetrics/)
[![riskmetric](https://img.shields.io/badge/riskmetric-0.50-orange)](https://pharmar.github.io/riskmetric/)
<!-- badges: end -->

`tidyCDISC` is a shiny app to easily create custom tables and figures
from ADaM-ish data sets.

<br>

<center>
<a href="https://rinpharma.shinyapps.io/tidyCDISC/">
<img src="man/figures/demo_tidyCDISC_button_lite2dark.png" alt="Demo full tidyCDISC app" width="25%">
</a>
</center>

## Purpose

One of `tidyCDISC`’s goals is to develop clinical tables that meet table
standards leveraged for submission filings, called “standard analyses”.
However, this is secondary to the app’s primary purpose: providing rich
exploratory capabilities for clinical studies. High-level features of
the app allow users to produce customized tables using a point-and-click
interface, examine trends in patient populations with dynamic figures,
and supply visualizations that narrow in on single patient profile.

The beauty of this application is that the user doesn’t have to write a
lick of code to gather abundant insights from the study data, so it aims
to serve a large population of clinical personnel with varying levels of
programming experience. For example:

- A **clinical head**, with presumably no programming skills but the
  most domain expertise, can explore results without asking a
  statistician or programmer to build tables & figures.

- A **statistician** can use the application to make tables/figures
  instantly, cutting down on statistical programming requests for excess
  tables that aren’t required, but just “nice to see”.

<div class="floating">

<img src="man/figures/pct_95_cropped.jpg" width="25%" style="float:right; padding:10px" />

- A **statistical programmer** can use `tidyCDISC` to perform
  preliminary QC programming prior to writing code in a validated
  process. Users who’ve leverage `tidyCDISC` for routine trial analysis
  tend to report **significant time savings, about 95%**, when
  performing programming duties.

</div>

For a high-level overview of the app with 10-minute demo, please review
the following conference presentation on `tidyCDISC`at **R/Medicine
2020**:

<br>

<center>

[![tidyCDISC @
R/Medicine](man/figures/tidyCDISC_RMedicine_thumbnail.png)](https://youtu.be/QeHSjw-vU3U?t=103)

</center>

<br>

## Scope

As previously mentioned, `tidyCDISC` can only accept data sets that
conform to CDISC ADaM standards with some minor flexibility (see [upload
requirements](https://Biogen-Inc.github.io/tidyCDISC/articles/x00_Data_Upload.html)
for more details). At this time, the app only accepts sas7bdat files.

If you’re looking to regularly generate R code for tables, the
`tidyCDISC` app has a built-in export feature that downloads an R script
to reproduce any analysis performed in the app.

<br>

## Usage

You can start using the demo version of the app here:
[tidyCDISC](https://rinpharma.shinyapps.io/tidyCDISC/). Note the demo
version disables the **Data Upload** feature and instead uses the CDISC
pilot data. If you’d like to upload your own study data, we recommend
installing `tidyCDISC` from CRAN (instructions below) to run the app
locally or deploy it in your preferred environment. Please review the
“[Get
Started](https://Biogen-Inc.github.io/tidyCDISC/articles/tidyCDISC.html)”
guide to follow an example use case with the app. However, to optimize
one’s use of `tidyCDISC`, we highly recommend reading the following
articles that take a deeper look into the topics presented in the “Get
Started” tutorial:

- [00 Data
  Upload](https://Biogen-Inc.github.io/tidyCDISC/articles/x00_Data_Upload.html)

- [01 Table
  Generator](https://Biogen-Inc.github.io/tidyCDISC/articles/x01_Table_Generator.html)

- [02 Population
  Explorer](https://Biogen-Inc.github.io/tidyCDISC/articles/x02_Pop_Exp.html)

- [03 Individual
  Explorer](https://Biogen-Inc.github.io/tidyCDISC/articles/x03_Indv_Expl.html)

- [04
  Filtering](https://Biogen-Inc.github.io/tidyCDISC/articles/x04_Filtering.html)

We’re confident the `tidyCDISC` application can save you time. If there
is some use case that `tidyCDISC` can’t solve, we want to know about it.
Please send the
[developers](https://github.com/Biogen-Inc/tidyCDISC/issues/new) a
message with your question or request!

<br>

## Install the `tidyCDISC` R package

As a reminder, you can start using the demo version of the app here:
[tidyCDISC](https://rinpharma.shinyapps.io/tidyCDISC/) without any
installation required. However, if you choose to upload your own study
data OR export & run R code from the Table Generator, you will need the
`tidyCDISC` package installed on your machine locally. Execute the
following code to install the package to your local machine:

``` r
# Install from CRAN
install.packages("tidyCDISC")

# Or install the latest dev version
remotes::install_github("Biogen-Inc/tidyCDISC")
```

With a simple `library(tidyCDISC)` you can access all the exported
functions from `tidyCDISC` that help users reproduce analysis performed
in the app. Or, you can run the application locally (or deploy it in an
`app.R` file) using:

``` r
# Run the application 
tidyCDISC::run_app()
```

<br>

<br>

Happy exploring!

<br>

<br>
