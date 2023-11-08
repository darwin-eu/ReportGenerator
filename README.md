
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportGenerator <img src="man/figures/logo.png" align="right" height="180" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Codecov test
coverage](https://codecov.io/github/darwin-eu-dev/ReportGenerator/branch/main/graph/badge.svg)](https://app.codecov.io/github/darwin-eu-dev/ReportGenerator?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/ReportGenerator)](https://CRAN.R-project.org/package=ReportGenerator)

<!-- badges: end -->

## Overview

`ReportGenerator` creates automatic study reports from DARWIN EU®
research. It is a Shiny app with an interactive menu where the user can
select figures and tables from the Complete Catalogue of Standard Data
Analyses.

## Installation

You can install the development version of ReportGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/ReportGenerator")
```

`reportGenerator()` takes as an input zip or csv files with results from
IncidencePrevalence. To launch the Shiny app just type in the console:

``` r
ReportGenerator::reportGenerator()
```

Alternatively, you can access to an [online version
here](https://data-dev.darwin-eu.org/content/46c367cd-6e29-4382-9d7d-aff8caaa582a/).

Load your own results from the
[IncidencePrevalence](https://darwin-eu.github.io/IncidencePrevalence/)
package. ReportGenerator accepts data from the latest version of
IncidencePrevalence and offers limited support for previous versions. To
test the package, the user can generate some mock data with
`ReportGenerator::generateMockData()`, which will create a “Results”
folder with data in zip format.

``` r
ReportGenerator::generateMockData()
```
