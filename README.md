
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportGenerator <img src="man/figures/logo.png" align="right" height="180" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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
here](https://data-dev.darwin-eu.org/content/5f2cf055-690c-4a28-9567-a1986eaafe8e/).

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
