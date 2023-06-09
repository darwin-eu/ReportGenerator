
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportGenerator <img src="man/figures/logo.png" align="right" height="180" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The ReportGenerator R package creates automatic study reports from
DARWIN EU® research. It includes a Shiny app where the user can select a
specific type of study according to the Complete Catalogue of Standard
Data Analyses (D1.3.8.2), and what tables/figures to produce out of a
set menu of outputs.

## Installation

You can install the development version of ReportGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/ReportGenerator")
```

To test the package, generate first the mock data with
`generateMockData()`. It will create a “results” folder in the working
directory that contains IncidencePrevalence data in csv and zip format.

``` r
 generateMockData()
```

`reportGenerator()` takes as an input zip or csv files with results from
IncidencePrevalence. To launch the Shiny app just type in the console:

``` r
reportGenerator()
```
