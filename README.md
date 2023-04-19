
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportGenerator

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Automatic report generator of the Darwin EU studies.

## Installation

You can install the development version of reportGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/reportGenerator")
```

To test the package, generate first the mock data with mockSampleCSV()

``` r

 generateMockData()
```

It generates an WORD document in the reports folder.

To launch the Shiny app use reportDashboard(). It automatically takes as
an input a zip folders with CSV files.

``` r

reportGenerator()
```
