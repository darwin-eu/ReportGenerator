
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IncidencePrevalenceReport

<!-- badges: start -->
<!-- badges: end -->

Report generator of the estimates from the IncidencePrevalence package.

## Installation

You can install the development version of IncidencePrevalenceReport
like so:

``` r
install.packages("remotes")
remotes::install_github("cbarbozaerasmus/IncidencePrevalenceReport")
```

## Example

The function reportIncidencePrevalence takes a character string for
title and author. Then a tibble for incidence and prevalence from
IncidencePrevalence package.

``` r
library(IncidencePrevalenceReport)
library(IncidencePrevalence)

title <- "..."
author <- "..."
prevalence <- prevalence$prevalence_estimates
incidence <- incidence$incidence_estimates

## RUN

reportIncidencePrevalence(title,
                          author,
                          prevalence,
                          incidence)
```

It generates an HTML document in the reports folder.
