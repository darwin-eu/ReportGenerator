
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reportGenerator

<!-- badges: start -->
<!-- badges: end -->

Automatic report generator of the Darwin EU studies.

## Installation

You can install the development version of reportGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("cbarbozaerasmus/reportGenerator")
```

To test the package, generate first the mock data with mockSampleCSV()

``` r

 mockSampleCSV()
```

## Example

The function incidencePrevalenceReport takes a character string for
title, author and other variables.

``` r
##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report Generator Test"
authors <- c("Jane Roe",
             "John Roe",
             "Richard Roe")
authorsInstitution <- c("Erasmus MC",
                        "Oxford University",
                        "Erasmus MC")
abstractText <- "ABSTRACT: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."

incidencePrevalenceReport(title = title,
                          authors = authors,
                          authorsInstitution = authorsInstitution,
                          abstractText = abstractText,
                          byCondition = TRUE,
                          format = "word")
```

It generates an WORD document in the reports folder.

To launch the Shiny app use resultsDashboard(). It automatically takes
as an input the mock data, or the folders where the data is located.

``` r

resultsDashboard(importFolderDenominator = here("inst/csv/denominatorMockData"),
                            importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                            importFolderPrevalence = here("inst/csv/prevalenceMockResults"))
```
