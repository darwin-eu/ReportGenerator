# ReportGenerator 1.22.0

* Updated to use the latest version of IncidencePrevalence (v1.0.0), CohortCharacteristics (v0.4.0) and CohortSurvival (v0.6.2).

# ReportGenerator 1.21.0

* Bugs fixes.

# ReportGenerator 1.20.0

* Bugs fixes. 

* Improvements in preview sections.

# ReportGenerator 1.19.2

* Update for new versions of analytical packages.

* Fixed bug related to other_result.

# ReportGenerator 1.19.0

* Improved integration with summarisedResult() format for IncidencePrevalence, CohortCharacteristics and CohortSurvival.

# ReportGenerator 1.18.0

* omopgenerics integrations for IncidencePrevalence and data extraction. 

# ReportGenerator 1.17.3

* Update to renv.lock

# ReportGenerator 1.17.1

* Fixed tests.

# ReportGenerator 1.17.0

* Binds results using result_id. 

# ReportGenerator 1.16.0

* Bug fix for test mock utils.

# ReportGenerator 1.15.0

* Update to use CohortCharacteristics

* Integration with omopgenerics

* Shiny app generation

# ReportGenerator 1.14.0

* User can drop zip folder with results from multiple analytic packages.

* PatientProfiles and CohortSurvival added to main menu. 

# ReportGenerator 1.13.0

* Updated and tested IncidencePrevalence 0.6.0

# ReportGenerator 1.12.0

* Improved tests.

# ReportGenerator 1.11.0

* CRAN release test.

* Added tests and examples.

# ReportGenerator 1.10.0

* Preview persist even when changing different tables and figures.

# ReportGenerator 1.9.0

* Users can select the version of IncidencePrevalence data that ReportGenerator accepts.

* Yaml file for items configuration. 

* Github website. 

* Improvements to function variablesConfigYaml() to add new versions of results to yaml file. 

# ReportGenerator 1.8.2

* New download button. 

# ReportGenerator 1.8.1

* Updated to the last CRAN version of IncidencePrevalence. 

# ReportGenerator 1.8.0

* Preview items can be locked and added to the Word report. 

* Fixed bug that prevented loading one database. 

* Corrections to the plot filters. 

# ReportGenerator 1.7.1

* Fixes to the filters and facets for figures.

# ReportGenerator 1.7.0

* Fixed bug "unknown file" when generating Word report. 

* Added incidence and prevalence figures by sex and age. 

* User can select data for table 1 according to the analysis_id

# ReportGenerator 1.5.1

* Automatic text corrections

* Fixed error when loading files into temp dir

# ReportGenerator 1.5.0

* Plotting now uses functions from IncidencePrevalence package. 

* Improved code efficiency using reactiveValues to gather data. 

* Fixes to the mock data generator; now it generates mock populations of different sizes. 

# ReportGenerator 1.4.0

* The file input now accepts multiple zip folders, i.e. data from several databases.

* Drag and drop menu is back. 

* Bug related to the type of graph that prevented the generation of the report was solved. 

# ReportGenerator 1.3.0

* Tables now use the package Huxtable.

* Table 1 now is available on preview and to print it in the report. 

* Data is cleaned now every time a new zip is uploaded to the file input. 

* Fixes to the reset. 

# ReportGenerator 1.2.0

* Figures for incidence now have a drop down menu to select the facet, either by database or outcome. 

# ReportGenerator 1.1.0

* Check out the new logo!

* Figure 1 now has a drop down menu tu select either Option A or Option B (Facet by Outcome, Facet by Database).

# ReportGenerator 1.0.1

* Improvements to `generateMockData()`

# ReportGenerator 1.0.0

-   Working version to use in a Darwin sprint.

-   Main functionality of the package can be accessed with `reportGenerator()`, which launches the Shiny app.

-   Tables and Figures that show results for incidence from the `IncidencePrevalence` package.
