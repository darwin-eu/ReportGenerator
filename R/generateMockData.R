#' This function uses data mockIncidencePrevalenceRef function to simulate three databases in CSV format to test the ReportGenerator
#' @param databaseName A vector that specifies the name of each database.
#' @export
#' @import dplyr rmarkdown here tidyr IncidencePrevalence
#' @importFrom utils head write.csv
#' @importFrom stats time
#' @return CSV files inside inst/csv duckdb
generateMockData <- function(databaseName = c("SYNTHEA",
                                              "IPCI",
                                              "CPRD",
                                              "SIDIAP")) {

cdm <- mockIncidencePrevalenceRef(sampleSize = 50000)

# databaseName <- c("Synthea", "IPCI", "CPRD")

for (i in databaseName) {
  print(i)

  # For testing
  # i <- "IPCI"

  ## Obtaining data

  # Denominator data

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm,
                                                  startDate  = as.Date("2008-01-01"),
                                                  endDate  = as.Date("2018-01-01"),
                                                  ageGroup  = list(c(20, 29),
                                                                   c(30, 39),
                                                                   c(40, 49),
                                                                   c(50, 59),
                                                                   c(60, 69),
                                                                   c(0,99)),
                                                  sex  = c("Female", "Male", "Both"),
                                                  daysPriorHistory  = 180) %>% mutate(database_name = i)
  # Incidence data

  incidence <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    denominatorCohortId  = as.integer(c("1", "2", "3", "4", "5",
                                        "6", "7", "8", "9", "10",
                                        "11", "12", "13", "14",
                                        "15", "16", "17", "18")),
    outcomeCohortId = as.integer(1),
    interval = "years",
    completeDatabaseIntervals = TRUE,
    outcomeWashout = 180,
    repeatedEvents = FALSE,
    minCellCount = 5,
    verbose = TRUE
  )

  # Prevalence data, both point and period

  prevalencePoint <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    denominatorCohortId = as.integer(c("1", "2", "3", "4", "5",
                                       "6", "7", "8", "9", "10",
                                       "11", "12", "13", "14",
                                       "15", "16", "17", "18")),
    outcomeCohortId   = as.integer(1),
    outcomeLookbackDays = 0,
    interval = "years",
    timePoint = "start",
    minCellCount = 5,
    verbose = TRUE
  )

  prevalencePeriod <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    denominatorCohortId = as.integer(c("1", "2", "3", "4", "5",
                                       "6", "7", "8", "9", "10",
                                       "11", "12", "13", "14",
                                       "15", "16", "17", "18")),
    outcomeCohortId   = as.integer(1),
    outcomeLookbackDays = 0,
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = FALSE,
    minCellCount = 0,
    verbose = TRUE
  )

  # Results

  studyResults <- gatherIncidencePrevalenceResults(cdm=cdm,
                                                   resultList=list(incidence,
                                                                   prevalencePoint,
                                                                   prevalencePeriod))

  # studyResults[1] # prevalence_estimates_
  # studyResults[2] # prevalence_attrition_
  # studyResults[3] # incidence_estimates_
  # studyResults[4] # incidence_attrition_
  # studyResults[5] # $cdm_snapshot_

  ## Writing prevalence data into csv

  if (!file.exists(here("inst/csv/mock/prevalenceResults"))) {

    subDir <- here("inst",
                   "csv",
                   "mock",
                   "prevalenceResults")

    dir.create(file.path(subDir),
               recursive = TRUE)
  }

  studyResults$prevalence_estimates <- studyResults$prevalence_estimates %>% mutate(database_name = i)

  write.csv(studyResults$prevalence_estimates,
            paste(here("inst/csv/mock/prevalenceResults/"),
                  "prevalence_mock_estimates_",
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

  ## Writing incidence data into csv

  if (!file.exists(here("inst/csv/mock/incidenceResults"))) {

    subDir <- here("inst",
                   "csv",
                   "mock",
                   "incidenceResults")

    dir.create(file.path(subDir),
               recursive = TRUE)

  }

  studyResults$incidence_estimates <- studyResults$incidence_estimates %>% mutate(database_name = i)

  write.csv(studyResults$incidence_estimates,
            paste(here("inst/csv/mock/incidenceResults/"),
                  "incidence_mock_estimates_",
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

  ## Exporting whole results into zip folder

  if (!file.exists(here("inst/csv/mock/results_zip"))) {

    subDir <- here("inst",
                   "csv",
                   "mock",
                   "results_zip")

    dir.create(file.path(subDir),
               recursive = TRUE)

  }

  exportIncidencePrevalenceResults(result = studyResults,
                                   zipName = paste0("mock_results",
                                                    "_",
                                                    i),
                                   outputFolder = here::here("inst/csv/mock/results_zip"))

  # To write in csv the denominator data in csv ---------------

  # if (!file.exists(here("inst/csv/denominatorMockData"))) {
  #
  #   subDir <- here("inst",
  #                  "csv",
  #                  "denominatorMockData")
  #
  #   dir.create(file.path(subDir),
  #              recursive = TRUE)
  # }
  #
  # write.csv(cdm$denominator,
  #           paste(here("inst/csv/denominatorMockData/denominatorMockData"),
  #                 i,
  #                 ".csv",
  #                 sep = ""),
  #           row.names = FALSE)

  # Left join of incidence with settings() ---------------

  # incidenceEstimates <- incidence %>%
  #   left_join(settings(incidenceEstimates)) %>%
  #   # left_join(settings(cdm$denominator),
  #   #           by=c("analysis_id" = "cohort_definition_id")) %>%
  #   mutate(database_name = i)

  # Left join of prevalence with settings() ---------------

  # prevalenceResultsSettings <- prevalenceEstimates %>%
  #   left_join(settings(prevalenceEstimates)) %>%
  #   # left_join(dpop$denominator_settings,
  #   #           by=c("denominator_id" = "cohort_definition_id")) %>%
  #   mutate(database_name = i)


}

}
utils::globalVariables(c("generateDenominatorCohortSet",
                         "estimateIncidence",
                         "estimatePointPrevalence",
                         "estimatePeriodPrevalence",
                         "gatherIncidencePrevalenceResults",
                         "exportIncidencePrevalenceResults"))

