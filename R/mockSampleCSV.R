#' This function uses data mockIncidencePrevalenceRef function to simulate three databases in CSV format to test the ReportGenerator
#' @param databaseName A vector that specifies the name of each database.
#' @export
#' @import dplyr rmarkdown here tidyr IncidencePrevalence
#' @importFrom utils head write.csv
#' @importFrom stats time
#' @return CSV files inside inst/csv
mockSampleCSV <- function(databaseName = c("Synthea",
                                           "IPCI",
                                           "CPRD")) {

cdm <- mockIncidencePrevalenceRef(sampleSize = 50000)

# databaseName <- c("Synthea", "IPCI", "CPRD")

for (i in databaseName) {
  print(i)

#   i <- "IPCI"

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
                                                  daysPriorHistory  = 180) %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/denominatorMockData"))) {

    subDir <- here("inst",
                   "csv",
                   "denominatorMockData")

    dir.create(file.path(subDir),
               recursive = TRUE)
  }

  write.csv(cdm$denominator,
            paste(here("inst/csv/denominatorMockData/denominatorMockData"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)



  incidenceEstimates <- estimateIncidence(
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
    verbose = FALSE
  )

  incidenceResults <- incidenceEstimates %>%
    left_join(settings(incidenceEstimates)) %>%
    # left_join(settings(cdm$denominator),
    #           by=c("analysis_id" = "cohort_definition_id")) %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/incidenceMockResults"))) {

    subDir <- here("inst",
                   "csv",
                   "incidenceMockResults")

    dir.create(file.path(subDir),
               recursive = TRUE)
  }

  write.csv(incidenceResults,
            paste(here("inst/csv/incidenceMockResults/incidenceMockResults"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

  # i <- "IPCI"

  prevalenceEstimates <- estimatePeriodPrevalence(
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
    verbose = FALSE
  )

  prevalenceResults <- prevalenceEstimates %>%
    left_join(settings(incidenceEstimates)) %>%
    # left_join(dpop$denominator_settings,
    #           by=c("denominator_id" = "cohort_definition_id")) %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/prevalenceMockResults"))) {

    subDir <- here("inst",
                   "csv",
                   "prevalenceMockResults")

    dir.create(file.path(subDir),
               recursive = TRUE)
  }

  write.csv(prevalenceResults,
            paste(here("inst/csv/prevalenceMockResults/prevalenceMockResults"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

}

}
