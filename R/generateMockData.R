# Copyright 2023 DARWIN EU®
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' This function uses data mockIncidencePrevalenceRef function to simulate three databases in CSV format to test the ReportGenerator
#' @param databaseName A vector that specifies the name of each database.
#' @export
#' @import dplyr rmarkdown here tidyr IncidencePrevalence
#' @importFrom utils head write.csv
#' @importFrom stats time
#' @importFrom zip zip
#' @return CSV files inside inst/csv duckdb
generateMockData <- function(databaseName = "IPCI") {

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

  studyResults <- gatherIncidencePrevalenceResults(cdm = cdm,
                                                   resultList = list(incidence,
                                                                     prevalencePoint,
                                                                     prevalencePeriod))

  # studyResults[1] # prevalence_estimates_
  # studyResults[2] # prevalence_attrition_
  # studyResults[3] # incidence_estimates_
  # studyResults[4] # incidence_attrition_
  # studyResults[5] # $cdm_snapshot_

  ## Writing prevalence data into csv

  if (!file.exists(here("prevalenceResults"))) {

    subDir <- here("prevalenceResults")

    dir.create(file.path(subDir),
               recursive = TRUE)
  }

  studyResults$prevalence_estimates <- studyResults$prevalence_estimates %>% mutate(database_name = i)

  write.csv(studyResults$prevalence_estimates,
            paste(here("prevalenceResults"),
                  "/prevalence_mock_estimates_",
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

  ## Writing incidence data into csv

  if (!file.exists(here("incidenceResults"))) {

    subDir <- here("incidenceResults")

    dir.create(file.path(subDir),
               recursive = TRUE)

  }

  studyResults$incidence_estimates <- studyResults$incidence_estimates %>% mutate(database_name = i)

  write.csv(studyResults$incidence_estimates,
            paste(here("incidenceResults"),
                  "/incidence_mock_estimates_",
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)


  ## Exporting whole results into zip folder

  if (!file.exists(here("results_zip"))) {

    subDir <- here("results_zip")

    dir.create(file.path(subDir),
               recursive = TRUE)

  }

  result <- studyResults

  zipName <-  paste0("mock_results", "_", i)

  outputFolder <- here::here("results_zip")

  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertTRUE(
    inherits(result, "IncidencePrevalenceGatheredResult"),
    add = errorMessage
  )
  checkmate::assertCharacter(zipName, len = 1,
                             add = errorMessage)
  checkmate::assertDirectoryExists(outputFolder,
                                   add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  tempDir <- tempdir()

  # write results to disk
  lapply(names(result), FUN = function(checkResultName) {
    checkResult <- result[[checkResultName]]
    utils::write.csv(checkResult,
                     file = file.path(
                       tempDir,
                       paste0(attr(result, "cdm_name"), "_",
                              checkResultName, "_",
                              format(Sys.Date(), format="%Y%m%d"),
                              ".csv")),
                     row.names = FALSE
    )
  })

  zip::zip(zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
           files = list.files(tempDir, full.names = TRUE),
           mode = "cherry-pick")


  lapply(list.files(tempDir, full.names = TRUE), file.remove)

}

}
utils::globalVariables(c("generateDenominatorCohortSet",
                         "estimateIncidence",
                         "estimatePointPrevalence",
                         "estimatePeriodPrevalence",
                         "gatherIncidencePrevalenceResults",
                         "exportIncidencePrevalenceResults"))

