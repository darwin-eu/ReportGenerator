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

#' Creates a mock data set for ReportGenerator. The output is a zip folder for each database.
#'
#' `generateMockData()` uses functions such as [IncidencePrevalence::mockIncidencePrevalenceRef()] to create csv files to test ReportGenerator.
#'
#' @param databaseName A vector with the name in characters of each database.
#' @param simulatePopulation TRUE or FALSE to simulate different population sizes. TRUE is default.
#' @param outputPath A character vector of the path to export mock data.
#' @param internal interal usage
#'
#' @import dplyr tidyr IncidencePrevalence duckdb checkmate CDMConnector PatientProfiles CohortSurvival CohortCharacteristics CodelistGenerator
#' @importFrom IncidencePrevalence generateDenominatorCohortSet estimateIncidence attrition estimatePointPrevalence estimatePeriodPrevalence
#' @importFrom utils head write.csv packageVersion
#' @importFrom stats time
#' @importFrom zip zip
#' @return csv files
#' @export
generateMockData <- function(databaseName = c("CHUBX",
                                              "CPRD_GOLD",
                                              "IMASIS",
                                              "IPCI",
                                              "SIDIAP"),
                             simulatePopulation = TRUE,
                             outputPath = testthat::test_path("studies", "zip"),
                             internal = TRUE) {

  if (!dir.exists(outputPath)) {
    dir.create(outputPath, recursive = TRUE)
  }

  result <- list()

  for (dbName in databaseName) {
    # dbName <- "CPRD_GOLD"
    if (simulatePopulation == TRUE) {
        sampleSize <- case_when(
          dbName == "CHUBX" ~ 21523,
          dbName == "CPRD_GOLD" ~ 15662,
          dbName == "IMASIS" ~ 10147,
          dbName == "IPCI" ~ 26745,
          dbName == "SIDIAP" ~ 82653)
        } else {
          sampleSize <- 50000
        }


    # Generate data

    incidencePrevalenceData <- getIncidencePrevalenceMock(sampleSize = sampleSize)
    treatmentPathwaysData <- getTreatmentPathways()
    summarised_characteristics <- getCharacteristicsResult()
    summarised_large_scale_characteristics <- getLargeScaleCharacteristicsResult()
    cohortSurvivalData <- getCohortSurvival()

    # Gather data

    dataList <- list("IncidencePrevalence" = list("incidence_estimates" = incidencePrevalenceData$incidence_estimates,
                                                  "prevalence_estimates" = incidencePrevalenceData$prevalence_estimates,
                                                  "incidence_attrition" = incidencePrevalenceData$incidence_attrition,
                                                  "prevalence_point_attrition" = incidencePrevalenceData$prevalence_point_attrition,
                                                  "prevalence_period_attrition" = incidencePrevalenceData$prevalence_period_attrition),
                     "CohortCharacteristics" = list("summarised_characteristics" = summarised_characteristics,
                                                    "summarised_large_scale_characteristics" = summarised_large_scale_characteristics),
                     "CohortSurvival" = list("single_event" = cohortSurvivalData$single_event,
                                             "competing_risk" = cohortSurvivalData$competing_risk),
                     "TreatmentPatterns" = list("treatmentPathways" = treatmentPathwaysData$treatmentPathways,
                                                "metadata" = treatmentPathwaysData$metadata,
                                                "summaryStatsTherapyDuration" = treatmentPathwaysData$summaryStatsTherapyDuration)
    )

    # Insert database name

    resultListIP <- lapply(dataList$IncidencePrevalence, mutate, cdm_name = dbName)
    resultListCC <- lapply(dataList$CohortCharacteristics, mutate, cdm_name = dbName)
    resultListCS <- lapply(dataList$CohortSurvival, mutate, cdm_name = dbName)
    resultListTP <- lapply(dataList$TreatmentPatterns, mutate, cdm_name = dbName)

    dataList[["IncidencePrevalence"]][["incidence_estimates"]] <- resultListIP$incidence_estimates
    dataList[["IncidencePrevalence"]][["prevalence_estimates"]] <- resultListIP$prevalence_estimates
    dataList[["IncidencePrevalence"]][["incidence_attrition"]] <- resultListIP$incidence_attrition
    dataList[["IncidencePrevalence"]][["prevalence_point_attrition"]] <- resultListIP$prevalence_point_attrition
    dataList[["IncidencePrevalence"]][["prevalence_period_attrition"]] <- resultListIP$prevalence_period_attrition
    dataList[["CohortCharacteristics"]][["summarised_characteristics"]] <- resultListCC$summarised_characteristics
    dataList[["CohortCharacteristics"]][["summarised_large_scale_characteristics"]] <- resultListCC$summarised_large_scale_characteristics
    dataList[["CohortSurvival"]][["single_event"]] <- resultListCS$single_event
    dataList[["CohortSurvival"]][["competing_risk"]] <- resultListCS$competing_risk
    dataList[["TreatmentPatterns"]][["treatmentPathways"]] <- resultListTP$treatmentPathways
    dataList[["TreatmentPatterns"]][["metadata"]] <- resultListTP$metadata
    dataList[["TreatmentPatterns"]][["summaryStatsTherapyDuration"]] <- resultListTP$summaryStatsTherapyDuration

    exportResults(resultList = dataList,
                  zipName = paste0("mock_data_", dbName),
                  outputFolder = outputPath)

    if (internal) {
      if (identical(result, list())) {
        result <- dataList
      } else {
        result[["IncidencePrevalence"]][["incidence_estimates"]] <- dplyr::bind_rows(result[["IncidencePrevalence"]][["incidence_estimates"]], dataList[["IncidencePrevalence"]][["incidence_estimates"]])
        result[["IncidencePrevalence"]][["prevalence_estimates"]] <- dplyr::bind_rows(result[["IncidencePrevalence"]][["prevalence_estimates"]], dataList[["IncidencePrevalence"]][["prevalence_estimates"]])
        result[["IncidencePrevalence"]][["incidence_attrition"]] <- dplyr::bind_rows(result[["IncidencePrevalence"]][["incidence_attrition"]], dataList[["IncidencePrevalence"]][["incidence_attrition"]])
        result[["IncidencePrevalence"]][["prevalence_attrition"]] <- dplyr::bind_rows(result[["IncidencePrevalence"]][["prevalence_attrition"]], dataList[["IncidencePrevalence"]][["prevalence_attrition"]])
        result[["CohortCharacteristics"]][["summarised_characteristics"]] <- omopgenerics::bind(result[["CohortCharacteristics"]][["summarised_characteristics"]] , dataList[["CohortCharacteristics"]][["summarised_characteristics"]] )
        result[["CohortCharacteristics"]][["summarised_large_scale_characteristics"]] <- omopgenerics::bind(result[["CohortCharacteristics"]][["summarised_large_scale_characteristics"]], dataList[["CohortCharacteristics"]][["summarised_large_scale_characteristics"]])
        result[["CohortSurvival"]][["single_event"]] <- omopgenerics::bind(result[["CohortSurvival"]][["single_event"]] , dataList[["CohortSurvival"]][["single_event"]])
        result[["CohortSurvival"]][["competing_risk"]] <- omopgenerics::bind(result[["CohortSurvival"]][["competing_risk"]], dataList[["CohortSurvival"]][["competing_risk"]])
        result[["TreatmentPatterns"]][["treatmentPathways"]] <- dplyr::bind_rows(result[["TreatmentPatterns"]][["treatmentPathways"]], dataList[["TreatmentPatterns"]][["treatmentPathways"]])
        result[["TreatmentPatterns"]][["metadata"]] <- dplyr::bind_rows(result[["TreatmentPatterns"]][["metadata"]], dataList[["TreatmentPatterns"]][["metadata"]])
        result[["TreatmentPatterns"]][["summaryStatsTherapyDuration"]] <- dplyr::bind_rows(result[["TreatmentPatterns"]][["summaryStatsTherapyDuration"]], dataList[["TreatmentPatterns"]][["summaryStatsTherapyDuration"]])
      }


    }
  }

  if (internal) {
    testData <- result
    usethis::use_data(testData,
                      internal = TRUE,
                      overwrite = TRUE)
  }


  duckdb::duckdb_shutdown(duckdb::duckdb())
  return(dataList)
}

getIncidencePrevalenceMock <- function(sampleSize) {

  checkmate::assert_class(sampleSize, "numeric")

  sampleSize <- 1000

  cdm <- IncidencePrevalence::mockIncidencePrevalenceRef(sampleSize = sampleSize,
                                                         outPre = 0.3,
                                                         minOutcomeDays = 365,
                                                         maxOutcomeDays = 3650)

  # Denominator data
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01")),
    ageGroup = list(
      c(0, 64),
      c(65, 100)
    ),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )
  # Incidence data
  incidence_estimates <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 180,
    completeDatabaseIntervals = TRUE,
    minCellCount = 5
  )


  incidence_estimates <- incidence_estimates |>
    visOmopResults::filterSettings(result_type == "incidence_attrition") |>
    dplyr::glimpse()


  # Attrition data
  incidence_attrition <- IncidencePrevalence::attrition(incidence_estimates)

  # Period prevalence
  prevalencePeriod <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = TRUE,
    minCellCount = 5
  )

  # Attrition data
  prevalence_period_attrition <- IncidencePrevalence::attrition(prevalencePeriod)

  # Point prevalence
  prevalencePoint <- IncidencePrevalence::estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    timePoint = "start",
    minCellCount = 5
  )
  # Attrition data
  prevalence_point_attrition <- IncidencePrevalence::attrition(prevalencePoint)

  # Join data
  prevalence_estimates <- omopgenerics::bind(prevalencePoint, prevalencePeriod)
  prevalence_attrition <- rbind(prevalence_point_attrition, prevalence_period_attrition)

  result <- list("incidence_estimates" = incidence_estimates,
                 "incidence_attrition" = incidence_attrition,
                 "prevalence_estimates" = prevalence_estimates,
                 "prevalence_point_attrition" = prevalence_point_attrition,
                 "prevalence_period_attrition" = prevalence_period_attrition)

  duckdb::duckdb_shutdown(duckdb::duckdb())
  return(result)
}

getCharacteristicsResult <- function() {

  # Set of mock results generated with the example at:
  # darwin-eu-dev/CohortCharacteristics

  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomia_dir()
  )

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schem = "main",
                                    write_schema = "main",
                                    cdm_name = "Eunomia"
  )

  meds_cs <- getDrugIngredientCodes(
    cdm = cdm,
    name = c(
      "acetaminophen",
      "morphine",
      "warfarin"
    )
  )

  cdm <- generateConceptCohortSet(
    cdm = cdm,
    name = "meds",
    conceptSet = meds_cs,
    end = "event_end_date",
    limit = "all",
    overwrite = TRUE
  )

  cdm <- generateConceptCohortSet(
    cdm = cdm,
    name = "injuries",
    conceptSet = list(
      "ankle_sprain" = 81151,
      "ankle_fracture" = 4059173,
      "forearm_fracture" = 4278672,
      "hip_fracture" = 4230399
    ),
    end = "event_end_date",
    limit = "all"
  )

  chars <- cdm$injuries |>
    summariseCharacteristics(cohortIntersectFlag = list(
      "Medications prior to index date" = list(
        targetCohortTable = "meds",
        window = c(-Inf, -1)
      ),
      "Medications on index date" = list(
        targetCohortTable = "meds",
        window = c(0, 0)
      )
    ))

  return(chars)
}

getLargeScaleCharacteristicsResult <- function() {

  # Set of mock results generated with the example at:
  # darwin-eu-dev/CohortCharacteristics

  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = CDMConnector::eunomia_dir()
  )

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schem = "main",
                                    write_schema = "main"
  )

  cdm <- generateConceptCohortSet(
    cdm = cdm,
    name = "ankle_sprain",
    conceptSet = list("ankle_sprain" = 81151),
    end = "event_end_date",
    limit = "first",
    overwrite = TRUE
  )

  lsc <- cdm$ankle_sprain |>
    summariseLargeScaleCharacteristics(
      window = list(c(-Inf, -1), c(0, 0)),
      eventInWindow = c(
        "condition_occurrence",
        "procedure_occurrence"
      ),
      episodeInWindow = "drug_exposure",
      minimumFrequency = 0.1
    )
  return(lsc)
}


getTreatmentPathways <- function() {

  # Set of mock results generated with the example at:
  # darwin-eu-dev/TreatmentPatterns

  cohortSet <- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns",
                       "exampleCohorts")
  )

  con <- DBI::dbConnect(
    drv = duckdb::duckdb(),
    dbdir = CDMConnector::eunomia_dir()
  )

  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main"
  )

  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table",
    overwrite = TRUE
  )

  cohorts <- cohortSet %>%
    # Remove 'cohort' and 'json' columns
    select(-"cohort", -"json", -"cohort_name_snakecase") %>%
    mutate(type = c("event", "event", "event", "event",
                    "exit", "event", "event", "target")) %>%
    rename(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
    )

  treatmentPathways <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  outputPath <- file.path(tempdir(), "treatmentPathways")

  dir.create(outputPath)

  TreatmentPatterns::export(
    andromeda = treatmentPathways,
    outputPath = outputPath,
    ageWindow = 10,
    minCellCount = 5,
    censorType = "minCellCount",
    archiveName = NULL
  )

  exportedFiles <- list.files(outputPath, pattern = "csv")
  exportedFileNames <- tools::file_path_sans_ext(exportedFiles)

  result <- list()
  for (i in 1:length(exportedFileNames)) {
    result[[exportedFileNames[i]]] <- read.csv(file.path(outputPath, exportedFiles[i]))
  }

  unlink(outputPath, recursive = TRUE)
  return(result)
}

getCohortSurvival <- function() {
  cdmSurvival <- CohortSurvival::mockMGUS2cdm()
  single_event <- CohortSurvival::estimateSingleEventSurvival(cdmSurvival,
                                                             targetCohortTable = "mgus_diagnosis",
                                                             targetCohortId = 1,
                                                             outcomeCohortTable = "death_cohort",
                                                             outcomeCohortId = 1,
                                                             strata = list(c("age_group"),
                                                                           c("sex"),
                                                                           c("age_group", "sex")))
  competing_risk <- CohortSurvival::estimateCompetingRiskSurvival(cdmSurvival,
                                                                 targetCohortTable = "mgus_diagnosis",
                                                                 outcomeCohortTable = "progression",
                                                                 competingOutcomeCohortTable = "death_cohort",
                                                                 strata = list(c("sex")))

  result <- list("single_event" = single_event,
                 "competing_risk" = competing_risk)
  return(result)
}
