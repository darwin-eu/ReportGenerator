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
#' @import dplyr tidyr IncidencePrevalence duckdb checkmate CDMConnector PatientProfiles CohortSurvival
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
                             internal = FALSE) {

  if (!dir.exists(outputPath)) {
    dir.create(outputPath, recursive = TRUE)
  }

  result <- list()

  for (dbName in databaseName) {
    if (simulatePopulation == TRUE) {
      if (dbName== "CHUBX") {
        sampleSize <- 21523
      } else if (dbName== "CPRD_GOLD") {
        sampleSize <- 15662
      } else if (dbName== "IMASIS") {
        sampleSize <- 10147
      } else if (dbName== "IPCI") {
        sampleSize <- 26745
      } else if (dbName== "SIDIAP") {
        sampleSize <- 82653
      }
    } else {
      sampleSize <- 50000
    }

    # Generate data

    incidencePrevalenceData <- getIncidencePrevalence(sampleSize = sampleSize)
    treatmentPathwaysData <- getTreatmentPathways()
    characteristicsData <- getCharacteristicsResult()
    largeScaleCharacteristicsData <- getLargeScaleCharacteristicsResult()
    cohortSurvivalData <- getCohortSurvival()

    # Gather data

    dataList <- list("incidence_estimates" = incidencePrevalenceData$incidence_estimates,
                     "prevalence_estimates" = incidencePrevalenceData$prevalence_estimates,
                     "incidence_attrition" = incidencePrevalenceData$incidence_attrition,
                     "prevalence_attrition" = incidencePrevalenceData$incidence_attrition,
                     "treatmentPathways" = treatmentPathwaysData$treatmentPathways,
                     "metadata" = treatmentPathwaysData$metadata,
                     "summaryStatsTherapyDuration" = treatmentPathwaysData$summaryStatsTherapyDuration,
                     "summarised_characteristics" = characteristicsData,
                     "summarised_large_scale_characteristics" = largeScaleCharacteristicsData,
                     "Survival estimate" = cohortSurvivalData$survivalEstimate,
                     "Survival cumulative incidence" = cohortSurvivalData$survivalCumulativeIncidence)

    # Insert database name

    dataList <- lapply(dataList, mutate, cdm_name = dbName)

    dataList[["treatmentPathways"]] <- treatmentPathwaysData$treatmentPathways
    dataList[["metadata"]] <- treatmentPathwaysData$metadata
    dataList[["summaryStatsTherapyDuration"]] <- treatmentPathwaysData$summaryStatsTherapyDuration

    exportResults(resultList = dataList,
                  zipName = paste0("mock_data_", dbName),
                  outputFolder = outputPath)
  }

  if (internal) {
    testData <- dataList
    usethis::use_data(testData,
                      internal = TRUE,
                      overwrite = TRUE)
  }
  duckdb::duckdb_shutdown(duckdb::duckdb())
}

getIncidencePrevalence <- function(sampleSize) {

  checkmate::assert_class(sampleSize, "numeric")

  cdm <- IncidencePrevalence::mockIncidencePrevalenceRef(
    sampleSize = sampleSize,
    outPre = 0.5)

  # Denominator data
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm = cdm,
                                                           name = "denominator",
                                                           cohortDateRange = c(as.Date("2008-01-01"),
                                                                               as.Date("2012-01-01")),
                                                           ageGroup  = list(c(18, 39),
                                                                            c(40, 59),
                                                                            c(18, 99)),
                                                           sex  = c("Female", "Male", "Both"),
                                                           daysPriorObservation = 365)
  # Incidence data
  incidence_estimates <- IncidencePrevalence::estimateIncidence(cdm = cdm,
                                                                denominatorTable = "denominator",
                                                                outcomeTable = "outcome",
                                                                interval = c("years", "overall"),
                                                                completeDatabaseIntervals = TRUE,
                                                                outcomeWashout = 180,
                                                                repeatedEvents = FALSE,
                                                                minCellCount = 5,
                                                                returnParticipants = FALSE)

  # Attrition data
  incidence_attrition <- IncidencePrevalence::attrition(incidence_estimates)

  # Period prevalence
  prevalencePeriod <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,
                                                                    denominatorTable = "denominator",
                                                                    outcomeTable = "outcome")

  # Attrition data
  prevalence_period_attrition <- IncidencePrevalence::attrition(prevalencePeriod)

  # Point prevalence
  prevalencePoint <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,
                                                                  denominatorTable = "denominator",
                                                                  outcomeTable = "outcome",
                                                                  interval = "years",
                                                                  timePoint = "start")
  # Attrition data
  prevalence_point_attrition <- IncidencePrevalence::attrition(prevalencePoint)



  # Join data
  prevalence_estimates <- rbind(prevalencePoint,
                                prevalencePeriod)
  prevalence_attrition <- rbind(prevalence_point_attrition,
                                prevalence_period_attrition)

  result <- list("incidence_estimates" = incidence_estimates,
                 "incidence_attrition" = incidence_attrition,
                 "prevalence_estimates" = prevalence_estimates,
                 "prevalence_attrition" = prevalence_attrition)

  duckdb::duckdb_shutdown(duckdb::duckdb())
  return(result)
}

getCharacteristicsResult <- function() {

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date(c(
      "1985-01-01", "1989-04-29", "1974-12-03"
    )),
    observation_period_end_date = as.Date(c(
      "2011-03-04", "2022-03-14", "2023-07-10"
    )),
    period_type_concept_id = 0
  )
  dus_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    )),
    cohort_end_date = as.Date(c(
      "1990-04-19", "1991-04-19", "2010-11-14", "2000-05-25"
    ))
  )
  comorbidities <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 2, 1),
    subject_id = c(1, 1, 3, 3),
    cohort_start_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-01-01", "1990-06-01", "2000-01-01", "2000-06-01"
    ))
  )
  medication <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 2, 1),
    subject_id = c(1, 1, 2, 3),
    cohort_start_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    )),
    cohort_end_date = as.Date(c(
      "1990-02-01", "1990-08-01", "2009-01-01", "1995-06-01"
    ))
  )
  emptyCohort <- dplyr::tibble(
    cohort_definition_id = numeric(),
    subject_id = numeric(),
    cohort_start_date = as.Date(character()),
    cohort_end_date = as.Date(character())
  )
  cdm <- mockPatientProfiles(
    dus_cohort = dus_cohort, cohort1 = emptyCohort,
    cohort2 = emptyCohort, observation_period = observation_period,
    comorbidities = comorbidities, medication = medication
  )

  cdm$dus_cohort <- omopgenerics::newCohortTable(
    table = cdm$dus_cohort, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("exposed", "unexposed")
    )
  )
  cdm$comorbidities <- omopgenerics::newCohortTable(
    table = cdm$comorbidities, cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2), cohort_name = c("covid", "headache")
    )
  )
  cdm$medication <- omopgenerics::newCohortTable(
    table = cdm$medication,
    cohortSetRef = dplyr::tibble(
      cohort_definition_id = c(1, 2, 3),
      cohort_name = c("acetaminophen", "ibuprophen", "naloxone")
    ),
    cohortAttritionRef = NULL
  )
  characteristicsResult <- summariseCharacteristics(
    cdm$dus_cohort,
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medication", value = "flag", window = c(-365, 0)
      ), "Comorbidities" = list(
        targetCohortTable = "comorbidities", value = "flag", window = c(-Inf, 0)
      )
    )
  )
  return(characteristicsResult)
}

getLargeScaleCharacteristicsResult <- function() {

  # Mock data example from PatientProfiles vignette

  person <- dplyr::tibble(
    person_id = c(1, 2),
    gender_concept_id = c(8507, 8532),
    year_of_birth = c(1990, 1992),
    month_of_birth = c(1, 1),
    day_of_birth = c(1, 1),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1, 2),
    observation_period_start_date = as.Date(c("2011-10-07", "2000-01-01")),
    observation_period_end_date = as.Date(c("2031-10-07", "2030-01-01")),
    period_type_concept_id = 44814724
  )
  cohort_interest <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1, 2),
    subject_id = c(1, 1, 2, 2),
    cohort_start_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    )),
    cohort_end_date = as.Date(c(
      "2012-10-10", "2015-01-01", "2013-10-10", "2015-01-01"
    ))
  )
  drug_exposure <- dplyr::tibble(
    drug_exposure_id = 1:11,
    person_id = c(rep(1, 8), rep(2, 3)),
    drug_concept_id = c(
      rep(1125315, 2), rep(1503328, 5), 1516978, 1125315, 1503328, 1516978
    ),
    drug_exposure_start_date = as.Date(c(
      "2010-10-01", "2012-12-31", "2010-01-01", "2012-09-01", "2013-04-01",
      "2014-10-31", "2015-05-01", "2015-10-01", "2012-01-01", "2012-10-01",
      "2014-10-12"
    )),
    drug_exposure_end_date = as.Date(c(
      "2010-12-01", "2013-05-12", "2011-01-01", "2012-10-01", "2013-05-01",
      "2014-12-31", "2015-05-02", "2016-10-01", "2012-01-01", "2012-10-30",
      "2015-01-10"
    )),
    drug_type_concept_id = 38000177,
    quantity = 1
  )
  condition_occurrence <- dplyr::tibble(
    condition_occurrence_id = 1:8,
    person_id = c(rep(1, 4), rep(2, 4)),
    condition_concept_id = c(
      317009, 378253, 378253, 4266367, 317009, 317009, 378253, 4266367
    ),
    condition_start_date = as.Date(c(
      "2012-10-01", "2012-01-01", "2014-01-01", "2010-01-01", "2015-02-01",
      "2012-01-01", "2013-10-01", "2014-10-10"
    )),
    condition_end_date = as.Date(c(
      "2013-01-01", "2012-04-01", "2014-10-12", "2015-01-01", "2015-03-01",
      "2012-04-01", "2013-12-01", NA
    )),
    condition_type_concept_id = 32020
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    person = person, observation_period = observation_period,
    cohort_interest = cohort_interest, drug_exposure = drug_exposure,
    condition_occurrence = condition_occurrence
  )

  concept <- dplyr::tibble(
    concept_id = c(1125315, 1503328, 1516978, 317009, 378253, 4266367),
    domain_id = NA_character_,
    vocabulary_id = NA_character_,
    concept_class_id = NA_character_,
    concept_code = NA_character_,
    valid_start_date = as.Date("1900-01-01"),
    valid_end_date = as.Date("2099-01-01")
  ) %>%
    dplyr::mutate(concept_name = paste0("concept: ", .data$concept_id))

  cdm <- CDMConnector::insertTable(cdm, "concept", concept)

  largeScaleCharacteristicsResult <- cdm$cohort_interest %>%
    PatientProfiles::addDemographics(
      ageGroup = list(c(0, 24), c(25, 150))
    ) %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      strata = list("age_group", c("age_group", "sex")),
      episodeInWindow = c("condition_occurrence", "drug_exposure"),
      minimumFrequency = 0
    )
  return(largeScaleCharacteristicsResult)
}


getTreatmentPathways <- function() {

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
  singleEvent <- CohortSurvival::estimateSingleEventSurvival(cdmSurvival,
                                                             targetCohortTable = "mgus_diagnosis",
                                                             targetCohortId = 1,
                                                             outcomeCohortTable = "death_cohort",
                                                             outcomeCohortId = 1,
                                                             strata = list(c("age_group"),
                                                                           c("sex"),
                                                                           c("age_group", "sex")))
  competingRisk <- CohortSurvival::estimateCompetingRiskSurvival(cdmSurvival,
                                                                 targetCohortTable = "mgus_diagnosis",
                                                                 outcomeCohortTable = "progression",
                                                                 competingOutcomeCohortTable = "death_cohort",
                                                                 strata = list(c("sex")))

  result <- list("survivalEstimate" = singleEvent,
                 "survivalCumulativeIncidence" = competingRisk)
  return(result)
}
