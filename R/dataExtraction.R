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
# distributed under the License is distributed on an "AS IS" BASIS,inst
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' `joinDatabases()` joins several zip or csv folders into a list of dataframes.
#'
#' @param fileDataPath List of full file locations
#' @param fileName Name of the file in character to process in case the input is only csv
#'
#' @return A list of dataframes
#'
#' @import yaml
#' @importFrom cli cli_progress_step
#' @export
joinDatabases <- function(fileDataPath) {

  # Check
  checkmate::assertCharacter(fileDataPath)

  # Settings
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  packagesNames <- names(configData)
  fileType <- getFileType(fileDataPath)
  unzipDir <- unzipDir()

  # Check zip
  if (fileType == "zip") {

    # `unzipFiles()` Unzip files
    databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                  fileDataPath = fileDataPath)

    # `extractCSV()` iterates folders for CSV files
    csv_files <- extractCSV(databaseFolders = databaseFolders,
                            configData = configData)
    csv_list <- processCSV(csv_files)
    summarised_result <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list)
    other_result <- importOtherResult(csv_list = csv_list$other_result_list,
                                      configData = configData)

  } else if (fileType == "csv") {
    cli::cli_progress_step("Processing {length(fileDataPath)} CSV files", spinner = TRUE)
    # fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\latest\\p3-c1-010-results 2\\p3-c1-010-results-cprd"
    # fileDataPath <- list.files(fileDataPath, full.names = TRUE)
    csv_list <- processCSV(csv_files = fileDataPath)
    summarised_result <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list)
    other_result <- importOtherResult(csv_list = csv_list$other_result_list,
                                      configData = configData)
    cli::cli_process_done()

  }
  result <- list("summarised_result" = summarised_result,
                 "other_result" = other_result)
  # cli::cli_alert("Results processed from {names(result)} package{?s}")
  return(result)
}

#' `unzipFiles()` to a folder per database in a temporary directory for later use in `exctractCSV()`
#'
#' @param unzipDir Locations of the folder to unzip files
#' @param fileDataPath Locations of the files to unzip
#'
#' @return The databaseFolders to which the files were unzipped
unzipFiles <- function(unzipDir,
                       fileDataPath) {

  cli::cli_progress_step(glue::glue("Unzipping {length(fileDataPath)} file{if (length(fileDataPath) > 1){'s'} else {''}}"), spinner = TRUE)

  if (!checkmate::testDirectoryExists(unzipDir)) {
    dir.create(unzipDir)
  }

  checkmate::assert_directory_exists(unzipDir)

  folderNumber <- 0

  # Unzipping puts the files in a separate folder
  for (fileLocation in fileDataPath) {
    folderNumber <- folderNumber + 1
    unzip(zipfile = fileLocation,
          exdir = file.path(unzipDir, paste0("database", as.character(folderNumber))),
          junkpaths = TRUE)
  }

  databaseFolders <- dir(unzipDir, full.names = TRUE)
  databaseFolders <- databaseFolders[!startsWith(basename(databaseFolders), "__")]
  checkmate::assertDirectoryExists(databaseFolders)

  return(databaseFolders)

}

#' `extractCSV()` iterates every file in the databaseFolders to `processCSV()`
#'
#' @param databaseFolders A list of full name folder locations
#' @param configData Configuration from yaml file
#'
#' @return A list objects with summarisedResults
extractCSV <- function(databaseFolders, configData) {
  cli::cli_alert("Extracting CSV files from {length(databaseFolders)} folder{?s}")
  result <- list()
  for (i in 1:length(databaseFolders)) {
    filesDir <- databaseFolders[i]
    filesList <- list.files(filesDir,
                            pattern = ".csv",
                            full.names = TRUE,
                            recursive = TRUE)
    result <- c(result, filesList) %>%
      unlist()
  }
  return(result)
}
#' `processCSV()` iterates every csv file and uses `loadFileData()` to check what type of result it is and adds it to a list
#'
#' @param data Results
#' @param filesLocation A path for filelocation
#' @param configData Data from yaml configuration file
#' @param databaseName Database name from TreatmentPatterns
#'
#' @return A list with all the results organized by type
processCSV <- function(csv_files) {
  cli::cli_alert("Processing CSV files")
  summarised_result_list <- list()
  other_result_list <- list()

  for (i in 1:length(csv_files)) {
    col_names <- names(data.table::fread(csv_files[i], nrows = 0, fill = TRUE))
    if (setequal(sort(col_names), sort(omopgenerics::resultColumns()))) {
      summarised_result_list <- c(summarised_result_list, csv_files[i]) %>%
        unlist()
    } else {
      other_result_list <- c(other_result_list, csv_files[i]) %>%
        unlist()
    }
  }

  result <- list("summarised_result_list" = summarised_result_list,
                 "other_result_list" = other_result_list)
  return(result)

}

importOtherResult <- function(csv_list,
                              configData) {

  data  <- list()
  # Assign the databaseName if there is a metadata file from TP
  databaseName <- getDatabaseName(csv_list)

  checkmate::assertList(data)
  # Iterates and checks every csv file and adds it
  for (i in 1:length(csv_list)) {
    # i <- 2
    cli::cli_text("Reading non-summarised result {csv_list[i]}")
    resultsData <- read_csv(csv_list[i], show_col_types = FALSE, col_types = c(.default = "c"))
    # resultsData
    # vroom::problems(resultsData)
    resultsColumns <- names(resultsData)
    # Change estimate values to character
    if ("estimate_value" %in% resultsColumns) {
      resultsData <- resultsData %>%
        mutate(estimate_value = as.character(estimate_value))
      checkmate::assertClass(resultsData$estimate_value, "character")
    }
    # Checks the type of every individual fileName
    data <- loadFileData(data,
                         fileName = csv_list[i],
                         configData,
                         resultsData,
                         resultsColumns,
                         databaseName)
  }
  return(data)
}

getFileType <- function(fileDataPath) {

  if (grepl(".zip", fileDataPath[1], fixed = TRUE)) {
    resultFileType <- "zip"
  } else if (grepl(".csv", fileDataPath[1], fixed = TRUE)) {
    resultFileType <- "csv"
  }
  return(resultFileType)

}

getDatabaseName <- function(filesLocation) {
  # Assign the databaseName in case there is a metadata file from TreatmentPatterns
  metadata <- filesLocation[stringr::str_detect(filesLocation, "metadata")]
  if (!identical(metadata, character(0))) {
    databaseName <- readr::read_csv(metadata, show_col_types = FALSE) %>%
      pull(cdmSourceName) %>%
      unique()
    return(databaseName)
  }
}

#' `loadFileData()` checks every csv their result type using columns or summarisedResult class as reference
#'
#' @param data named list with data.
#' @param fileName the file to load
#' @param configData config loaded from file
#' @param resultsData the loaded file data
#' @param resultsColumns the loaded file columns
#' @param databaseName db name
#'
#' @return the list with data
loadFileData <- function(data,
                         fileName,
                         configData,
                         resultsData,
                         resultsColumns,
                         databaseName) {

    for (pkg in names(configData)) {
      # pkg <- "IncidencePrevalence"
      pkgConfigData <- configData[[pkg]]
      for (val in names(pkgConfigData)) {
        # val <- "attrition"
        configColumns <- pkgConfigData[[val]]
        configColumns <- unlist(configColumns$names)
        if (val == "attrition") {
          if (all(configColumns %in% resultsColumns) & grepl("attrition", fileName)) {
            data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
        }
        else if (val == "cohortAttrition") {
          if (all(configColumns %in% resultsColumns)) {
            resultsData$excluded_subjects <- as.character(resultsData$excluded_subjects)
            resultsData$excluded_records <- as.character(resultsData$excluded_records)
            data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
          }
          else if (val == "treatmentPathways") {
            if (all(configColumns %in% resultsColumns)) {
              if (!('cdm_name' %in% resultsColumns)) {
                resultsData <- mutate(resultsData, cdm_name = databaseName)
                }
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          } else if (val == "cohortAttrition") {
            if (all(configColumns %in% resultsColumns)) {
              data[[pkg]][[val]] <- bind_rows(data[[pkg]][[val]], resultsData)
            }
          }
      }
    }
    return(data)
}

additionalCols <- function(data) {

  additionalCols <- data %>%
    pull(additional_name)


  additionalCols <- additionalCols[stringr::str_detect(additionalCols, " and ")] %>%
    unique() %>%
    str_split(" and ") %>%
    unlist()

  if (!is.null(additionalCols)) {
    result <- data %>%
      separate_wider_delim(additional_level,
                           delim = " and ",
                           names = additionalCols,
                           too_few = c("align_start"),
                           cols_remove = FALSE)
    # %>%
    #   select(!additional_name)
    return(result)
  } else {
    return(data)
  }

}

getPackageData <- function(data, package_name, resultType, resultsColumns, resultsData, configData) {
  pkgConfigData <- configData[[package_name]]
  configColumns <- pkgConfigData[[resultType]][["names"]]
  if (all(configColumns %in% resultsColumns)) {
    resultsDataWithCols <- additionalCols(resultsData)
    if (is.null(data[[package_name]])) {
      data[[package_name]] <- list()
    }
    if (is.null(data[[package_name]][[resultType]])) {
      data[[package_name]][[resultType]] <- tibble::tibble()
    }
    data[[package_name]][[resultType]] <- bind_rows(data[[package_name]][[resultType]], resultsDataWithCols)
  }
  return(data)
}

selectCols <- function(data) {
  result <- data %>%
    select(c(result_id, cdm_name, result_type, package_name, package_version,
             group_name, group_level, strata_name, strata_level, variable_name,
             variable_level, estimate_name, estimate_type, estimate_value,
             additional_name, additional_level))
  return(result)
}

selectColsLSC <- function(data) {
  result <- data %>%
    select(c(result_id, cdm_name, result_type, package_name, package_version,
             group_name, group_level, strata_name, strata_level, variable_name,
             variable_level, estimate_name, estimate_type, estimate_value,
             additional_name, additional_level, table_name))
  return(result)
}

unzipDir <- function() {
  directory <- file.path(tempdir(), getRandomId())
  return(directory)
}


#' Writes variablesConfig file in Yaml
#'
#' `variablesConfigYaml()` the user can load the column names to the variablesConfig.yaml so ReportGenerator can recognize its data.
#'
#' @param fileDataPath A zip folder with csv results from an analytical packages, e.g. IncidencePrevalence, PatientProfiles, TreatmentPatterns, etc.
#' @param package A string to identify the name of the package, default IncidencePrevalence.
#' @param version A string to identify which version of IncidencePrevalence is used to generate the results.
#'
#' @return Adds column names to the variablesConfig.yaml
#'
#' @import dplyr
#' @importFrom utils unzip
#' @importFrom readr read_csv
variablesConfigYaml <- function(fileDataPath = NULL,
                                package = "IncidencePrevalence",
                                version = NULL) {

  if (package == "IncidencePrevalence") {
    outputDir <- file.path(tempdir(), "dataLocation")
    dir.create(outputDir)
    utils::unzip(zipfile = fileDataPath,
                 exdir = outputDir)
    csvFiles <- list.files(path = outputDir,
                           pattern = ".csv",
                           full.names = TRUE,
                           recursive = TRUE)

    incidenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_estimates")]
    incidenceEstimates <- read_csv(incidenceEstimatesPath, show_col_types = FALSE)
    columnNamesIncidence <- names(incidenceEstimates)[grepl("incidence", names(incidenceEstimates))]
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["incidence_estimates"]][["names"]] <- columnNamesIncidence
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    prevalenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "prevalence_estimates")]
    prevalenceEstimates <- read_csv(prevalenceEstimatesPath, show_col_types = FALSE)
    columnNamesPrevalence <- names(prevalenceEstimates)[grepl("prevalence", names(prevalenceEstimates))]
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["prevalence_estimates"]][["names"]] <- columnNamesPrevalence
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    incidenceAttritionPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_attrition")]
    incidenceAttrition <- read_csv(incidenceAttritionPath, show_col_types = FALSE)
    columnNamesIncidenceAttrition <- setdiff(names(incidenceAttrition), names(incidenceEstimates))
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["incidence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
    configData[[package]][[version]][["prevalence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    unlink(outputDir, recursive = TRUE)

  } else if (package == "TreatmentPatterns") {
    csvFiles <- fileDataPath
    treatmentPathwaysPath <- csvFiles[stringr::str_detect(csvFiles, "treatmentPathways")]
    treatmentPathways <- read_csv(treatmentPathwaysPath, show_col_types = FALSE)
    columnNamesTreatmentPathways <- names(treatmentPathways)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["treatmentPathways"]][["names"]] <- columnNamesTreatmentPathways
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  } else if (package == "PatientProfiles") {
    if (is.null(version)) {
      version <- as.character(packageVersion(package))
    }
    checkmate::expect_character(version)

    cdm <- PatientProfiles::mockPatientProfiles(patient_size = 10)
    summaryPP <- CohortCharacteristics::summariseCharacteristics(cohort = cdm$cohort1, cdm = cdm)
    columnNamesCharacteristics <- names(summaryPP)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(summaryPP$result_type)]][["names"]] <- columnNamesCharacteristics
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    cdm[["cohort1"]] <- cdm[["cohort1"]]%>%
      PatientProfiles::addDemographics()

    lscPP <- CohortCharacteristics::summariseLargeScaleCharacteristics(cohort = cdm[["cohort1"]],
                                                                 strata = list("age",
                                                                               "sex"),
                                                                 window = list(c(-30, -1),
                                                                               c(0, 0),
                                                                               c(1, 30),
                                                                               c(31, 365)),
                                                                 eventInWindow = "condition_occurrence",
                                                                 episodeInWindow = "condition_occurrence",
                                                                 cdm = attr(cdm[["cohort1"]], "cdm_reference"))
    lscPP <- read.csv(lsc_csv)
    columnNamesLSC <- names(lscPP)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(lscPP$result_type)]][["names"]] <- columnNamesLSC
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  } else if (package == "CohortSurvival") {
    if (is.null(version)) {
      version <- as.character(packageVersion(package))
    }
    cdm <- CohortSurvival::mockMGUS2cdm()
    MGUS_death <- CohortSurvival::estimateSingleEventSurvival(cdm,
                                                              targetCohortTable = "mgus_diagnosis",
                                                              outcomeCohortTable = "death_cohort"
    )
    columnSurvival <- names(MGUS_death)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][[unique(MGUS_death$result_type)]][["names"]] <- columnSurvival
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  }
}

#' `variablesConfigAttrition()` extracts to variablesConfig file the columns to identify Cohort Attrition
#'
#' @param path To a cohort attrition CSV file
#'
#' @return Updates the yaml config file
variablesConfigAttrition <- function(path) {
  outputDir <- file.path(tempdir(), "dataLocation")
  dir.create(outputDir)
  utils::unzip(zipfile = fileDataPath,
               exdir = outputDir)
  csvFiles <- list.files(path = outputDir,
                         pattern = ".csv",
                         full.names = TRUE,
                         recursive = TRUE)

  incidenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_estimates")]
  incidenceEstimates <- read_csv(incidenceEstimatesPath, show_col_types = FALSE)
  columnNamesIncidence <- names(incidenceEstimates)[grepl("incidence", names(incidenceEstimates))]
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  configData[[package]][[version]][["incidence_estimates"]][["names"]] <- columnNamesIncidence
  write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  prevalenceEstimatesPath <- csvFiles[stringr::str_detect(csvFiles, "prevalence_estimates")]
  prevalenceEstimates <- read_csv(prevalenceEstimatesPath, show_col_types = FALSE)
  columnNamesPrevalence <- names(prevalenceEstimates)[grepl("prevalence", names(prevalenceEstimates))]
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  configData[[package]][[version]][["prevalence_estimates"]][["names"]] <- columnNamesPrevalence
  write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  incidenceAttritionPath <- csvFiles[stringr::str_detect(csvFiles, "incidence_attrition")]
  incidenceAttrition <- read_csv(incidenceAttritionPath, show_col_types = FALSE)
  columnNamesIncidenceAttrition <- setdiff(names(incidenceAttrition), names(incidenceEstimates))
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  configData[[package]][[version]][["incidence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
  configData[[package]][[version]][["prevalence_attrition"]][["names"]] <- columnNamesIncidenceAttrition
  write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

  unlink(outputDir, recursive = TRUE)
}

#' Clean attrition data
#'
#' `dataCleanAttrition()` clean incidence/prevalence attrition data
#'
#' @param attrition attrition data
#'
#' @return the updated attrition
dataCleanAttrition <- function(attrition) {
  if (!is.null(attrition)) {
    attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                                        "Prior history requirement not fulfilled during study period ",
                             attrition$reason)
    if (!("reason_id" %in% names(attrition))) {
      attrition <- attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16),
               number_subjects = current_n,
               excluded_subjects = excluded)
    } else {
      attrition <- attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16))
    }
    return(attrition)
  }
}



#' #' `testData()` extracts data from zip results and saves it in .rda format
#' #' @param testFilesIP testthat dir location of the test files with version number of folder (internal use).
#' #' @param testFilesTP testthat dir of treatment pattern files
#' #' @param testFilesPP testthat dir of patient profiles files
#' #' @param testFilesCS testthat dir of cohort survival files
#' #'
#' #' @importFrom usethis use_data
#' #'
#' #' @return sysdata.rda instruction
#' testData <- function(testFilesIP = testthat::test_path("IP", "0.6.0", "zip"),
#'                      testFilesTP = testthat::test_path("TP", "2.5.2", "csv", "CHUBX"),
#'                      testFilesPP = testthat::test_path("PP", "0.5.1", "zip"),
#'                      testFilesCS = testthat::test_path("CS", "0.2.5", "zip")) {
#'   checkmate::assertDirectoryExists(testFilesIP)
#'   checkmate::assertDirectoryExists(testFilesTP)
#'   checkmate::assertDirectoryExists(testFilesPP)
#'   checkmate::assertDirectoryExists(testFilesCS)
#'   uploadedFilesIP <- list.files(testFilesIP,
#'                                 pattern = ".zip",
#'                                 full.names = TRUE,
#'                                 recursive = TRUE)
#'   treatmentPathways_test <- read_csv(file.path(testFilesTP, "treatmentPathways.csv"),
#'                                      show_col_types = FALSE)
#'   uploadedFilesPP <- list.files(testFilesPP,
#'                                 pattern = ".zip",
#'                                 full.names = TRUE,
#'                                 recursive = TRUE)
#'   uploadedFilesCS <- list.files(testFilesCS,
#'                                 pattern = ".zip",
#'                                 full.names = TRUE,
#'                                 recursive = TRUE)
#'   checkmate::assertFileExists(uploadedFilesIP)
#'   checkmate::assertFileExists(uploadedFilesPP[1])
#'   checkmate::assertFileExists(uploadedFilesCS[1])
#'   checkmate::assertTibble(treatmentPathways_test)
#'   outputDir <- file.path(tempdir(), "dataLocation")
#'   dir.create(outputDir)
#'
#'   # Extract
#'   testData <- joinDatabases(fileDataPath  = uploadedFilesIP,
#'                            outputDir = outputDir)$IncidencePrevalence
#'   testData[["treatmentPathways_test"]] <- treatmentPathways_test
#'   testDataPP <- joinDatabases(fileDataPath  = uploadedFilesPP,
#'                              outputDir = outputDir)$PatientProfiles
#'   testDataCS <- joinDatabases(fileDataPath  = uploadedFilesCS,
#'                              outputDir = outputDir)$CohortSurvival
#'
#'   testData <- c(testData, testDataPP, testDataCS)
#'   unlink(outputDir, recursive = TRUE)
#'   return(testData)
#' }
