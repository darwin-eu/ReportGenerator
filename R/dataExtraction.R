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

#' Writes variablesConfig file in Yaml
#'
#' `variablesConfigYaml()` the user can load the column names to the variablesConfig.yaml so ReportGenerator can recognize its data.
#'
#' @param fileDataPath A list of zipped csv files.
#' @param package A string to identify the name of the package, default IncidencePrevalence.
#' @param version A string to identify which version of IncidencePrevalence is used to generate the results.
#'
#' @return Adds column names to the variablesConfig.yaml
#' @export
#' @import dplyr
#' @importFrom utils unzip
#' @importFrom readr read_csv
variablesConfigYaml <- function(fileDataPath = NULL,
                                package = "IncidencePrevalence",
                                version = NULL) {
  # fileDataPath <- "D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/inst/extdata/examples/0.4.1/zip/mock_data_ReportGenerator_SIDIAP.zip"
  if (package == "IncidencePrevalence") {
    csvLocation <- file.path(tempdir(), "varDataLocation")
    utils::unzip(zipfile = fileDataPath,
                 exdir = csvLocation)
    csvFiles <- list.files(path = csvLocation,
                           pattern = ".csv",
                           full.names = TRUE,
                           recursive = TRUE)
    for (fileLocation in csvFiles) {
      if (grepl("prevalence_attrition", fileLocation)) {
        prevalence_attrition <- read_csv(fileLocation, show_col_types = FALSE)
        tempNames <- names(prevalence_attrition)
        tempTitle <- "prevalence_attrition"
        configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        configData[[package]][[version]][["prevalence_attrition"]][["names"]] <- tempNames
        write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
      } else if (grepl("incidence_attrition", fileLocation)) {
        incidence_attrition <- read_csv(fileLocation, show_col_types = FALSE)
        tempNames <- names(incidence_attrition)
        tempTitle <- "incidence_attrition"
        configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        configData[[package]][[version]][["incidence_attrition"]][["names"]] <- tempNames
        write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
      } else if(grepl("incidence_estimates", fileLocation)) {
        incidence_estimates <- read_csv(fileLocation, show_col_types = FALSE)
        tempNames <- names(incidence_estimates)
        tempTitle <- "incidence_estimates"
        configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        configData[[package]][[version]][["incidence_estimates"]][["names"]] <- tempNames
        write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
      } else {
        prevalence_estimates <- read_csv(fileLocation, show_col_types = FALSE)
        tempNames <- names(prevalence_estimates)
        tempTitle <- "prevalence_estimates"
        configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        configData[[package]][[version]][["prevalence_estimates"]][["names"]] <- tempNames
        write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
      }
    }
    unlink(csvLocation, recursive = TRUE)
  } else if (package == "TreatmentPatterns") {
    csvFiles <- list.files(path = fileDataPath,
                           pattern = ".csv",
                           full.names = TRUE,
                           recursive = TRUE)
    for (fileLocation in csvFiles) {
      if (grepl("treatmentPathways.csv", fileLocation)) {
        treatmentPathways <- read_csv(fileLocation, show_col_types = FALSE)
        tempNames <- names(treatmentPathways)
        tempTitle <- "treatmentPathways"
        configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        configData[[package]][[version]][[tempTitle]][["names"]] <- tempNames
        write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
        }
      }
    }
}

#' `joinDatabase()` gathers several zip or csv folders into a list of dataframes for ReportGenerator
#'
#' @param uploadedFiles A list of filepaths
#' @param csvLocation Temporary folder
#' @param package Name of the packages that generated the results
#' @param versionData Version of the package
#'
#' @return A list of dataframes
#'
#' @import yaml
#' @export
joinDatabase <- function(uploadedFiles = NULL,
                         csvLocation,
                         package = "IncidencePrevalence",
                         versionData = "0.4.1") {
  # csvLocation <- file.path(tempdir(), "dataLocation")
  # dir.create(csvLocation)
  #
  # uploadedFiles <- c("D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/opiodsDataPartners/CDWBordeaux_IncidencePrevalenceResults.zip",
  #                    "D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/opiodsDataPartners/IPCI_IncidencePrevalenceResults.zip",
  #                    "D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/opiodsDataPartners/SIDIAP_IncidencePrevalenceResults.zip")
  #
  # package <- "IncidencePrevalence"
  # versionData <- "0.4.1"

  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  configData <- configData[[package]][[versionData]]
  configDataTypes <- names(configData)
  if (grepl(".zip",
            uploadedFiles[1],
            fixed = TRUE)) {
    folderNumber <- 0
    for (fileLocation in uploadedFiles) {
      folderNumber <- folderNumber + 1
      unzip(zipfile = fileLocation,
            exdir = paste0(csvLocation, "/", "database", as.character(folderNumber)))
    }
    databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
    data <- list()
    for (filesList in databaseFolders) {
      # filesList <- databaseFolders[1]
      filesLocation <- list.files(filesList,
                                  pattern = ".csv",
                                  full.names = TRUE,
                                  recursive = TRUE)
      for (file in filesLocation) {
        # file <- filesLocation[1]
        resultsData <- read_csv(file, show_col_types = FALSE)
        resultsColumns <- names(resultsData)
        for (val in configDataTypes) {
          # val <- "incidence_estimates"
          configColumns <- configData[[val]]
          configColumns <- unlist(configColumns$names)
          if (length(configColumns) == length(resultsColumns)) {
            if (identical(configColumns, resultsColumns)) {
              message(paste0(val, ": match yes"))
              data[[val]] <- bind_rows(data[[val]], resultsData)
              }
            }
        }
      }
      }

  } else if (grepl(".csv",
                   uploadedFiles[1],
                   fixed = TRUE)) {
    # csvLocation <- file.path(tempdir(), "dataLocation")
    # dir.create(csvLocation)
    #
    # uploadedFiles <- list.files("D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/0.4.1/csv", full.names = TRUE)
    #
    # package <- "IncidencePrevalence"
    # versionData <- "0.4.1"
    for (file in uploadedFiles) {
      # file <- filesLocation[1]
      resultsData <- read_csv(file, show_col_types = FALSE)
      resultsColumns <- names(resultsData)
      for (val in configDataTypes) {
        # val <- "incidence_attrition"
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (length(configColumns) == length(resultsColumns)) {
          if (identical(configColumns, resultsColumns)) {
            message(paste0(val, ": match yes"))
            data[[val]] <- bind_rows(data[[val]], resultsData)
          }
        }
      }
    }
  }

  return(data)
}

columnCheck <- function(csvFiles,
                        configData,
                        configDataTypes) {
  data <- list()
  for (fileLocation in csvFiles) {
    # fileLocation <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpCOhl36/mock_data_ReportGenerator_SIDIAP/test_database_incidence_attrition_2023_06_22.csv"
    resultsData <- read_csv(fileLocation, show_col_types = FALSE)
    resultsColumns <- names(resultsData)
    for (val in configDataTypes) {
      # val <- "incidence_attrition"
      configColumns <- configData[[val]]
      configColumns <- unlist(configColumns$names)
      if (length(configColumns) == length(resultsColumns)) {
        message("Length correspondance yes")
        if (identical(configColumns, resultsColumns)) {
          message("Length correspondance yes")
          data[[val]] <- resultsData
          } else {
            message("Length correspondance no")
            }
        } else {
          message("Length correspondance no")
          }
      }
  }
  return(data)
}

dataCleanAttrition <- function(incidence_attrition = NULL,
                               prevalence_attrition = NULL) {
  if (!is.null(prevalence_attrition)) {
    prevalence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                                        "Prior history requirement not fulfilled during study period ",
                                        prevalence_attrition$reason)
    if (!("reason_id" %in% names(prevalence_attrition))) {
      prevalence_attrition <- prevalence_attrition %>%
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
      prevalence_attrition <- prevalence_attrition %>%
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
    return(prevalence_attrition)
  } else if (!is.null(incidence_attrition)) {
    incidence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                                       "Prior history requirement not fulfilled during study period ",
                                       incidence_attrition$reason)
    if (!("reason_id" %in% names(incidence_attrition))) {
      incidence_attrition <- incidence_attrition %>%
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
      incidence_attrition <- incidence_attrition %>%
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
    return(incidence_attrition)
  }
}

#' `testData()` extracts data from zip results and saves it in .rda format
#'
#' @param fileDataPath A path to the zip file, in character
#'
#' @return sysdata.rda instruction
testData <- function() {
  # List
  uploadedFiles <- list.files(system.file("extdata", "examples", "0.4.1", "zip",
                                          package = "ReportGenerator"),
                              pattern = ".zip",
                              full.names = TRUE)

  csvLocation <- file.path(tempdir(), "varDataLocation")
  dir.create(csvLocation)

  # Extract
  testData <- joinDatabase(uploadedFiles = uploadedFiles, csvLocation = csvLocation)

  # Assign
  incidence_attrition_test <- testData$incidence_attrition
  incidence_estimates_test <- testData$incidence_estimates
  prevalence_attrition_test <- testData$prevalence_attrition
  prevalence_estimates_test <- testData$prevalence_estimates

  # Save
  usethis::use_data(incidence_attrition_test, prevalence_attrition_test,
                    incidence_estimates_test, prevalence_estimates_test,
                    internal = TRUE)
  unlink(csvLocation, recursive = TRUE)
}
