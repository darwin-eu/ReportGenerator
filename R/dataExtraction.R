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
#' @import dplyr readr
#' @importFrom utils unzip
#' @importFrom readr read_csv
variablesConfigYaml <- function(fileDataPath = NULL,
                                package = "IncidencePrevalence",
                                version = NULL) {
  # fileDataPath <- here("results", "v0.2.1", "mock_data_ReportGenerator_SIDIAP.zip")
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
}

joinZipFiles <- function(uploadedFiles = NULL, csvLocation) {

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
    incidence_estimates <- data.frame()
    incidence_attrition <- data.frame()
    prevalence_estimates <- data.frame()
    prevalence_attrition <-  data.frame()

    for (folderLocation in databaseFolders) {
      incidence_estimate_file <- list.files(folderLocation,
                                            pattern = "incidence_estimates",
                                            full.names = TRUE,
                                            recursive = TRUE)
      incidence_estimate_file <- read.csv(incidence_estimate_file)
      incidence_estimates <- bind_rows(incidence_estimates, incidence_estimate_file)


      incidence_attrition_file <- list.files(folderLocation,
                                             pattern = "incidence_attrition",
                                             full.names = TRUE,
                                             recursive = TRUE)
      incidence_attrition_file <- read.csv(incidence_attrition_file)
      incidence_attrition <- bind_rows(incidence_attrition, incidence_attrition_file)

      prevalence_estimates_file <- list.files(folderLocation,
                                              pattern = "prevalence_estimates",
                                              full.names = TRUE,
                                              recursive = TRUE)
      prevalence_estimates_file <- read.csv(prevalence_estimates_file)
      prevalence_estimates <- bind_rows(prevalence_estimates, prevalence_estimates_file)

      prevalence_attrition_file <- list.files(folderLocation,
                                              pattern = "prevalence_attrition",
                                              full.names = TRUE,
                                              recursive = TRUE)
      prevalence_attrition_file <- read.csv(prevalence_attrition_file)
      prevalence_attrition <- bind_rows(prevalence_attrition, prevalence_attrition_file)
    }

    dir.create(path = paste0(csvLocation, "//", "csvFilesFolder"))
    csvFilesLocation <- paste0(csvLocation, "//", "csvFilesFolder")

    if (dir.exists(csvFilesLocation)) {
      write.csv(incidence_estimates, file = paste0(csvFilesLocation, "//", "incidence_estimates.csv"), row.names = FALSE)
      write.csv(incidence_attrition, file = paste0(csvFilesLocation, "//", "incidence_attrition.csv"), row.names = FALSE)
      write.csv(prevalence_estimates, file = paste0(csvFilesLocation, "//", "prevalence_estimates.csv"), row.names = FALSE)
      write.csv(prevalence_attrition, file = paste0(csvFilesLocation, "//", "prevalence_attrition.csv"), row.names = FALSE)
    }

    csvFiles <- list.files(csvFilesLocation, pattern = ".csv",
                           full.names = TRUE)

  } else if (grepl(".csv",
                   uploadedFiles[1],
                   fixed = TRUE)) {
    csvFiles <- uploadedFiles
  }

  return(csvFiles)
}

columnCheck <- function(csvFiles,
                        configData,
                        configDataTypes) {
  data <- list()
  for (fileLocation in csvFiles) {
    # fileLocation <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpCOhl36/mock_data_ReportGenerator_SIDIAP/test_database_incidence_attrition_2023_06_22.csv"
    resultsData <- read_csv(fileLocation)
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
