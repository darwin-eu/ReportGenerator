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
  # fileDataPath <- fileDataPath[1]

  if (package == "IncidencePrevalence") {
    csvLocation <- file.path(tempdir(), "dataLocation")
    dir.create(csvLocation)
    utils::unzip(zipfile = fileDataPath,
                 exdir = csvLocation)
    csvFiles <- list.files(path = csvLocation,
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
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    prevalenceAttritionPath <- csvFiles[stringr::str_detect(csvFiles, "prevalence_attrition")]
    prevalenceAttrition <- read_csv(prevalenceAttritionPath, show_col_types = FALSE)
    columnNamesPrevalenceAttrition <- setdiff(names(prevalenceAttrition), names(prevalenceEstimates))
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["prevalence_attrition"]][["names"]] <- columnNamesPrevalenceAttrition
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))

    unlink(csvLocation, recursive = TRUE)

  } else if (package == "TreatmentPatterns") {
    csvFiles <- fileDataPath
    treatmentPathwaysPath <- csvFiles[stringr::str_detect(csvFiles, "treatmentPathways")]
    treatmentPathways <- read_csv(treatmentPathwaysPath, show_col_types = FALSE)
    columnNamesTreatmentPathways <- names(treatmentPathways)
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
    configData[[package]][[version]][["treatmentPathways"]][["names"]] <- columnNamesTreatmentPathways
    write_yaml(configData, system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  }

}

# -------------------------------------------------------------------------


#' `joinDatabase()` joins several zip or csv folders into a list of dataframes.
#'
#' @param fileDataPath File path(s) in character
#' @param package Name of the packages that generated the results
#' @param versionData Version of the package
#'
#' @return A list of dataframes
#'
#' @import yaml
#' @export
joinDatabase <- function(fileDataPath = NULL,
                         fileName = NULL,
                         package = "IncidencePrevalence",
                         versionData = "0.5.1",
                         csvLocation = NULL) {

  # Loading yml file
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  configData <- configData[[package]][[versionData]]
  configDataTypes <- names(configData)
  # Check zip
  if (grepl(".zip", fileDataPath[1], fixed = TRUE)) {
    # Empty list to allocate data
    data <- list()
    # Folder count to allocate multiple databases
    folderNumber <- 0
    # Unzips every zip and puts the files in a separate folder in the temp dir
    for (fileLocation in fileDataPath) {
      # fileLocation <- fileLocation[1]
      folderNumber <- folderNumber + 1
      unzip(zipfile = fileLocation,
            exdir = file.path(csvLocation, paste0("database", as.character(folderNumber))))
    }
    # List of unzipped database directories where files are located
    databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
    # Iterates through each database folder to list the files inside
    for (filesList in databaseFolders) {
      # filesList <- databaseFolders[1]
      filesLocation <- list.files(filesList,
                                  pattern = ".csv",
                                  full.names = TRUE,
                                  recursive = TRUE)
      # Iterates every individual file
      for (file in filesLocation) {
        # file <- filesLocation[1]
        resultsData <- read_csv(file, show_col_types = FALSE)
        resultsColumns <- names(resultsData)
        # Checks the type of every individual file
        for (val in configDataTypes) {
          # val <- "incidence_attrition"
          if (val == "incidence_attrition" & grepl("incidence_attrition", file)) {
            configColumns <- configData[[val]]
            configColumns <- unlist(configColumns$names)
            if (all(configColumns %in% resultsColumns)) {
                message(paste0(val, ": match yes"))
                data[[val]] <- bind_rows(data[[val]], resultsData)
              }
          } else if (val == "prevalence_attrition" & grepl("prevalence_attrition", file)) {
              configColumns <- configData[[val]]
              configColumns <- unlist(configColumns$names)
              if (all(configColumns %in% resultsColumns)) {
                message(paste0(val, ": match yes"))
                data[[val]] <- bind_rows(data[[val]], resultsData)
            }
          } else if (val == "incidence_estimates") {
            configColumns <- configData[[val]]
            configColumns <- unlist(configColumns$names)
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match yes"))
              data[[val]] <- bind_rows(data[[val]], resultsData)
              }
          } else if (val == "prevalence_estimates") {
            configColumns <- configData[[val]]
            configColumns <- unlist(configColumns$names)
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match yes"))
              data[[val]] <- bind_rows(data[[val]], resultsData)
            }
          } else if (val == "prevalence_estimates") {
            configColumns <- configData[[val]]
            configColumns <- unlist(configColumns$names)
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match yes"))
              data[[val]] <- bind_rows(data[[val]], resultsData)
            }
          } else if (val == "treatmentPathways") {
            configColumns <- configData[[val]]
            configColumns <- unlist(configColumns$names)
            if (all(configColumns %in% resultsColumns)) {
              message(paste0(val, ": match yes"))
              data[[val]] <- bind_rows(data[[val]], resultsData)
            }
          }
        }
      }
    }
    } else if (grepl(".csv", fileDataPath[1], fixed = TRUE)) {
      data <- columnCheck(csvFiles = fileDataPath,
                          fileName = fileName,
                          configData = configData,
                          configDataTypes = configDataTypes)
    }
  return(data)
  }

columnCheck <- function(csvFiles,
                        fileName,
                        configData,
                        configDataTypes) {
  data <- list()
  # print("File to the loop")
  # print(csvFiles)
  for (i in seq(1:length(csvFiles))) {
    # i <- 1
    resultsData <- read_csv(csvFiles[i], show_col_types = FALSE)
    resultsColumns <- names(resultsData)
    for (val in configDataTypes) {
      # val <- "prevalence_attrition"
      if (val == "incidence_attrition" & grepl("incidence_attrition", fileName[i])) {
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (all(configColumns %in% resultsColumns)) {
            message(paste0(val, ": match yes"))
            data[[val]] <- bind_rows(data[[val]], resultsData)
          }
      } else if (val == "prevalence_attrition" & grepl("prevalence_attrition", fileName[i])) {
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (all(configColumns %in% resultsColumns)) {
            message(paste0(val, ": match yes"))
            data[[val]] <- bind_rows(data[[val]], resultsData)
          }
      } else if (val == "incidence_estimates") {
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (all(configColumns %in% resultsColumns)) {
          message(paste0(val, ": match yes"))
          data[[val]] <- bind_rows(data[[val]], resultsData)
        }
      } else if (val == "prevalence_estimates") {
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (all(configColumns %in% resultsColumns)) {
          message(paste0(val, ": match yes"))
          data[[val]] <- bind_rows(data[[val]], resultsData)
        }
      } else if (val == "treatmentPathways") {
        configColumns <- configData[[val]]
        configColumns <- unlist(configColumns$names)
        if (all(configColumns %in% resultsColumns)) {
          message(paste0(val, ": match yes"))
          data[[val]] <- bind_rows(data[[val]], resultsData)
        }
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
#' @param internal Boolean
#'
#' @importFrom usethis use_data
#'
#' @return sysdata.rda instruction
testData <- function(internal = TRUE) {
  # List
  uploadedFiles <- list.files(system.file("extdata", "examples", "IncPrev", "zip",
                                          package = "ReportGenerator"),
                              pattern = ".zip",
                              full.names = TRUE)

  treatmentPathways_test <- read_csv(system.file("extdata", "examples",
                                                 "TrePat", "csv", "treatmentPathways.csv",
                                                 package = "ReportGenerator"))
  checkmate::assertCharacter(uploadedFiles)
  checkmate::assertTibble(treatmentPathways_test)
  csvLocation <- file.path(tempdir(), "varDataLocation")
  dir.create(csvLocation)

  # Extract
  testData <- joinDatabase(uploadedFiles = uploadedFiles, csvLocation = csvLocation)

  # Assign
  incidence_attrition_test <- testData$incidence_attrition
  incidence_estimates_test <- testData$incidence_estimates
  prevalence_attrition_test <- testData$prevalence_attrition
  prevalence_estimates_test <- testData$prevalence_estimates

  if (internal) {
  # Save
  usethis::use_data(incidence_attrition_test, prevalence_attrition_test,
                    incidence_estimates_test, prevalence_estimates_test,
                    treatmentPathways_test,
                    internal = TRUE,
                    overwrite = TRUE)
  unlink(csvLocation, recursive = TRUE)
  } else {
    saveRDS(incidence_attrition_test, file = file.path(system.file("vignettes", package  = "ReportGenerator"), "incidence_attrition.rds"))
    saveRDS(incidence_estimates_test, file = file.path(system.file("vignettes", package  = "ReportGenerator"), "incidence_estimates.rds"))
    saveRDS(prevalence_attrition_test, file = file.path(system.file("vignettes", package  = "ReportGenerator"), "prevalence_attrition.rds"))
    saveRDS(prevalence_estimates_test, file = file.path(system.file("vignettes", package  = "ReportGenerator"), "prevalence_estimates.rds"))
    saveRDS(treatmentPathways_test, file = file.path(system.file("vignettes", package  = "ReportGenerator"), "treatmentPathways.rds"))
  }
}
