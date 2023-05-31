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

#' Extracts data from csv files
#'
#' @param importFolderDenominator Location of csv files.
#' @return A tibble.
#' @export
denominatorExtraction <- function (importFolderDenominator = here("inst/csv/denominatorMockData")) {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderDenominator,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  )

  return(result)
}
#' Extracts data from csv files and saves rds
#'
#' @param importFolderIndcidence Location of csv files.
#' @param studyName Name of the study.
#' @return A tibble
#' @export
incidenceExtraction <- function (importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                                 studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderIndcidence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))
  return(result)
}
#' Extracts data from csv files and saves rds
#'
#' @param importFolderIndcidence Location of incidence results.
#' @param studyName Name of the study.
#' @return A tibble
#' @export
#'
incidenceExtractionToRDS <- function (importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                                      studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderIndcidence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))

  subDir <- here("inst",
                 "data",
                 "incidence")

  if (!file.exists(subDir)) {
    dir.create(file.path(here("inst",
                              "data",
                              "incidence")),
               recursive = TRUE)
  }

  saveRDS(result,
          here("inst",
               "data",
               "incidence",
               paste0(
                 studyName,
                 ".rds"
               )
               )
          )
}
#' Extracts data from csv and saves rds
#'
#' @param importFolderPrevalence Location of prevalence results.
#' @param studyName Name of the study.
#' @return A tibble
#' @export
#'
prevalenceExtraction <- function (importFolderPrevalence = here("inst/csv/prevalenceMockResults"),
                                  studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderPrevalence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))
  return(result)

}
#' Extracts data from csv files and saves rds
#'
#' @param importFolderPrevalence Location of prevalence results.
#' @param studyName Name of the study.
#' @return A tibble
#' @export
#'
prevalenceExtractionToRDS <- function (importFolderPrevalence = here("inst/csv/prevalenceMockResults"),
                                       studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderPrevalence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))

  subDir <- here("inst",
                 "data",
                 "prevalence")

  if (!file.exists(subDir)) {
    dir.create(file.path(here("inst",
                              "data",
                              "prevalence")),
               recursive = TRUE)
  }

  saveRDS(result,
          here("inst",
               "data",
               "prevalence",
               paste0(
                 studyName,
                 ".rds"
               )
          )
  )

}

#' Joins zip databases
#'
#' `joinZipFiles()` joins data from multiple datapartners and returns a list of condensed files in a temp dir
#'
#' @param filesLocation Location of zip files
#' @return A list of files
#' @import dplyr
#' @export
joinZipFiles <- function(uploadedFiles = NULL) {

  # Results
  # uploadedFiles <- list.files(here("results"), full.names = TRUE, pattern = ".zip")

  # Other results
  # uploadedFiles <- list.files(here("OtherResults"), full.names = TRUE, pattern = ".zip")

  if (grepl(".zip",
            uploadedFiles[1],
            fixed = TRUE)) {

    csvLocation <- tempdir()
    lapply(list.files(path = csvLocation, full.names = TRUE), unlink)
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

      # folderLocation <- databaseFolders[3]

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
  unlink(csvLocation)
  return(csvFiles)
}

columnCheck <- function(csvFiles,
                        configData,
                        configDataTypes) {
  data <- list()
  for (fileLocation in csvFiles) {
    resultsData <- read_csv(fileLocation)
    resultsColumns <- names(resultsData)
    for (val in configDataTypes) {
      configColumns <- configData %>% filter(name == val)
      configColumns <- configColumns$variables
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
  if (is.null(data)) {
    warning("No correspondance in columns was detected")
    return(data)
  } else {
    message("Correspondance detected")
    return(data)
  }
}

#' Writes variablesConfig file
#'
#' `variablesConfigWriter()` takes a list of csv files, extract their columns and writes de variables config file
#'
#' @param fileDataPath A list of csv files
#'
#' @return Writes a csv file into the config folder of ReportGenerator
#' @export
#' @import dplyr
variablesConfigWriter <- function(fileDataPath = NULL) {

  # fileDataPath <- here("results", "mock_data_ReportGenerator_SIDIAP.zip")

  csvLocation <- tempdir()

  unzip(fileDataPath, exdir = csvLocation)

  csvFiles <- list.files(path = csvLocation,
                         pattern = ".csv",
                         full.names = TRUE,
                         recursive = TRUE)

  for (fileLocation in csvFiles) {

    if (grepl("incidence_attrition", fileLocation)) {
      variablesConfig <- read_csv("inst/config/variablesConfig.csv")
      variablesConfig <- filter(variablesConfig, name != "incidence_attrition")
      incidence_attrition <- read_csv(fileLocation)
      tempNames <- list(names(incidence_attrition))
      tempTitle <- "incidence_attrition"
      itemDataFrame <- data.frame(name = tempTitle, variables = tempNames[[1]])
      itemsVariables <- bind_rows(variablesConfig, itemDataFrame)
      itemsVariablesExport <- itemsVariables %>% filter(name %in% c("incidence_attrition",
                                                                    "incidence_estimates",
                                                                    "prevalence_attrition",
                                                                    "prevalence_estimates"))
      write.csv(itemsVariablesExport, file = here("inst",
                                                  "config",
                                                  "variablesConfig.csv"), row.names = FALSE)

    } else if(grepl("incidence_estimates", fileLocation)) {
      variablesConfig <- read.csv("inst/config/variablesConfig.csv")
      variablesConfig <- filter(variablesConfig, name != "incidence_estimates")
      incidence_estimates <- read_csv(fileLocation)
      tempNames <- list(names(incidence_estimates))
      tempTitle <- "incidence_estimates"
      itemDataFrame <- data.frame(name = tempTitle, variables = tempNames[[1]])
      itemsVariables <- bind_rows(variablesConfig, itemDataFrame)
      itemsVariablesExport <- itemsVariables %>% filter(name %in% c("incidence_attrition",
                                                                    "incidence_estimates",
                                                                    "prevalence_attrition",
                                                                    "prevalence_estimates"))
      write.csv(itemsVariablesExport, file = here("inst",
                                                  "config",
                                                  "variablesConfig.csv"), row.names = FALSE)

    } else if(grepl("prevalence_attrition", fileLocation)) {
      variablesConfig <- read.csv("inst/config/variablesConfig.csv")
      variablesConfig <- filter(variablesConfig, name != "prevalence_attrition")
      prevalence_attrition <- read_csv(fileLocation)
      tempNames <- list(names(prevalence_attrition))
      tempTitle <- "prevalence_attrition"
      itemDataFrame <- data.frame(name = tempTitle, variables = tempNames[[1]])
      itemsVariables <- bind_rows(variablesConfig, itemDataFrame)
      itemsVariablesExport <- itemsVariables %>% filter(name %in% c("incidence_attrition",
                                                                    "incidence_estimates",
                                                                    "prevalence_attrition",
                                                                    "prevalence_estimates"))
      write.csv(itemsVariablesExport, file = here("inst",
                                                  "config",
                                                  "variablesConfig.csv"), row.names = FALSE)

    } else if(grepl("prevalence_estimates", fileLocation)) {
      variablesConfig <- read.csv("inst/config/variablesConfig.csv")
      variablesConfig <- filter(variablesConfig, name != "prevalence_estimates")
      prevalence_estimates <- read_csv(fileLocation)
      tempNames <- list(names(prevalence_estimates))
      tempTitle <- "prevalence_estimates"
      itemDataFrame <- data.frame(name = tempTitle, variables = tempNames[[1]])
      itemsVariables <- bind_rows(variablesConfig, itemDataFrame)
      itemsVariablesExport <- itemsVariables %>% filter(name %in% c("incidence_attrition",
                                                                    "incidence_estimates",
                                                                    "prevalence_attrition",
                                                                    "prevalence_estimates"))
      write.csv(itemsVariablesExport, file = here("inst",
                                                  "config",
                                                  "variablesConfig.csv"), row.names = FALSE)

    }



  }

  # variablesIncidence <- filter(variablesConfig, name == "incidence_estimates")


  tempNames <- list(names(prevalence_estimates))

  tempTitle <- "prevalence_estimates"

  itemDataFrame <- data.frame(name = tempTitle, variables = tempNames[[1]])

  itemsVariables <- bind_rows(variablesConfig, itemDataFrame)


  itemsVariablesExport <- itemsVariables %>% filter(name %in% c("cdm_snapshot", "doseSummaryWide", "incidence_attrition", "incidence_estimates",
                                                                "indicationSummaryWide", "largeScaleSummary", "LSC", "prevalence_attrition",
                                                                "prevalence_estimates", "stratification" ))

  write.csv(itemsVariablesExport, file = here("inst",
                                              "config",
                                              "variablesConfig.csv"), row.names = FALSE)


}

