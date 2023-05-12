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


joinZipFiles <- function(filesLocation = NULL) {

  uploadedFiles <- list.files(path = filesLocation, full.names = TRUE)

  # fileLocation <- "D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/onlyZip"

  # uploadedFiles <- list.files(path = "D:/Users/cbarboza/Documents/darwin-docs/darwinReport/ReportGenerator/results/onlyZip", full.names = TRUE)

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
      incidence_estimate_file <- list.files(folderLocation,
                                            pattern = "incidence_estimates",
                                            full.names = TRUE)
      incidence_estimate_file <- read.csv(incidence_estimate_file)
      incidence_estimates <- bind_rows(incidence_estimates, incidence_estimate_file)

      incidence_attrition_file <- list.files(folderLocation,
                                             pattern = "incidence_attrition",
                                             full.names = TRUE)
      incidence_attrition_file <- read.csv(incidence_attrition_file)
      incidence_attrition <- bind_rows(incidence_attrition, incidence_attrition_file)

      prevalence_estimates_file <- list.files(folderLocation,
                                              pattern = "prevalence_estimates",
                                              full.names = TRUE)
      prevalence_estimates_file <- read.csv(prevalence_estimates_file)
      prevalence_estimates <- bind_rows(prevalence_estimates, prevalence_estimates_file)

      prevalence_attrition_file <- list.files(folderLocation,
                                              pattern = "prevalence_attrition",
                                              full.names = TRUE)
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


