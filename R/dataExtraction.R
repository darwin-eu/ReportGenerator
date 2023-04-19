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
