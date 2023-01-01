#' Extracts denominator data from files in CSV format.
#'
#' @return A tibble
#' @export
#'
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
#' Extracts incidence data from files in CSV format.
#'
#' @return A tibble
#' @export
#'
incidenceExtraction <- function (importFolderIndcidence = here("inst/csv/incidenceMockResults")) {

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
#' Extracts prevalence data from files in CSV format.
#'
#' @return A tibble
#' @export
#'
prevalenceExtraction <- function (importFolderPrevalence = here("inst/csv/prevalenceMockResults")) {

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
