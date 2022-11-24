#' Extracts denominator data from files in CSV format.
#'
#' @return A tibble
#' @export
#'
denominatorData <- function () {

  result <- bind_rows(
    lapply(
      list.files(
        here("inst/csv/denominatorMockData"),
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
incidenceData <- function () {

  result <- bind_rows(
    lapply(
      list.files(
        here("inst/csv/incidenceMockResults"),
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>%
    mutate(age_strata = gsub(";", "-", age_strata))
  return(result)
}
#' Extracts prevalence data from files in CSV format.
#'
#' @return A tibble
#' @export
#'
prevalenceData <- function () {

  result <- bind_rows(
    lapply(
      list.files(
        here("inst/csv/prevalenceMockResults"),
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>%
    mutate(age_strata = gsub(";", "-", age_strata))
  return(result)
}
