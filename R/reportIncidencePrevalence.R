#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#'
#' @param studyTitle A character title of the study.
#' @param studyAuthor A character name of the author.
#' @param denominatorData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param word If TRUE generates a Word document report.
#' @export
#' @import dplyr CDMConnector rmarkdown here
#' @return An HTML document
reportIncidencePrevalence <- function(studyTitle,
                                      studyAuthor,
                                      denominatorData,
                                      prevalenceData,
                                      word = TRUE) {

  if (word == TRUE) {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }
    rmarkdown::render(
      input = paste0(system.file(package = "IncidencePrevalenceReport"),
                     "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
      output_format = word_document(reference_docx = paste0(system.file(package = "IncidencePrevalenceReport"),
                                                            "/rmarkdown/templates/IncidencePrevalenceReport.docx")),
      output_file = here("Reports/IncidencePrevalenceReport"),
      encoding = 'UTF-8'
    )
  } else {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports")
    }
    rmarkdown::render(
      input = paste0(system.file(package = "IncidencePrevalenceReport"),
                     "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
      output_format = "html_document",
      output_file = here("Reports/IncidencePrevalenceReport"),
      # params = list(titleParam = studyTitle,
      #               authorParam = studyAuthor,
      #               prevalenceParam = prevalenceData,
      #               incidenceParam = incidenceData),
      encoding = 'UTF-8'
    )
  }
}



