#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#'
#' @param studyTitle A character title of the study.
#' @param studyAuthor A character name of the author.
#' @param denominatorData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param word If TRUE generates a Word document report.
#' @export
#' @import dplyr CDMConnector rmarkdown here officer
#' @return An HTML document
reportIncidencePrevalence <- function(studyTitle,
                                      studyAuthor,
                                      denominatorData,
                                      prevalenceData,
                                      word = TRUE) {

  prevalenceTable <- prevalenceData$prevalence_estimates %>% select(time,
                                                                    numerator,
                                                                    denominator,
                                                                    prev)

  prevalenceGraph <- prevalenceData$prevalence_estimates %>%
    left_join(prevalenceData$analysis_settings,
              by = "prevalence_analysis_id") %>%
    left_join(denominator$denominator_settings,
              by = c("cohort_id_denominator_pop" = "cohort_definition_id")) %>%
    ggplot(aes(start_time, prev))+
    facet_grid(age_strata ~ sex_strata)+
    geom_bar(stat='identity') +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,NA))+
    theme_bw()

  prevalenceDoc <- read_docx(path = paste0(system.file(package = "IncidencePrevalenceReport"),
                                       "/rmarkdown/templates/IncidencePrevalenceReport.docx")) %>%
    body_add_table(head(prevalenceTable, n = 20), style = "Normal Table") %>%
    body_add_break() %>%
    body_add_gg(value = prevalenceGraph, style = "Normal")


  print(prevalenceDoc, target = here("Reports/IncidencePrevalenceReport.docx"))

  if (word == TRUE) {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }
    ## Renders docx with officeR

    ## Renders docx with rmarkdown function
    # rmarkdown::render(
    #   input = paste0(system.file(package = "IncidencePrevalenceReport"),
    #                  "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
    #   output_format = word_document(reference_docx = paste0(system.file(package = "IncidencePrevalenceReport"),
    #                                                         "/rmarkdown/templates/IncidencePrevalenceReport.docx")),
    #   output_file = here("Reports/IncidencePrevalenceReport"),
    #   encoding = 'UTF-8'
    # )
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



