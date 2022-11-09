#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#'
#' @param studyTitle Title of the study.
#' @param studyAuthor Name of the author.
#' @param abstractText Abstract in character.
#' @param denominatorData A tibble from IncidencePrevalence.
#' @param incidenceData A tibble from IncidencePrevalence.
#' @param prevalenceData A tibble from IncidencePrevalence.
#' @param format Output options are "word", "pdf" or "html".
#' @export
#' @import dplyr CDMConnector rmarkdown here officer scales
#' @importFrom utils head
#' @return A WORD, PDF or HTML document.
incidencePrevalenceReport <- function(studyTitle,
                                      studyAuthor,
                                      abstractText,
                                      denominatorData,
                                      incidenceData,
                                      prevalenceData,
                                      format = "word") {

  titleFigurePrevalence1 <- "Figure 1. Incidence over time"
  titleTablePrevalence1 <- "Prevalence table"

  textPrevalenceTable <- "Automatic text"
  prevalence_estimates <- prevalenceData$prevalence_estimates

  prevalenceTable <- prevalence_estimates %>% select(time = "time",
                                                     numerator = "numerator",
                                                     denominator = "denominator",
                                                     prev = "prev")

  prevalenceGraph <- prevalence$prevalence_estimates %>%
    left_join(prevalence$analysis_settings,
              by = "prevalence_analysis_id") %>%
    left_join(dpop$denominator_settings,
              by=c("denominator_id" = "cohort_definition_id")) %>%
    ggplot(aes(time, prev))+
    facet_grid(age_strata ~ sex_strata)+
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,NA))+
    theme_bw()

  if (format == "word") {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }

    ## Renders docx with officeR

    prevalenceDoc <- read_docx(path = paste0(system.file(package = "reportGenerator"),
                                             "/rmarkdown/templates/IncidencePrevalenceReportM.docx")) %>%
      body_add(value = studyTitle, style = "Title") %>%
      body_add(value = studyAuthor, style = "Normal") %>%
      body_add(value = abstractText, style = "heading 1") %>%
      body_add(value = titleTablePrevalence1, style = "caption") %>%
      body_add_table(head(prevalenceTable, n = 20), style = "Table Grid") %>%
      body_add(textPrevalenceTable, style = "Normal") %>%
      body_add(value = titleFigurePrevalence1, style = "heading 1") %>%
      body_add_gg(value = prevalenceGraph, style = "Normal")


    print(prevalenceDoc, target = here("Reports/IncidencePrevalenceReport.docx"))

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



