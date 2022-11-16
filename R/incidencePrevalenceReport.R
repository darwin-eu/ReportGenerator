#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#' @param studyTitle Title of the study.
#' @param studyAuthor Name of the author.
#' @param abstractText Abstract in character.
# @param denominatorData A tibble from IncidencePrevalence.
# @param incidenceData A tibble from IncidencePrevalence.
# @param prevalenceData A tibble from IncidencePrevalence.
#' @param format Output options are "word", "pdf" or "html".
#' @export
#' @import dplyr CDMConnector rmarkdown here officer scales
#' @importFrom utils head
#' @importFrom stats time
#' @return A WORD, PDF or HTML document.
incidencePrevalenceReport <- function(studyTitle = "...",
                                      studyAuthor = "...",
                                      abstractText = "...",
                                      # denominatorData = NULL,
                                      # incidenceData = NULL,
                                      # prevalenceData = NULL,
                                      format = "word") {

  # Incidence report data

  titleFigureIncidence <- "Figure 1. Incidence over time"
  titleTableIncidence <- "Incidence table"
  textIncidenceTable <- "Automatic text"
#
#   incidenceEstimates <- incidenceData$incidence_estimates
#
#   incidenceTable <- incidenceEstimates %>% select(Time = "time",
#                                                   `Number of persons` = "n_persons",
#                                                   `Person days`  = "person_days",
#                                                   `Incidence rate / 100000` = "ir_100000_pys")
  # Loading data from CSV

  incidenceData <- bind_rows(
    lapply(
      list.files(
        here("inst/csv/incidenceMockResults"),
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  )

  prevalenceData <- bind_rows(
    lapply(
      list.files(
        here("inst/csv/prevalenceMockResults"),
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  )

  # Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>

  # New drug users / patients

  incidenceTable1 <- incidenceData %>%
    group_by(incidenceData$database_name) %>%
    summarise(`Number of new <Drug users / Patients with condition X>` = sum(n_persons))

  # Total number of drug users / patients

  prevalenceTable1 <- prevalenceData %>%
    group_by(prevalenceData$database_name) %>%
    summarise(`Total number of <Drug users / Patients with condition X>` = sum(numerator))

  # For publication in report

  table1 <- left_join(incidenceTable1,
                      prevalenceTable1,
                      by=c("incidenceData$database_name" = "prevalenceData$database_name"))

  # Figure 1. Incidence rate/s of drug/s use over calendar time (per month/year) overall

  incidenceFigure1 <- incidenceData %>%
    filter(sex_strata == "Both",
           age_strata == "0;99") %>%
    ggplot(aes(x = time,
               y = ir_100000_pys,
               col = database_name)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    geom_errorbar(aes(ymin = ir_100000_pys_low,
                      ymax = ir_100000_pys_high)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name")

  # Figure 2. by year/month: two plots – males/females, all databases


  incidenceFigure2 <- incidenceData %>%
    filter(age_strata == "0;99",
           sex_strata != "Both") %>%
    ggplot(aes(x = time,
               y = ir_100000_pys,
               group = sex_strata,
               col = database_name)) +
    facet_grid(cols = vars(sex_strata)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name")

  # Figure 2b . by year/month: plot for each database, diff lines per age group

  incidenceFigure2b <- incidenceData %>%
    filter(age_strata != "0;99",
           sex_strata == "Both") %>%
    ggplot(aes(x = time,
               y = ir_100000_pys)) +
    facet_grid(rows = vars(database_name)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line(aes(colour = age_strata)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         colour = "Age group")

  # Figure 2c . by age group (x-axis) for databases (color) and sex (dashed/line)

  incidenceFigure2c <- incidenceData %>%
    filter(age_strata != "0;99",
           sex_strata != "Both") %>%
    ggplot(aes(x = time,
               y = ir_100000_pys,
               col = database_name)) +
    facet_grid(rows = vars(database_name),
               cols = vars(age_strata)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line(aes(linetype = sex_strata)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         linetype = "Sex")

  # line

  # incidenceGraph <- incidenceEstimates %>%
  #   left_join(incidence$analysis_settings,
  #             by = "incidence_analysis_id") %>%
  #   left_join(dpop$denominator_settings,
  #             by=c("denominator_id" = "cohort_definition_id")) %>%
  #   ggplot(aes(x = time, y = ir_100000_pys, group = sex_strata, col = sex_strata)) +
  #   facet_grid(cols = vars(age_strata)) +
  #   scale_y_continuous(labels = scales::percent,
  #                      limits = c(0,NA)) +
  #   geom_line() +
  #   geom_point() +
  #   theme_bw()


  # bar

  # incidenceGraph <- incidenceEstimates %>%
  #   left_join(incidence$analysis_settings,
  #             by = "incidence_analysis_id") %>%
  #   left_join(dpop$denominator_settings,
  #             by=c("denominator_id" = "cohort_definition_id")) %>%
  #   ggplot(aes(time, ir_100000_pys))+
  #   facet_grid(age_strata ~ sex_strata)+
  #   geom_bar(stat = "identity") +
  #   scale_y_continuous(labels = scales::percent,
  #                      limits = c(0,NA))+
  #   theme_bw()

  # Prevalence report data

  titleFigurePrevalence <- "Figure 1. Prevalence over time"
  titleTablePrevalence <- "Prevalence table"
  textPrevalenceTable <- "Automatic text"

  # prevalenceEstimates <- prevalenceData$prevalence_estimates
  #
  # prevalenceTable <- prevalenceEstimates %>% select(time = "time",
  #                                                   numerator = "numerator",
  #                                                   denominator = "denominator",
  #                                                   prev = "prev")
  #
  # prevalenceGraph <- prevalenceEstimates %>%
  #   left_join(prevalence$analysis_settings,
  #             by = "prevalence_analysis_id") %>%
  #   left_join(dpop$denominator_settings,
  #             by=c("denominator_id" = "cohort_definition_id")) %>%
  #   ggplot(aes(time, prev))+
  #   facet_grid(age_strata ~ sex_strata)+
  #   geom_bar(stat = "identity") +
  #   scale_y_continuous(labels = scales::percent,
  #                      limits = c(0,NA))+
  #   theme_bw()

  if (format == "word") {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }

    ## Renders docx with officeR

    incidencePrevalenceDocx <- read_docx(path = paste0(system.file(package = "reportGenerator"),
                                             "/rmarkdown/templates/IncidencePrevalenceReportM.docx")) %>%
      # Introduction section


      body_add(value = studyTitle, style = "Title") %>%
      body_add(value = studyAuthor, style = "Normal") %>%
      body_add(value = abstractText, style = "heading 1") %>%

      # Incidence section

      body_add(value = titleTableIncidence, style = "caption") %>%
      body_add_table(table1, style = "Table Grid") %>%
      body_add(textIncidenceTable, style = "Normal") %>%
      body_add(value = titleFigureIncidence, style = "heading 1") %>%
      body_add_gg(value = incidenceFigure1, style = "Normal") %>%
      body_add_gg(value = incidenceFigure2, style = "Normal") %>%
      body_add_gg(value = incidenceFigure2b, style = "Normal") %>%
      body_add_gg(value = incidenceFigure2c, style = "Normal") %>%

      # Prevalence section
      # body_add(value = titleTablePrevalence, style = "caption") %>%
      # body_add_table(head(prevalenceTable, n = 20), style = "Table Grid") %>%
      # body_add(textPrevalenceTable, style = "Normal") %>%
      # body_add(value = titleFigurePrevalence, style = "heading 1") %>%
      # body_add_gg(value = prevalenceGraph, style = "Normal")


    print(incidencePrevalenceDocx, target = here("Reports/IncidencePrevalenceReport.docx"))

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



