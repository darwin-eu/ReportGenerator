#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#' @param studyTitle Title of the study.
#' @param studyAuthor Name of the author.
#' @param abstractText Abstract in character.
# @param denominatorData A tibble from IncidencePrevalence.
# @param incidenceData A tibble from IncidencePrevalence.
# @param prevalenceData A tibble from IncidencePrevalence.
#' @param format Output options are "word", "pdf" or "html".
#' @export
#' @import dplyr CDMConnector rmarkdown here officer
#' @importFrom utils head globalVariables
#' @importFrom scales percent
#' @importFrom readr read_csv
#' @importFrom stats time
#' @return A WORD, PDF or HTML document.
incidencePrevalenceReport <- function(studyTitle = "...",
                                      studyAuthor = "...",
                                      abstractText = "...",
                                      # denominatorData = NULL,
                                      # incidenceData = NULL,
                                      # prevalenceData = NULL,
                                      format = "word") {

  # Loading data from CSV files

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

  # Incidence objects

  # Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>

  # New drug users / patients

  incidenceTable1 <- incidenceData %>%
    group_by(database_name) %>%
    summarise(`Number of new cases` = sum(n_persons))

  # Total number of drug users / patients

  prevalenceTable1 <- prevalenceData %>%
    group_by(database_name) %>%
    summarise(`Total number of cases` = sum(numerator))

  # For publication in report

  table1 <- left_join(incidenceTable1,
                      prevalenceTable1,
                      by = "database_name")

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

  # Figure 3 . by year/month: plot for each database, diff lines per age group

  incidenceFigure3 <- incidenceData %>%
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

  # Figure 4 . by age group (x-axis) for databases (color) and sex (dashed/line)

  incidenceFigure4 <- incidenceData %>%
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

  # Table 2. Incidence data according to figures 1 and 2

  table2Incidence <- incidenceData %>%
    select(database_name,
           time,
           sex_strata,
           age_strata,
           n_persons,
           person_years,
           ir_100000_pys)

  table2Incidence <- table2Incidence[with(table2Incidence,
                                          order(database_name,
                                                time,
                                                sex_strata,
                                                age_strata)),]

  colnames(table2Incidence) <- c("Database",
                                 "Time",
                                 "Sex",
                                 "Age group",
                                 "N persons",
                                 "Person years",
                                 "IR 10000 pys")

  # Prevalence objects

  # Figure 1. Prevalence of drug/s use over calendar time (per month/year) overall

  prevalenceFigure1 <- prevalenceData %>%
    filter(sex_strata == "Both",
           age_strata == "0;99") %>%
    ggplot(aes(x = time,
               y = prev,
               col = database_name)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    geom_errorbar(aes(ymin = prev_low,
                      ymax = prev_high)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name")

  # Figure 2. by year/month: two plots – males/females, all databases

  prevalenceFigure2 <- prevalenceData %>%
    filter(age_strata == "0;99",
           sex_strata != "Both") %>%
    ggplot(aes(x = time,
               y = prev,
               group = sex_strata,
               col = database_name)) +
    facet_grid(cols = vars(sex_strata)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence ",
         col = "Database name")

  # Figure 3 . by year/month: plot for each database, diff lines per age group

  prevalenceFigure3 <- prevalenceData %>%
    filter(age_strata != "0;99",
           sex_strata == "Both") %>%
    ggplot(aes(x = time,
               y = prev)) +
    facet_grid(rows = vars(database_name)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    geom_line(aes(colour = age_strata)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         colour = "Age group")

  # Figure 4 . by age group (x-axis) for databases (color) and sex (dashed/line)

  prevalenceFigure4 <- prevalenceData %>%
    filter(age_strata != "0;99",
           sex_strata != "Both") %>%
    ggplot(aes(x = time,
               y = prev,
               col = database_name)) +
    facet_grid(rows = vars(database_name),
               cols = vars(age_strata)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    geom_line(aes(linetype = sex_strata)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         linetype = "Sex")

  # Table 2. Prevalence data according to figures 1 and 2

  table2Prevalence <- prevalenceData %>%
    select(database_name,
           time,
           sex_strata,
           age_strata,
           numerator,
           denominator,
           prev)

  table2Prevalence <- table2Prevalence[with(table2Prevalence,
                                            order(database_name,
                                                  time,
                                                  sex_strata,
                                                  age_strata)),]

  colnames(table2Prevalence) <- c("Database",
                                  "Time",
                                  "Sex",
                                  "Age group",
                                  "Numerator",
                                  "Denominator",
                                  "Prevalence")






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

  # prevalenceEstimates <- prevalence_estimates
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

  # Incidence report data

  # Table 1

  titleTable1Incidence <- "Table 1. Incidence"
  textTable1Incidence <- "Table 1. Automatic text"

  # Figures

  titleFigure1Incidence <- "Figure 1. Incidence over time"
  titleFigure2Incidence <- "Figure 2. Incidence over time"
  titleFigure3Incidence <- "Figure 3. Incidence over time"
  titleFigure4Incidence <- "Figure 4. Incidence over time"

  # Table 2

  titleTable2Incidence <- "Table 2. Incidence table"
  textTable2Incidence <- "Table 2. Automatic text"

  # Prevalence report data

  titleTable1Prevalence <- "Table 1. Prevalence"
  textTable1Prevalence <- "Table 2. Automatic text"

  titleFigure1Prevalence <- "Figure 1. Prevalence over time"
  titleFigure2Prevalence <- "Figure 2. Prevalence over time"
  titleFigure3Prevalence <- "Figure 3. Prevalence over time"
  titleFigure4Prevalence <- "Figure 4. Prevalence over time"

  titleTable2Prevalence <- "Prevalence table 2"
  textTable2Prevalence <- "Table 2. Automatic text"

  # Report generation with OfficeR

  if (format == "word") {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }

    incidencePrevalenceDocx <- read_docx(path = paste0(system.file(package = "reportGenerator"),
                                             "/rmarkdown/templates/IncidencePrevalenceReportM.docx")) %>%

      # Sections

      # Introduction

      body_add(value = studyTitle,
               style = "Title") %>%
      body_add(value = studyAuthor,
               style = "Normal") %>%
      body_add(value = abstractText,
               style = "heading 1") %>%

      # Incidence

      # Table 1 Incidence

      body_add(value = titleTable1Incidence,
               style = "caption") %>%
      body_add_table(table1,
                     style = "Table Grid") %>%
      body_add(textTable1Incidence,
               style = "Normal") %>%

      # Figure 1 Incidence

      body_add(value = titleFigure1Incidence,
               style = "heading 1") %>%
      body_add_gg(value = incidenceFigure1,
                  style = "Normal") %>%

      # Figure 2 Incidence

      body_add(value = titleFigure2Incidence,
               style = "heading 1") %>%
      body_add_gg(value = incidenceFigure2,
                  style = "Normal") %>%

      # Figure 3 Incidence

      body_add(value = titleFigure3Incidence,
               style = "heading 1") %>%
      body_add_gg(value = incidenceFigure3,
                  style = "Normal") %>%

      # Figure 4 Incidence

      body_add(value = titleFigure4Incidence,
               style = "heading 1") %>%
      body_add_gg(value = incidenceFigure4,
                  style = "Normal") %>%

      # Table 2 Incidence

      body_add(value = titleTable2Incidence,
               style = "caption") %>%
      body_add_table(table2Incidence,
                     style = "Table Grid") %>%
      body_add(textTable2Incidence,
               style = "Normal") %>%

      # Prevalence

      # Table 1 Prevalence

      body_add(value = titleTablePrevalence,
               style = "caption") %>%
      body_add_table(table1,
                     style = "Table Grid") %>%
      body_add(textPrevalenceTable,
               style = "Normal") %>%

      # Figure 1 Prevalence

      body_add(value = titleFigure1Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure1,
                  style = "Normal") %>%

      # Figure 2 Prevalence

      body_add(value = titleFigure2Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure2,
                  style = "Normal") %>%

      # Figure 3 Prevalence

      body_add(value = titleFigure3Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure3,
                  style = "Normal") %>%

      # Figure 4 Prevalence

      body_add(value = titleFigure4Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure4,
                  style = "Normal") %>%

      # Table 2 Prevalence
      body_add(value = titleTable2Prevalence,
               style = "caption") %>%
      body_add_table(table2Prevalence,
                     style = "Table Grid") %>%
      body_add(textTable2Prevalence,
               style = "Normal") %>%

      print(incidencePrevalenceDocx,
          target = here("Reports/IncidencePrevalenceReport.docx"))

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
if(getRversion() >= "2.15.1")    utils::globalVariables(c("age_strata",
                                                          "database_name",
                                                          "denominator",
                                                          "ir_100000_pys",
                                                          "ir_100000_pys_high",
                                                          "ir_100000_pys_low",
                                                          "n_persons",
                                                          "numerator",
                                                          "person_years",
                                                          "sex_strata",
                                                          "prev",
                                                          "prev_high",
                                                          "prev_low"))


