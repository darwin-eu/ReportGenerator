#' Creates an HTML report taking the prevalence and incidence estimates from IncidencePrevalence.
#' @param title Title of the study in <chr>.
#' @param authors Name of the author in <chr>.
#' @param authorsInstitution Name of the institution performing the study in <chr>.
#' @param date Date relevant to the study in <chr>.
#' @param countries Country or countries that participated in the study.
#' @param abstractText Abstract in character.
# @param denominatorData A tibble from IncidencePrevalence.
# @param incidenceData A tibble from IncidencePrevalence.
# @param prevalenceData A tibble from IncidencePrevalence.
#' @param format A string. Options are "word", "pdf" or "html".
#' @param byCondition TRUE if study is related to a condition, FALSE if it is related to drug utilisation.
#' @export
#' @import dplyr CDMConnector rmarkdown here officer
#' @importFrom utils head globalVariables
#' @importFrom scales percent
#' @importFrom readr read_csv
#' @importFrom stats time
#' @return A WORD, PDF or HTML document.
incidencePrevalenceReport <- function(title = NULL,
                                      authors = NULL,
                                      authorsInstitution = NULL,
                                      date = NULL,
                                      countries = NULL,
                                      abstractText = NULL,
                                      # denominatorData = NULL,
                                      # incidenceData = NULL,
                                      # prevalenceData = NULL,
                                      byCondition = TRUE,
                                      format = "word") {


  ### Data extraction functions

  denominatorData <- denominatorExtraction()

  incidenceData <- incidenceExtraction()

  prevalenceData <- prevalenceExtraction()

  ### OBJECTS

  ## Specifications

  # Authors

  authorsTable <- data.frame(Authors = authors,
                             Institution = authorsInstitution)

  ## Incidence objects

  # Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>

  # Loading data from function

  incidenceTable1 <- incidenceTable1(incidenceData,
                                     denominatorData)

  # Correcting either drug or condition study

  if (byCondition == FALSE) {

    colnames(incidenceTable1) <- c("Database",
                                   "Number of new drug users",
                                   "Total number of drug users")

    # Sum of total patients in database

    totalPopulationIncidence <- sum(incidenceTable1$`Total number of drug users`)

    } else {

      colnames(incidenceTable1) <- c("Database",
                                   "Number of new patients",
                                   "Total number of patients")

      # Sum of total patients in database

      totalPopulationIncidence <- sum(incidenceTable1$`Total number of patients`)

      }

  # Table 1 paragraph

  # Year period

  minYearIncidence <- format(min(incidenceData$incidence_start_date),
                             format = "%Y")

  maxYearIncidence <- format(max(incidenceData$incidence_end_date),
                             format = "%Y")

  # Paragraph

  incidenceTitleIntroTable1 <- "Number of new cases during study period"

  incidenceIntroTable1 <- paste("Table 1 describes the total number of new events with at least one day of observation time during the study period. For this study, we investigated use of <outcome 1> in more than ",
                                totalPopulationIncidence,
                                " patients during the study period ",
                                minYearIncidence,
                                " to ",
                                maxYearIncidence,
                                " .",
                                sep = "")

  incidenceTitleTable1 <- "Table 1. Number of new cases and total number of patients per database"

  incidenceTextTable1 <- " "

  # Figure 1. Incidence rate/s of drug/s use over calendar time (per month/year) overall

  incidenceFigure1 <- incidenceFigure1(incidenceData = incidenceData)

  titleFigure1Incidence <- "Figure 1. Incidence rate over time overall"

  # Figure 2. by year/month: two plots – males/females, all databases

  incidenceFigure2 <- incidenceFigure2(incidenceData = incidenceData)

  titleFigure2Incidence <- "Figure 2. Incidence rate over time stratified by sex"

  # Figure 3 . by year/month: plot for each database, diff lines per age group

  incidenceFigure3 <- incidenceFigure3(incidenceData = incidenceData)

  titleFigure3Incidence <- "Figure 3. Incidence rate over time stratified by age group"

  # Figure 4 . by age group (x-axis) for databases (color) and sex (dashed/line)

  incidenceFigure4 <- incidenceFigure4(incidenceData = incidenceData)

  titleFigure4Incidence <- "Figure 4. Incidence rate over time stratified by sex and age group"

  # Table 2. Incidence data according to figures 1 and 2

  table2Incidence <- table2Incidence(incidenceData = incidenceData)

  titleTable2Incidence <- "Table 2. Numeric values of displayed figures 1-4"

  textTable2Incidence <- " "

  ## Prevalence objects

  # Table 3. Number of cases and total number of patients per database

  prevalenceTable3 <- prevalenceTable3(prevalenceData = prevalenceData)

  if (byCondition == FALSE) {

    colnames(prevalenceTable3) <- c("Database",
                                   "Number of drug users",
                                   "Total number of drug users per database")

    # Sum of total patients in database

    totalPopulationPrevalence <- sum(prevalenceTable3$`Total number of drug users`)

    } else {

    colnames(prevalenceTable3) <- c("Database",
                                   "Number of patients ",
                                   "Total number of patients")

    # Sum of total patients in database

    totalPopulationPrevalence <- sum(prevalenceTable3$`Total number of patients`)

    }

  # Table 3 paragraph

  # Year period

  minYearPrevalence <- format(min(prevalenceData$prevalence_start_date),
                             format = "%Y")

  maxYearPrevalence <- format(max(prevalenceData$prevalence_end_date),
                             format = "%Y")

  # Paragraph

  prevalenceTitleIntroTable3 <- "Number of cases during study period"

  prevalenceIntroTable3 <- paste("Table 3 describes the total number of patients with a condition or drug use time during the study period. For this study, we investigated use of <outcome 1> in more than ",
                                totalPopulationPrevalence,
                                " patients during the study period ",
                                minYearPrevalence,
                                " to ",
                                maxYearPrevalence,
                                " .",
                                sep = "")

  prevalenceTitleTable3 <- "Table 3. Number of cases and total number of patients per database"

  prevalenceTextTable3 <- " "

  # Figure 5. Prevalence of drug/s use over calendar time (per month/year) overall

  prevalenceFigure5 <- prevalenceFigure5(prevalenceData = prevalenceData)

  titleFigure5Prevalence <- "Figure 5. Prevalence over time overall"

  # Figure 6. by year/month: two plots – males/females, all databases

  prevalenceFigure6 <- prevalenceFigure6(prevalenceData = prevalenceData)

  titleFigure6Prevalence <- "Figure 6. Prevalence over time stratified by sex"

  # Figure 7. by year/month: plot for each database, diff lines per age group

  prevalenceFigure7 <- prevalenceFigure7(prevalenceData = prevalenceData)

  titleFigure7Prevalence <- "Figure 7. Prevalence over time stratified by age group"

  # Figure 8. by age group (x-axis) for databases (color) and sex (dashed/line)

  prevalenceFigure8 <- prevalenceFigure8(prevalenceData = prevalenceData)

  titleFigure8Prevalence <- "Figure 8. Prevalence over time stratified by sex and age group"

  # Table 4. Numeric values of displayed figures 5-8

  table4Prevalence <- table4Prevalence(prevalenceData = prevalenceData)

  titletable4Prevalence <- "Table 4. Numeric values of displayed figures 5-8"

  texttable4Prevalence <- " "

  # Table of Contents

  tableContentsTitle <- "Table of Contents"

  ### REPORT GENERATION

  if (format == "word") {
    if (!file.exists(here("Reports/"))) {
      dir.create("Reports/")
    }

    incidencePrevalenceDocx <- read_docx(path = paste0(system.file(package = "ReportGenerator"),
                                             "/templates/word/darwinTemplate.docx")) %>%

      ## Sections

      ## Introduction

      body_add(value = title,
               style = "Title") %>%
      body_add_table(authorsTable,
                     style = "Authors") %>%
      body_add(value = abstractText,
               style = "heading 2") %>%

      ## Table of Contents

      body_add(value = tableContentsTitle,
               style = "tableContents") %>%
      body_add(value = incidenceTitleTable1,
               style = "Normal") %>%
      body_add(value = titleFigure1Incidence,
               style = "Normal") %>%
      body_add(value = titleFigure2Incidence,
               style = "Normal") %>%
      body_add(value = titleFigure3Incidence,
               style = "Normal") %>%
      body_add(value = titleFigure4Incidence,
               style = "Normal") %>%
      body_add(value = titleTable2Incidence,
               style = "Normal") %>%
      body_add(value = prevalenceTitleTable3,
               style = "Normal") %>%
      body_add(value = titleFigure5Prevalence,
               style = "Normal") %>%
      body_add(value = titleFigure6Prevalence,
               style = "Normal") %>%
      body_add(value = titleFigure7Prevalence,
               style = "Normal") %>%
      body_add(value = titleFigure8Prevalence,
               style = "Normal") %>%
      body_add(value = titletable4Prevalence,
               style = "Normal") %>%

      ## Incidence

      # Table 1 Incidence


      body_add(incidenceTitleIntroTable1,
               style = "heading 1") %>%
      body_add(incidenceIntroTable1,
               style = "Normal") %>%
      body_add(value = incidenceTitleTable1,
               style = "caption") %>%
      body_add_table(incidenceTable1,
                     style = "Table Grid") %>%
      body_add(incidenceTextTable1,
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


      ## Prevalence

      # Table 3 Prevalence

      body_add(prevalenceTitleIntroTable3,
               style = "heading 1") %>%
      body_add(prevalenceIntroTable3,
               style = "Normal") %>%
      body_add(value = prevalenceTitleTable3,
               style = "caption") %>%
      body_add_table(prevalenceTable3,
                     style = "Table Grid") %>%
      body_add(prevalenceTextTable3,
               style = "Normal") %>%

      # Figure 5 Prevalence

      body_add(value = titleFigure5Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure5,
                  style = "Normal") %>%

      # Figure 6 Prevalence

      body_add(value = titleFigure6Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure6,
                  style = "Normal") %>%

      # Figure 7 Prevalence

      body_add(value = titleFigure7Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure7,
                  style = "Normal") %>%

      # Figure 8 Prevalence

      body_add(value = titleFigure8Prevalence,
               style = "heading 1") %>%
      body_add_gg(value = prevalenceFigure8,
                  style = "Normal") %>%

      # Table 4 Prevalence
      body_add(value = titletable4Prevalence,
               style = "caption") %>%
      body_add_table(table4Prevalence,
                     style = "Table Grid") %>%
      body_add(texttable4Prevalence,
               style = "Normal") %>%

      print(incidencePrevalenceDocx,
            target = here("Reports/IncidencePrevalenceReport.docx"))

  } else {

    if (!file.exists(here("Reports/"))) {

      dir.create("Reports")

    }

    rmarkdown::render(

      input = paste0(system.file(package = "IncidencePrevalenceReport"),
                     "/rmarkdown/templates/IncidencePrevalenceReport.Rmd"),
      output_format = "html_document",
      output_file = here("Reports/IncidencePrevalenceReport"),
      # params = list(titleParam = title,
      #               authorParam = authors,
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
                                                          "prev_low",
                                                          "subject_id",
                                                          "n_events"))





