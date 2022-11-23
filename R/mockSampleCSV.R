#' This function uses data mockIncidencePrevalenceRef function to simulate three databases in CSV format to test the ReportGenerator
#' @param databaseName A vector that specifies the name of each database.
#' @export
#' @import dplyr rmarkdown here tidyr IncidencePrevalence
#' @importFrom utils head write.csv
#' @importFrom stats time
#' @return CSV files inside inst/csv
mockSampleCSV <- function(databaseName = c("Synthea",
                                           "IPCI",
                                           "CPRD")) {

cdm <- mockIncidencePrevalenceRef(sampleSize = 50000)

# databaseName <- c("Synthea", "IPCI", "CPRD")

for (i in databaseName) {
  print(i)

#   i <- "IPCI"

  dpop <- collectDenominator(cdm = cdm,
                             startDate  = as.Date("2000-01-01"),
                             endDate  = as.Date("2022-01-01"),
                             ageStrata  = list(c(20, 29),
                                               c(30, 39),
                                               c(40, 49),
                                               c(50, 59),
                                               c(60, 69),
                                               c(0,99)),
                             sexStrata  = c("Female", "Male", "Both"),
                             daysPriorHistory  = 365)

  cdm$denominator <- dpop$denominator_population %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/denominatorMockData"))) {
    dir.create("inst/csv/denominatorMockData")
  }

  write.csv(cdm$denominator,
            paste(here("inst/csv/denominatorMockData/denominatorMockData"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)



  incidence <- computeIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeId = "1",
    denominatorId  = c("1", "2", "3", "4", "5",
                       "6", "7", "8", "9", "10",
                       "11", "12", "13", "14",
                       "15", "16", "17", "18"),
    interval = c("Years"),
    outcomeWashout = 180
  )

  incidenceEstimates <- incidence$incidence_estimates %>%
    left_join(incidence$analysis_settings,
              by = "incidence_analysis_id") %>%
    left_join(dpop$denominator_settings,
              by=c("denominator_id" = "cohort_definition_id")) %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/incidenceMockResults"))) {
    dir.create("inst/csv/incidenceMockResults")
  }

  write.csv(incidenceEstimates,
            paste(here("inst/csv/incidenceMockResults/incidenceMockResults"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

  # i <- "IPCI"

  prevalence <- computePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorId  = c("1", "2", "3", "4", "5",
                       "6", "7", "8", "9", "10",
                       "11", "12", "13", "14",
                       "15", "16", "17", "18"),
    outcomeTable = "outcome",
    outcomeId  = "1",
    interval = "Years",
    type = "period",
    minCellCount = 0
  )

  prevalenceEstimates <- prevalence$prevalence_estimates %>%
    left_join(prevalence$analysis_settings,
              by = "prevalence_analysis_id") %>%
    left_join(dpop$denominator_settings,
              by=c("denominator_id" = "cohort_definition_id")) %>%
    mutate(database_name = i)

  if (!file.exists(here("inst/csv/prevalenceMockResults"))) {
    dir.create("inst/csv/prevalenceMockResults")
  }

  write.csv(prevalenceEstimates,
            paste(here("inst/csv/prevalenceMockResults/prevalenceMockResults"),
                  i,
                  ".csv",
                  sep = ""),
            row.names = FALSE)

}

}
