#' Table 1 Paragraph. Incidence rate/s of drug/s use over calendar incidence_start_date (per month/year) overall
#'
#' @param indcidenceData
#' @export
#' @import dplyr CDMConnector rmarkdown here officer
#' @importFrom utils head globalVariables
#' @importFrom scales percent
#' @importFrom readr read_csv
#' @importFrom stats time
#' @return table1Paragraph
#' @export
table1IncidenceParagraph <- function(indcidenceData) {

  incidenceTable1 <- incidenceTable1(incidenceData)

  totalPopulationIncidence <- sum(incidenceTable1$total)

  minYearIncidence <- format(min(incidenceData$incidence_start_date),
                             format = "%Y")

  maxYearIncidence <- format(max(incidenceData$incidence_end_date),
                             format = "%Y")

  incidenceTitleIntroTable1 <- "Number of new cases during study period"

  incidenceIntroTable1 <- paste("Table 1 describes the total number of new events with at least one day of observation time during the study period. For this study, we investigated use of <outcome 1> in more than ",
                                totalPopulationIncidence,
                                " patients during the study period ",
                                minYearIncidence,
                                " to ",
                                maxYearIncidence,
                                " .",
                                sep = "")

  return(incidenceIntroTable1)

}
