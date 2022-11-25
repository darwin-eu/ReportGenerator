#' Title
#'
#' @param incidenceData
#'
#' @return
#' @export
#'
#' @examples
incidenceTable1 <- function(incidenceData) {

incidenceTable1 <- incidenceData %>%
  group_by(database_name) %>%
  summarise(total = sum(n_events))

denominatorTable1 <- denominatorData %>%
  group_by(database_name) %>%
  summarise(subjects = length(unique(subject_id)))

incidenceTable1 <- left_join(incidenceTable1,
                             denominatorTable1,
                             by = "database_name")

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

}
