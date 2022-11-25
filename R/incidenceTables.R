#' Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>
#'
#' @param incidenceData
#'
#' @return incidenceTable1
#' @export
incidenceTable1 <- function(incidenceData = NULL) {

  incidenceTableData <- incidenceData %>%
    group_by(database_name) %>%
    summarise(total = sum(n_events))

  denominatorTable1 <- denominatorData %>%
    group_by(database_name) %>%
    summarise(subjects = length(unique(subject_id)))

  incidenceTableData <- left_join(incidenceTableData,
                               denominatorTable1,
                               by = "database_name")

  return(incidenceTableData)

}
#' Table 2. Incidence data according to figures 1 and 2
#'
#' @param incidenceData
#'
#' @return table2Incidence
#' @export
table2Incidence <- function(incidenceData) {

  table2Data <- incidenceData %>%
    select(database_name,
           time,
           sex_strata,
           age_strata,
           n_events,
           person_years,
           ir_100000_pys) %>%
    mutate(person_years = round(person_years, 2))

  table2Data <- table2Data[with(table2Data,
                                          order(database_name,
                                                time,
                                                sex_strata,
                                                age_strata)),]

  colnames(table2Data) <- c("Database",
                                 "Time",
                                 "Sex",
                                 "Age group",
                                 "N events",
                                 "Person-years",
                                 "IR 10000 pys")
  return(table2Data)
}
