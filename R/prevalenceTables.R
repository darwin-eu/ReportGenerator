#' Table 3. Number of cases and total number of patients per database
#'
#' @param prevalenceData
#'
#' @return prevalenceTable3
#' @export
prevalenceTable3 <- function(prevalenceData) {

  prevalenceTableData <- prevalenceData %>%
    group_by(database_name) %>%
    summarise(numerator = sum(numerator),
              denominator = sum(denominator))

  return(prevalenceTableData)
}
#' Table 4. Numeric values of displayed figures 5-8
#'
#' @param prevalenceData
#'
#' @return table4Prevalence
#' @export
table4Prevalence <- function(prevalenceData) {

  table4Data <- prevalenceData %>%
    select(database_name,
           time,
           sex_strata,
           age_strata,
           numerator,
           denominator,
           prev) %>%
    mutate(prev = scales::percent(round(prev, 3)))

  table4Data <- table4Data[with(table4Data,
                                            order(database_name,
                                                  time,
                                                  sex_strata,
                                                  age_strata)),]

  colnames(table4Data) <- c("Database",
                                  "Time",
                                  "Sex",
                                  "Age group",
                                  "Number of cases",
                                  "Denominator population",
                                  "Prevalence")
  return(table4Data)
}
