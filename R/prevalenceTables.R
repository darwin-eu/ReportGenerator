#' Table 3. Number of cases and total number of patients per database
#'
#' @param prevalenceData
#'
#' @return prevalenceTable3
#' @export
prevalenceTable3 <- function(prevalenceData) {

  prevalenceTableData <- prevalenceData %>%
    group_by(database_name) %>%
    summarise(n_cases = sum(n_cases),
              n_population = sum(n_population))

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
           prevalence_start_date,
           denominator_sex,
           denominator_age_group,
           n_cases,
           n_population,
           prevalence) %>%
    mutate(prevalence = scales::percent(round(prevalence, 3)))

  table4Data <- table4Data[with(table4Data,
                                            order(database_name,
                                                  prevalence_start_date,
                                                  denominator_sex,
                                                  denominator_age_group)),]

  colnames(table4Data) <- c("Database",
                                  "prevalence_start_date",
                                  "Sex",
                                  "Age group",
                                  "Number of cases",
                                  "Denominator population",
                                  "Prevalence")
  return(table4Data)
}
