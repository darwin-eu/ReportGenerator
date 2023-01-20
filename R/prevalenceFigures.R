#' Figure 5. Prevalence of drug/s use over calendar time (per month/year) overall
#'
#' @param prevalenceData
#'
#' @return prevalenceFigureData
#' @export
prevalenceFigure5 <- function(prevalenceData) {

  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group == "0-99") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               col = database_name)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0, 1)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    geom_errorbar(aes(ymin = prevalence_95CI_lower,
                      ymax = prevalence_95CI_upper)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name")

  return(prevalenceFigureData)
}
#' Figure 6. by year/month: two plots – males/females, all databases
#'
#' @param prevalenceData
#'
#' @return prevalenceFigure6
#' @export
prevalenceFigure6 <- function(prevalenceData) {

  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group == "0-99",
           denominator_sex != "Both") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = denominator_sex,
               col = database_name)) +
    facet_grid(cols = vars(denominator_sex)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0, 1)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence ",
         col = "Database name")

  return(prevalenceFigureData)
}
#' Figure 7. by year/month: plot for each database, diff lines per age group
#'
#' @param prevalenceData
#'
#' @return prevalenceFigure7
#' @export
prevalenceFigure7 <- function(prevalenceData) {
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group != "0-99",
           denominator_sex == "Both") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence)) +
    facet_grid(rows = vars(database_name)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0, 1)) +
    geom_line(aes(colour = denominator_age_group)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         colour = "Age group")

return(prevalenceFigureData)
}
#' Figure 8. by age group (x-axis) for databases (color) and sex (dashed/line)
#'
#' @param prevalenceData
#'
#' @return prevalenceFigure8
#' @export
prevalenceFigure8 <- function(prevalenceData) {
  prevalenceFigureData <- prevalenceData %>%
    filter(denominator_age_group != "0-99",
           denominator_sex != "Both") %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               col = database_name)) +
    facet_grid(rows = vars(database_name),
               cols = vars(denominator_age_group)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0, 1)) +
    geom_line(aes(linetype = denominator_sex)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name",
         linetype = "Sex") +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1))

  return(prevalenceFigureData)
}
