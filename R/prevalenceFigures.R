#' Figure 5. Prevalence of drug/s use over calendar time (per month/year) overall
#'
#' @param prevalenceData
#'
#' @return prevalenceFigureData
#' @export
prevalenceFigure5 <- function(prevalenceData) {

  prevalenceFigureData <- prevalenceData %>%
    filter(sex_strata == "Both",
           age_strata == "0-99") %>%
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
    filter(age_strata == "0-99",
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
    filter(age_strata != "0-99",
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
    filter(age_strata != "0-99",
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
