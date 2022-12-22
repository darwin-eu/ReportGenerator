#' Figure 1. Incidence rate/s of drug/s use over calendar incidence_start_date (per month/year) overall
#'
#' @param indcidenceData
#'
#' @return incidenceFigure1
#' @export
incidenceFigure1 <- function(indcidenceData) {

  incidenceFigureData <- incidenceData %>%
    filter(denominator_sex == "Both",
           denominator_age_group == "0-99") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               col = database_name)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower,
                      ymax = incidence_100000_pys_95CI_upper)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name")

  return(incidenceFigureData)
}
#' Figure 2. by year/month: two plots – males/females, all databases
#'
#' @param indcidenceData
#'
#' @return incidenceFigure2
#' @export
incidenceFigure2 <- function(indcidenceData) {

  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group == "0-99",
           denominator_sex != "Both") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               group = denominator_sex,
               col = database_name)) +
    facet_grid(cols = vars(denominator_sex)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name")

  return(incidenceFigureData)
}
#' Figure 3 . by year/month: plot for each database, diff lines per age group
#'
#' @param indcidenceData
#'
#' @return incidenceFigure3
#' @export
incidenceFigure3 <- function(indcidenceData) {

  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group != "0-99",
           denominator_sex == "Both") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys)) +
    facet_grid(rows = vars(database_name)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line(aes(colour = denominator_age_group)) +
  geom_point() +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       colour = "Age group")

  return(incidenceFigureData)
}
#' Figure 4 . by age group (x-axis) for databases (color) and sex (dashed/line)
#'
#' @param indcidenceData
#'
#' @return incidenceFigure4
#' @export
incidenceFigure4 <- function(incidenceData) {

  incidenceFigureData <- incidenceData %>%
    filter(denominator_age_group != "0-99",
           denominator_sex != "Both") %>%
    ggplot(aes(x = incidence_start_date,
               y = incidence_100000_pys,
               col = database_name)) +
    facet_grid(rows = vars(database_name),
               cols = vars(denominator_age_group)) +
    # scale_y_continuous(labels = scales::percent,
    #                    limits = c(0,NA)) +
    geom_line(aes(linetype = denominator_sex)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Incidence rate per 100000 person-years",
         col = "Database name",
         linetype = "Sex") +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1))

  return(incidenceFigureData)
}
