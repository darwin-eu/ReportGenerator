#' incidenceRatePerYearPlot
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import ggplot2
#' @export
incidenceRatePerYearPlot <- function (incidence_estimates) {
  incidence_estimates %>%
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
}

#' incidenceRatePerYearGroupBySexPlot
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import ggplot2
#' @export
incidenceRatePerYearGroupBySexPlot <- function(incidence_estimates) {
  incidence_estimates %>%
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
}

#' incidenceRatePerYearColorByAgePlot
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import ggplot2
#' @export
incidenceRatePerYearColorByAgePlot <- function(incidence_estimates) {
  incidence_estimates %>%
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
}

#' incidenceRatePerYearFacetByDBAgeGroupPlot
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import ggplot2
#' @export
incidenceRatePerYearFacetByDBAgeGroupPlot <- function(incidence_estimates) {
  incidence_estimates %>%
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
}
