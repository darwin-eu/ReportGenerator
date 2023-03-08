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
