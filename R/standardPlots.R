# Copyright 2023 DARWIN EU®
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Incidence Rate Per Year Plot
#'
#' @param incidence_estimates estimates of of the incidence
#' @param type plot type
#'
#' @import ggplot2
#' @export
incidenceRatePerYearPlot <- function(incidence_estimates, type) {
  if (type == "Facet by outcome") {
    incidence_estimates %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 col = database_name)) +
      geom_line(aes(group = 1)) +
      geom_point() +
      geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower,
                        ymax = incidence_100000_pys_95CI_upper)) +
      facet_wrap(~outcome_cohort_name) +
      theme_bw() +
      labs(x = "Calendar year",
           y = "Incidence rate per 100000 person-years",
           col = "Database name")
  } else if (type == "Facet by database") {
    incidence_estimates %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 col = outcome_cohort_name)) +
      geom_line(aes(group = 1)) +
      geom_point() +
      geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower,
                        ymax = incidence_100000_pys_95CI_upper)) +
      facet_wrap(~database_name) +
      theme_bw() +
      labs(x = "Calendar year",
           y = "Incidence rate per 100000 person-years",
           col = "Outcome")
  }
}
#' Incidence Rate Per Year by Sex Plot
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import ggplot2
#' @param type plot type
#' @export
incidenceRatePerYearGroupBySexPlot <- function(incidence_estimates, type) {
  result <- NULL
  if (type == "Facet by outcome") {
    result <- incidence_estimates %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 group = denominator_sex,
                 col = database_name)) +
      facet_wrap(~ denominator_sex + outcome_cohort_name) +
      geom_line() +
      geom_point() +
      theme_bw() +
      labs(x = "Calendar year",
           y = "Incidence rate per 100000 person-years",
           col = "Database name")
  } else if (type == "Facet by database") {
    result <- incidence_estimates %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 group = denominator_sex,
                 col = outcome_cohort_name)) +
      facet_wrap(~ denominator_sex + database_name) +
      geom_line() +
      geom_point() +
      theme_bw() +
      labs(x = "Calendar year",
           y = "Incidence rate per 100000 person-years",
           col = "Outcome")
  }
  return(result)
}
#' Incidence Rate Per Year by Age Plot
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
#' Incidence Rate Per Year by Age Group
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
#' Prevalence Rate Per Year Plot
#'
#' @param prevalence_estimates estimates of of the prevalence
#'
#' @import ggplot2
#' @export
prevalenceRatePerYearPlot <- function (prevalence_estimates) {
  prevalence_estimates %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               col = database_name)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,NA)) +
    # facet_grid(rows = vars(outcome_cohort_id)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    geom_errorbar(aes(ymin = prevalence_95CI_lower,
                      ymax = prevalence_95CI_upper)) +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         col = "Database name")
}
#' Prevalence Rate Per Year by Sex Plot
#'
#' @param prevalence_estimates estimates of of the prevalence
#'
#' @import ggplot2
#' @export
prevalenceRatePerYearGroupBySexPlot <- function (prevalence_estimates) {
  prevalence_estimates %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               group = denominator_sex,
               col = database_name)) +
    facet_grid(cols = vars(denominator_sex)) +
    facet_grid(rows = vars(outcome_cohort_id)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence ",
         col = "Database name")
}
#' Prevalence Rate Per Year by Database
#'
#' @param prevalence_estimates estimates of of the prevalence
#'
#' @import ggplot2
#' @export
prevalenceRatePerYearFacetByDBOutcomePlot <- function (prevalence_estimates) {
  prevalence_estimates %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence)) +
    facet_grid(rows = vars(database_name)) +
    facet_grid(rows = vars(outcome_cohort_id)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA)) +
    geom_line(aes(colour = denominator_age_group)) +
    geom_point() +
    theme_bw() +
    labs(x = "Calendar year",
         y = "Prevalence",
         colour = "Age group")
}
#' Prevalence Rate Per Year by Age Group
#'
#' @param prevalence_estimates estimates of of the prevalence
#'
#' @import ggplot2
#' @export
prevalenceRatePerYearFacetByDBAgeGroupPlot <- function (prevalence_estimates) {
  prevalence_estimates %>%
    ggplot(aes(x = prevalence_start_date,
               y = prevalence,
               col = database_name)) +
    facet_grid(rows = vars(database_name),
               cols = vars(denominator_age_group)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, NA)) +
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
}
