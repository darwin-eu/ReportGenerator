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

# Figure 5. Prevalence of drug/s use over calendar time (per month/year) overall
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
# Figure 6. by year/month: two plots – males/females, all databases
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
# Figure 7. by year/month: plot for each database, diff lines per age group
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
# Figure 8. by age group (x-axis) for databases (color) and sex (dashed/line)
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
utils::globalVariables(c("prevalence_95CI_lower",
                         "prevalence_95CI_upper"))
