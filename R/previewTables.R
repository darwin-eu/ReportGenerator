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

#' Create the table to choose item for preview.
#'
#' @param data the input data
createPreviewMenuTable <- function(data) {
  DT::datatable(data,
                escape = FALSE,
                extensions = 'RowReorder',
                options = list(dom = "t", rowReorder = FALSE),
                selection = list(mode = "single", target = "row"),
                rownames = FALSE,
                colnames = c("Items to select"))
}

#' Create preview table.
#'
#' @param data the input data
createPreviewTable <- function(data) {
  DT::datatable(data,
                options = list(pageLength = 10,
                               lengthMenu = c(5, 10, 15, 20)),
                rownames = FALSE)
}
#' table1NumParPreview
#'
#' @param incidence_attrition incidence of the attrition
#' @param prevalence_attrition prevalence of the attrition
#'
#' @import flextable dplyr
#' @importFrom huxtable as_hux
#' @export
table1NumParPreview <- function (incidence_attrition,
                                 prevalence_attrition) {

  if (!("reason_id" %in% names(prevalence_attrition))) {

    prevalence_attrition <- prevalence_attrition %>%
      mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                   reason == "Missing year of birth"  ~ 2,
                                   reason == "Missing sex"  ~ 3,
                                   reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                   reason == "No observation time available during study period"  ~ 5,
                                   reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                   reason == "Prior history requirement not fullfilled during study period"  ~ 7,
                                   reason == "Not Female"  ~ 8,
                                   reason == "Not Male"  ~ 8,
                                   reason == "No observation time available after applying age and prior history criteria"  ~ 10,
                                   reason == "Starting analysis population" ~ 11,
                                   reason == "Not observed during the complete database interval"  ~ 14,
                                   reason == "Do not satisfy full contribution requirement for an interval"  ~ 16,
                                   reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12),
             number_subjects = current_n,
             excluded_subjects = excluded)
    # unique(prevalence_attrition$reason)
  }
  tablePrevalenceAtt <- prevalence_attrition %>%
    group_by(reason_id,
             reason) %>%
    summarise(current_n = round(mean(number_subjects ), 0),
              excluded = round(mean(excluded_subjects ), 0)) %>%
    mutate(analysis_step = case_when(between(reason_id, 1, 8) ~ "initial",
                                     between(reason_id, 10, 16) ~ "prevalence"))
  tablePrevalenceAtt
  tableIncidenceAtt <- incidence_attrition %>%
    group_by(reason_id,
             reason) %>%
    summarise(current_n = round(mean(number_subjects ), 0),
              excluded = round(mean(excluded_subjects ), 0)) %>%
    mutate(analysis_step = case_when(between(reason_id, 1, 8) ~ "initial",
                                     between(reason_id, 10, 16) ~ "incidence"))
  table1Data <- union(tablePrevalenceAtt, tableIncidenceAtt)
  table1Data <- table1Data[,-1]
  table1Data <- table1Data %>%
    select(analysis_step, everything())
}
