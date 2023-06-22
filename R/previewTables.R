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

  # data <- data.frame(contents = c("Table - Number of participants",
  #                                 "Table - Number of participants by sex and age group",
  #                                 "Plot - Incidence rate per year"))

  checkboxes <- sprintf('<input type="checkbox" name="%s" value="%s"/>', data$contents, data$contents)

  data <- data %>% mutate(selection = checkboxes)

  DT::datatable(data,
                escape = FALSE,
                extensions = 'RowReorder',
                options = list(dom = "t", rowReorder = FALSE),
                selection = list(mode = "single", target = "row"),
                rownames = FALSE,
                colnames = c("Items to select", "Add to Word Report"))
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
#' @import here flextable dplyr
#' @importFrom huxtable as_hux
#' @export
table1NumParPreview <- function (incidence_attrition,
                                 prevalence_attrition) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))

  # incidence_attrition <- read_csv("Results/resultsMock_IPCI/test database_incidence_attrition_20230330.csv")
  #
  # prevalence_attrition <- read_csv("Results/resultsMock_IPCI/test database_prevalence_attrition_20230330.csv")

  # prevalence_attrition <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/networkResults/prevalence_attrition_CPRD GOLD.csv")

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

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


  # databaseNameInc <- unique(incidence_attrition$database_name)

  # databaseNameInc <- databaseNameInc[1:3]

  # tableIncidenceAtt <- incidence_attrition %>%
  #   filter(database_name == databaseNameInc[1],
  #          step == "Estimating incidence",
  #          analysis_interval == "years") %>%
  #   group_by(step,
  #            reason) %>%
  #   summarise(current_n = round(mean(current_n), 0),
  #             excluded = round(mean(excluded), 0))
  #
  # tablePrevIncData <- bind_rows(tablePrevalenceAtt, tableIncidenceAtt)
  #
  # for (i in databaseNamePrev[2:length(databaseNamePrev)]) {
  #
  #   subPrevalenceAtt <- prevalence_attrition %>%
  #     filter(database_name == i) %>%
  #     group_by(step,
  #              reason,
  #              current_n,
  #              excluded) %>%
  #     summarise()
  #
  #   subIncidenceAtt <- incidence_attrition %>%
  #     filter(database_name == i,
  #            step == "Estimating incidence",
  #            analysis_interval == "years") %>%
  #     group_by(step,
  #              reason) %>%
  #     summarise(current_n = round(mean(current_n), 0),
  #               excluded = round(mean(excluded), 0))
  #
  #   subPrevIncData <- bind_rows(subPrevalenceAtt, subIncidenceAtt)
  #
  #   subPrevalenceAtt <- subPrevIncData[, -c(1:2)]
  #
  #   tablePrevIncData <- bind_cols(tablePrevIncData,
  #                                 subPrevalenceAtt)
  #
  # }
  #
  # headerNames <- gsub("\\..*","", names(tablePrevIncData))
  #
  # # headerNames
  #
  # subtitles <- c(" ", databaseNamePrev)
  #
  # subtitlesHeader <- c()
  #
  # for (i in subtitles) {
  #
  #   subtitlesHeader <- c(subtitlesHeader, i, " ")
  #
  #
  # }
  #
  # huxTableAtt <- as_hux(tablePrevIncData)
  #
  # lengthNames <- length(names(huxTableAtt))
  #
  # huxTableAtt <- huxTableAtt %>%
  #   set_contents(1, 1:lengthNames, headerNames)
  #
  # huxTableAtt <- huxTableAtt %>%
  #   insert_row(subtitlesHeader, after = 0)
  #
  # for (i in seq(1, lengthNames, 2)) {
  #
  #   val <- (i+1)
  #
  #   huxTableAtt <- huxTableAtt %>% merge_cells(1, i:val)
  #
  # }
  #
  # huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")
  #
  # return(huxTableAtt)


  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]
  #
  # tablePrevalenceAtt <- prevalence_attrition %>%
  #   filter(database_name == databaseNamePrev[1]) %>%
  #   group_by(step,
  #            reason,
  #            current_n,
  #            excluded) %>%
  #   summarise()
  #
  # databaseNameInc <- unique(incidence_attrition$database_name)
  #
  # # databaseNameInc <- databaseNameInc[1:3]
  #
  # tableIncidenceAtt <- incidence_attrition %>%
  #   filter(database_name == databaseNameInc[1],
  #          step == "Estimating incidence",
  #          analysis_interval == "years") %>%
  #   group_by(step,
  #            reason) %>%
  #   summarise(current_n = round(mean(current_n), 0),
  #             excluded = round(mean(excluded), 0))
  #
  # tablePrevIncData <- bind_rows(tablePrevalenceAtt, tableIncidenceAtt)
  #
  # for (i in databaseNamePrev[2:length(databaseNamePrev)]) {
  #
  #   subPrevalenceAtt <- prevalence_attrition %>%
  #     filter(database_name == i) %>%
  #     group_by(step,
  #              reason,
  #              current_n,
  #              excluded) %>%
  #     summarise()
  #
  #   subIncidenceAtt <- incidence_attrition %>%
  #     filter(database_name == i,
  #            step == "Estimating incidence",
  #            analysis_interval == "years") %>%
  #     group_by(step,
  #              reason) %>%
  #     summarise(current_n = round(mean(current_n), 0),
  #               excluded = round(mean(excluded), 0))
  #
  #   subPrevIncData <- bind_rows(subPrevalenceAtt, subIncidenceAtt)
  #
  #   subPrevalenceAtt <- subPrevIncData[, -c(1:2)]
  #
  #   tablePrevIncData <- bind_cols(tablePrevIncData,
  #                                 subPrevalenceAtt)
  #
  # }
  #
  # headerNames <- gsub("\\..*","", names(tablePrevIncData))
  #
  # # headerNames
  #
  # subtitles <- c(" ", databaseNamePrev)
  #
  # subtitlesHeader <- c()
  #
  # for (i in subtitles) {
  #
  #   subtitlesHeader <- c(subtitlesHeader, i, " ")
  #
  #
  # }
  #
  # huxTableAtt <- as_hux(tablePrevIncData)
  #
  # lengthNames <- length(names(huxTableAtt))
  #
  # huxTableAtt <- huxTableAtt %>%
  #   set_contents(1, 1:lengthNames, headerNames)
  #
  # huxTableAtt <- huxTableAtt %>%
  #   insert_row(subtitlesHeader, after = 0)
  #
  # for (i in seq(1, lengthNames, 2)) {
  #
  #   val <- (i+1)
  #
  #   huxTableAtt <- huxTableAtt %>% merge_cells(1, i:val)
  #
  #   }
  #
  # huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")
  #
  # return(huxTableAtt)
}
