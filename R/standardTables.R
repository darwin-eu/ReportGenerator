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

#' `table1NumPar()` creates table 1 with number of participants from each database from attrition data
#'
#' @param incidence_attrition A dataframe with incidence attrition data.
#' @param prevalence_attrition A dataframe with prevalence attrition data.
#'
#' @import flextable dplyr
#' @importFrom huxtable as_hux set_contents insert_row set_align everywhere
#' @export
table1NumPar <- function(prevalence_attrition, incidence_attrition) {

  prevalence_attrition <- dataCleanAttrition(attrition = prevalence_attrition)
  incidence_attrition <- dataCleanAttrition(attrition = incidence_attrition)

  if (length(unique(prevalence_attrition$cdm_name)) == 1) {

    # Table data prevalence

    tablePrevalenceAtt <- prevalence_attrition %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ "prevalence")) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")


    tablePrevalenceAtt <- tablePrevalenceAtt[,-1]

    tablePrevalenceAtt <- tablePrevalenceAtt %>%
      select(analysis_step, everything())

    # tablePrevalenceAtt

    # Table data incidence

    tableIncidenceAtt <- incidence_attrition %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ "incidence")) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")

    tableIncidenceAtt <- tableIncidenceAtt[,-1]

    tableIncidenceAtt <- tableIncidenceAtt %>%
      select(analysis_step, everything())

    # tableIncidenceAtt

    tablePrevIncData <- union(tablePrevalenceAtt, tableIncidenceAtt)

    databaseName <- unique(incidence_attrition$cdm_name)

    headerNames <- gsub("\\..*","", names(tablePrevIncData))

    # headerNames

    subtitles <- c(" ", databaseName)

    subtitlesHeader <- c()

    for (i in subtitles) {

      subtitlesHeader <- c(subtitlesHeader, i, " ")


    }

    huxTableAtt <- as_hux(tablePrevIncData)

    lengthNames <- length(names(huxTableAtt))

    huxTableAtt <- huxTableAtt %>%
      set_contents(1, 1:lengthNames, headerNames)

    huxTableAtt <- huxTableAtt %>%
      insert_row(subtitlesHeader, after = 0)

    huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")

    # huxTableAtt

    return(huxTableAtt)

  } else {

    # Table data prevalence

    databaseNamePrev <- unique(prevalence_attrition$cdm_name)

    # databaseNamePrev <- databaseNamePrev[1:3]

    tablePrevalenceAtt <- prevalence_attrition %>%
      filter(cdm_name == databaseNamePrev[1]) %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ "prevalence")) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")


    tablePrevalenceAtt <- tablePrevalenceAtt[,-1]

    tablePrevalenceAtt <- tablePrevalenceAtt %>%
      select(analysis_step, everything())

    # tablePrevalenceAtt

    # Table data incidence

    databaseNameInc <- unique(incidence_attrition$cdm_name)

    # databaseNameInc <- databaseNameInc[1:3]

    tableIncidenceAtt <- incidence_attrition %>%
      filter(cdm_name == databaseNameInc[1]) %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ "incidence")) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")

    tableIncidenceAtt <- tableIncidenceAtt[,-1]

    tableIncidenceAtt <- tableIncidenceAtt %>%
      select(analysis_step, everything())

    # tableIncidenceAtt

    # Union

    tablePrevIncData <- union(tablePrevalenceAtt, tableIncidenceAtt)

    # tablePrevIncData

    # for (i in databaseNamePrev[2:3]) {
    for (i in databaseNamePrev[2:length(databaseNamePrev)]) {

      subPrevalenceAtt <- prevalence_attrition %>%
        filter(cdm_name == i) %>%
        group_by(reason_id,
                 reason) %>%
        summarise(current_n = round(mean(number_subjects ), 0),
                  excluded = round(mean(excluded_subjects ), 0)) %>%
        mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                         between(reason_id, 10, 16) ~ "prevalence")) %>%
        filter(reason != "Do not satisfy full contribution requirement for an interval")


      subPrevalenceAtt <- subPrevalenceAtt[,-1]

      subPrevalenceAtt <- subPrevalenceAtt %>%
        select(analysis_step, everything())

      # subPrevalenceAtt

      subIncidenceAtt <- incidence_attrition %>%
        filter(cdm_name == i) %>%
        group_by(reason_id,
                 reason) %>%
        summarise(current_n = round(mean(number_subjects ), 0),
                  excluded = round(mean(excluded_subjects ), 0)) %>%
        mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                         between(reason_id, 10, 16) ~ "incidence")) %>%
        filter(reason != "Do not satisfy full contribution requirement for an interval")

      subIncidenceAtt <- subIncidenceAtt[,-1]

      subIncidenceAtt <- subIncidenceAtt %>%
        select(analysis_step, everything())

      # subIncidenceAtt

      subPrevIncData <- union(subPrevalenceAtt, subIncidenceAtt)


      subPrevalenceAtt <- subPrevIncData[, -c(1:2)]

      tablePrevIncData <- bind_cols(tablePrevIncData,
                                    subPrevalenceAtt)

    }

    names(tablePrevIncData)

    headerNames <- gsub("\\..*","", names(tablePrevIncData))

    # headerNames

    subtitles <- c(" ", databaseNamePrev)

    subtitlesHeader <- c()

    for (i in subtitles) {

      subtitlesHeader <- c(subtitlesHeader, i, " ")


    }

    huxTableAtt <- as_hux(tablePrevIncData)

    lengthNames <- length(names(huxTableAtt))

    huxTableAtt <- huxTableAtt %>%
      set_contents(1, 1:lengthNames, headerNames)

    huxTableAtt <- huxTableAtt %>%
      insert_row(subtitlesHeader, after = 0)

    huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")



    # class(huxTableAtt)

    return(huxTableAtt)

  }

}
#' `table1IncAtt()` creates table 1 with number of participants from each database from attrition data
#'
#' @param attritionData A dataframe with attrition data.
#' @param attritionDataType Either "incidence" or prevalence
#'
#' @import flextable dplyr
#' @importFrom huxtable as_hux set_contents insert_row set_align everywhere
#' @export
table1Att <- function(attritionData, attritionDataType) {

  attritionData <- dataCleanAttrition(attrition = attritionData)

  if (length(unique(attritionData$cdm_name)) == 1) {
    # Table data incidence
    tableAtt <- attritionData %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ attritionDataType)) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")
    tableAtt <- tableAtt[,-1]
    tableAtt <- tableAtt %>%
      select(analysis_step, everything())
    databaseName <- unique(attritionData$cdm_name)
    headerNames <- gsub("\\..*","", names(tableAtt))
    subtitles <- c(" ", databaseName)
    subtitlesHeader <- c()
    for (i in subtitles) {
      subtitlesHeader <- c(subtitlesHeader, i, " ")
    }
    huxTableAtt <- as_hux(tableAtt)
    lengthNames <- length(names(huxTableAtt))
    huxTableAtt <- huxTableAtt %>%
      set_contents(1, 1:lengthNames, headerNames)
    huxTableAtt <- huxTableAtt %>%
      insert_row(subtitlesHeader, after = 0)
    huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")
    # huxTableAtt
    return(huxTableAtt)
  } else {
    databaseName <- unique(attritionData$cdm_name)
    # databaseName <- databaseName[1:3]
    tableAtt <- attritionData %>%
      filter(cdm_name == databaseName[1]) %>%
      group_by(reason_id,
               reason) %>%
      summarise(current_n = round(mean(number_subjects ), 0),
                excluded = round(mean(excluded_subjects ), 0)) %>%
      mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                       between(reason_id, 10, 16) ~ attritionDataType)) %>%
      filter(reason != "Do not satisfy full contribution requirement for an interval")
    tableAtt <- tableAtt[,-1]
    tableAtt <- tableAtt %>%
      select(analysis_step, everything())
    # for (i in databaseName[2:3]) {
    for (i in databaseName[2:length(databaseName)]) {
      # i <- "SIDIAP"
      subAtt <- attritionData %>%
        filter(cdm_name == i) %>%
        group_by(reason_id,
                 reason) %>%
        summarise(current_n = round(mean(number_subjects ), 0),
                  excluded = round(mean(excluded_subjects ), 0)) %>%
        mutate(analysis_step = case_when(between(reason_id, 1, 10) ~ "initial",
                                         between(reason_id, 10, 16) ~ attritionDataType)) %>%
        filter(reason != "Do not satisfy full contribution requirement for an interval")
      subAtt <- subAtt[,-1]
      subAtt <- subAtt %>%
        select(analysis_step, everything())
      subAtt <- subAtt[, -c(1:2)]
      tableAtt <- bind_cols(tableAtt, subAtt)
    }
    names(tableAtt)
    headerNames <- gsub("\\..*","", names(tableAtt))

    # headerNames

    subtitles <- c(" ", databaseName)
    subtitlesHeader <- c()
    for (i in subtitles) {
      subtitlesHeader <- c(subtitlesHeader, i, " ")
    }
    huxTableAtt <- as_hux(tableAtt)
    lengthNames <- length(names(huxTableAtt))
    huxTableAtt <- huxTableAtt %>%
      set_contents(1, 1:lengthNames, headerNames)
    huxTableAtt <- huxTableAtt %>%
      insert_row(subtitlesHeader, after = 0)
    huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")
    return(huxTableAtt)
  }
}
#' `table1IncPrev()` returns Table 1 using incidence/prevalence estimates.
#'
#' @param incidence_estimates A data frame with incidence attrition data.
#'
#' @import flextable dplyr gt
#' @importFrom huxtable as_hux set_contents insert_row set_align everywhere merge_cells
#' @export
table1Inc <- function(incidence_estimates) {

    databaseNameInc <- unique(incidence_estimates$cdm_name)

    totalParSex <- incidence_estimates %>%
      filter(cdm_name == databaseNameInc) %>%
      filter(denominator_sex != "Both") %>%
      select(cdm_name, outcome_cohort_name, denominator_sex, n_persons) %>%
      group_by(cdm_name,
               outcome_cohort_name,
               denominator_sex) %>%
      summarise(`Number of Persons` = sum(n_persons))

    totalParSex <- pivot_wider(totalParSex, names_from = denominator_sex, values_from = `Number of Persons`)

    totalParAge <- incidence_estimates %>%
      filter(cdm_name == databaseNameInc) %>%
      filter(denominator_sex == "Both") %>%
      select(cdm_name, outcome_cohort_name, denominator_age_group, n_persons) %>%
      group_by(cdm_name,
               outcome_cohort_name,
               denominator_age_group) %>%
      summarise(`Number of Persons` = sum(n_persons))

    totalParAge <- pivot_wider(totalParAge, names_from = denominator_age_group , values_from = `Number of Persons`)
    totalSexAge <- left_join(totalParSex, totalParAge, by = c("cdm_name", "outcome_cohort_name"))

    table1Inc <- gt::gt(totalSexAge,
                     rowname_col = "Outcome",
                     groupname_col = "cdm_name") %>%
      tab_spanner(
        label = "Sex",
        columns = c(Female, Male)
      ) %>%
      tab_spanner(
        label = "Age Group",
        columns = c(unique(incidence_estimates$denominator_age_group))
      ) %>%
      opt_interactive(active = FALSE,
                      use_pagination = FALSE,
                      use_pagination_info = FALSE,
                      use_sorting = FALSE,
                      use_compact_mode = FALSE)
    table1Inc
  }

#' table2IncOver
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import flextable dplyr
#' @export
table2IncOver <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             cdm_name,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  tableIncidence <- as_hux(tableIncidence,
                           add_colnames = getOption("huxtable.add_colnames", FALSE))
  return(tableIncidence)
}

#' table3IncYear
#' @param incidence_estimates estimates of of the incidence
#'
#' @import flextable dplyr
#' @export
table3IncYear <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             cdm_name,
             incidence_start_date,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  tableIncidence <- as_hux(tableIncidence,
                           add_colnames = getOption("huxtable.add_colnames", FALSE))

  return(tableIncidence)
}

#' table4IncAge
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import flextable dplyr
#' @export
table4IncAge <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             cdm_name,
             denominator_age_group,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  tableIncidence <- as_hux(tableIncidence,
                           add_colnames = getOption("huxtable.add_colnames", FALSE))

  return(tableIncidence)
}

#' table5IncSex
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import flextable dplyr
#' @export
table5IncSex <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             cdm_name,
             denominator_sex,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  tableIncidence <- as_hux(tableIncidence,
                           add_colnames = getOption("huxtable.add_colnames", FALSE))

  return(tableIncidence)
}

cohortSurvivalTable <- function(data) {
  as_hux(data,
         add_colnames = getOption("huxtable.add_colnames", FALSE))
}
