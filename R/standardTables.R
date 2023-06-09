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

#' table1NumPar
#'
#' @param incidence_attrition incidence of the attrition
#' @param prevalence_attrition prevalence of the attrition
#'
#' @import here flextable dplyr
#' @importFrom huxtable as_hux set_contents insert_row set_align everywhere
#' @export
table1NumPar <- function(prevalence_attrition, incidence_attrition) {

  prevalence_attrition <- dataCleanAttrition(prevalence_attrition = prevalence_attrition)
  incidence_attrition <- dataCleanAttrition(incidence_attrition = incidence_attrition)

  if (length(unique(prevalence_attrition$database_name)) == 1) {

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

    databaseName <- unique(incidence_attrition$database_name)

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

    databaseNamePrev <- unique(prevalence_attrition$database_name)

    # databaseNamePrev <- databaseNamePrev[1:3]

    tablePrevalenceAtt <- prevalence_attrition %>%
      filter(database_name == databaseNamePrev[1]) %>%
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

    databaseNameInc <- unique(incidence_attrition$database_name)

    # databaseNameInc <- databaseNameInc[1:3]

    tableIncidenceAtt <- incidence_attrition %>%
      filter(database_name == databaseNameInc[1]) %>%
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
        filter(database_name == i) %>%
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
        filter(database_name == i) %>%
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
#' table1SexAge
#'
#' @param incidence_estimates incidence of the attrition
#'
#' @import here flextable dplyr
#' @importFrom huxtable as_hux set_contents insert_row set_align everywhere merge_cells
#' @export
table1SexAge <- function(incidence_estimates) {
    # # Table data prevalence
    #
    # incidence_estimates <- incidence_estimates_ab
    # prevalence_estimates <- prevalence_estimates_ab

    # unique(incidence_estimates$denominator_age_group)
    # unique(incidence_estimates$denominator_sex)

    databaseNameInc <- unique(incidence_estimates$database_name)

    totalParSex <- incidence_estimates %>%
      filter(database_name == databaseNameInc) %>%
      filter(denominator_sex != "Both") %>%
      select(database_name, outcome_cohort_name, denominator_sex, n_persons) %>%
      group_by(database_name,
               outcome_cohort_name,
               denominator_sex) %>%
      summarise(`Total Users` = sum(n_persons))

    totalParSex <- pivot_wider(totalParSex, names_from = denominator_sex, values_from = `Total Users`)

    totalParAge <- incidence_estimates %>%
      filter(database_name == databaseNameInc) %>%
      filter(denominator_sex != "Both") %>%
      select(database_name, outcome_cohort_name, denominator_age_group, n_persons) %>%
      group_by(database_name,
               outcome_cohort_name,
               denominator_age_group) %>%
      summarise(`Total Users` = sum(n_persons))

    totalParAge <- pivot_wider(totalParAge, names_from = denominator_age_group , values_from = `Total Users`)

    totalSexAge <- left_join(totalParSex, totalParAge, by = c("database_name", "outcome_cohort_name"))

    huxSexAge <- as_hux(totalSexAge)

    headerNames <- names(huxSexAge)

    # headerNames <- gsub("database_name", "Database Name", headerNames)
    headerNames <- gsub("outcome_cohort_name", "Outcome", headerNames)
    headerNames <- gsub(";", "-", headerNames)

    lengthNames <- length(names(huxSexAge))

    blankAgeGroup <- rep(" ", (lengthNames-3))

    blankAgeGroup <- append(blankAgeGroup, list(x="Age Groups"), (length(blankAgeGroup)/2))

    subtitlesHeader <- c("Database / Total Users, N", "Sex", unlist(blankAgeGroup))

    # huxSexAge <- as_hux(totalSexAge)

    huxSexAge <- huxSexAge %>%
      set_contents(1, 1:lengthNames, headerNames)
    #
    # huxSexAge <- huxSexAge %>%
    #   insert_row(subtitlesHeader, after = 0)
    #
    #
    # blankCells <- rep(" ", (lengthNames-1))
    #
    # databaseSubsection <- c(unique(totalSexAge$database_name[1]), blankCells)
    #
    # huxSexAge <- huxSexAge %>%
    #   insert_row(databaseSubsection, after = 2) %>%
    #   merge_cells(3, 1:length(databaseSubsection))

    # huxSexAge <- subset(huxSexAge, select = -`Database Name`)

    huxSexAge <- huxSexAge %>% set_align(1, everywhere, "center")

    # flexSexAge <- as_flextable(huxSexAge) %>%
    #   delete_part(part = c("header")) %>%
    #   delete_part(part = c("footer")) %>%
    #   merge_h()

    # class(huxSexAge)

    return(huxSexAge)

  }

#' table2IncOver
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table2IncOver <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             database_name,
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
#' @import here flextable dplyr
#' @export
table3IncYear <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             database_name,
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
#' @import here flextable dplyr
#' @export
table4IncAge <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             database_name,
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
#' @import here flextable dplyr
#' @export
table5IncSex <- function (incidence_estimates) {

  tableIncidence <- incidence_estimates %>%
    group_by(outcome_cohort_name,
             database_name,
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
