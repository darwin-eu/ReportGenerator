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
#' @export
table1NumPar <- function (incidence_attrition,
                          prevalence_attrition) {

 databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  tablePrevalenceAtt <- prevalence_attrition %>%
    filter(database_name == databaseNamePrev[1]) %>%
    group_by(step,
             reason,
             current_n,
             excluded) %>%
    summarise()

  databaseNameInc <- unique(incidence_attrition$database_name)

  databaseNameInc <- databaseNameInc[1:3]

  tableIncidenceAtt <- incidence_attrition %>%
    filter(database_name == databaseNameInc[1],
           step == "Estimating incidence",
           analysis_interval == "years") %>%
    group_by(step,
             reason) %>%
    summarise(current_n = round(mean(current_n), 0),
              excluded = round(mean(excluded), 0))

  tablePrevIncData <- bind_rows(tablePrevalenceAtt, tableIncidenceAtt)

  for (i in databaseNamePrev[2:3]) {

    subPrevalenceAtt <- prevalence_attrition %>%
      filter(database_name == i) %>%
      group_by(step,
               reason,
               current_n,
               excluded) %>%
      summarise()

    subIncidenceAtt <- incidence_attrition %>%
      filter(database_name == i,
             step == "Estimating incidence",
             analysis_interval == "years") %>%
      group_by(step,
               reason) %>%
      summarise(current_n = round(mean(current_n), 0),
                excluded = round(mean(excluded), 0))

    subPrevIncData <- bind_rows(subPrevalenceAtt, subIncidenceAtt)

    subPrevalenceAtt <- subPrevIncData[, -c(1:2)]

    tablePrevIncData <- bind_cols(tablePrevIncData,
                                  subPrevalenceAtt)

  }

  headerNames <- gsub("\\..*","", names(tablePrevIncData))

  subtitles <- c(" ", databaseNamePrev)

  flexTableAtt <- flextable(tablePrevIncData, theme_fun = theme_apa)

  # Deletes original header

  flexTableAtt <- delete_part(x = flexTableAtt, part = "header")

  # Adds header with repeaded col_keys

  flexTableAtt <- add_header_row(flexTableAtt,
                                 values = headerNames,
                                 colwidths = rep(1, 8),
                                 top = FALSE)

  flexTableAtt <- add_header_row(flexTableAtt,
                                 values = subtitles,
                                 colwidths = rep(2, length(subtitles)),
                                 top = TRUE) %>%
    theme_box()

  return(flexTableAtt)
}

#' table2IncOver
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table2IncOver <- function (incidence_estimates) {

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  tableIncidence <- incidence_estimates %>%
    filter(n_persons %in% c(8215316,
                            2283830,
                            14854799,
                            7310575,
                            339946,
                            925321)) %>%
    group_by(outcome_cohort_name,
             database_name,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  return(flextable(tableIncidence))

}

#' table3IncYear
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table3IncYear <- function (incidence_estimates) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # names(incidence_estimates)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(n_persons %in% c(8215316,
                            2283830,
                            14854799,
                            7310575,
                            339946,
                            925321)) %>%
    group_by(outcome_cohort_name,
             database_name,
             incidence_start_date,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  return(flextable(tableIncidence))
}

#' table4IncAge
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table4IncAge <- function (incidence_estimates) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # names(incidence_estimates)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(n_persons %in% c(8215316,
                            2283830,
                            14854799,
                            7310575,
                            339946,
                            925321)) %>%
    group_by(outcome_cohort_name,
             database_name,
             denominator_age_group,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  # View(tableIncidence)

  return(flextable(tableIncidence))
}

#' table5IncSex
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table5IncSex <- function (incidence_estimates) {

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(n_persons %in% c(8215316,
                            2283830,
                            14854799,
                            7310575,
                            339946,
                            925321)) %>%
    group_by(outcome_cohort_name,
             database_name,
             denominator_sex,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  unique(tableIncidence$n_persons)

  return(flextable(tableIncidence))
}
if(getRversion() >= "2.15.1")    utils::globalVariables(c("step",
                                                          "reason",
                                                          "denominator",
                                                          "current_n",
                                                          "excluded",
                                                          "outcome_cohort_name",
                                                          "n_persons",
                                                          "numerator",
                                                          "person_years",
                                                          "sex_strata",
                                                          "prev",
                                                          "prev_high",
                                                          "prev_low",
                                                          "subject_id",
                                                          "n_events",
                                                          "incidence_estimates",
                                                          "prevalence_estimates",
                                                          "cdm_snapshot"))
