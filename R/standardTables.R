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
#' @importFrom huxtable as_hux set_contents insert_row set_align
#' @export
table1NumPar <- function (incidence_attrition,
                          prevalence_attrition) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # prevalence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
  #                                     "Prior history requirement not fulfilled during study period",
  #                                     prevalence_attrition$reason)
  #
  # incidence_attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
  #                                     "Prior history requirement not fulfilled during study period",
  #                                     incidence_attrition$reason)
  #
  # unique(prevalence_attrition$reason)
  # names(prevalence_attrition)
  #
  #
  # prevalence_attrition <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_CPRD GOLD.csv")
  # prevalence_attrition <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_IPCI.csv")
  # prevalence_attritionr <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_SIDIAP.csv")

  # names(prevalence_attrition)
  #
  #
  #
  # names(prevalence_attrition)
  # Add reason_id if not present; allows a correct order of the rows

    # intersect(names(prevalence_attrition_BloodCancer), names(prevalence_attrition_Mock))
    #
    #
    # setdiff(names(prevalence_attrition_BloodCancer), names(prevalence_attrition_Mock))
    #
    #
    # setdiff(names(prevalence_attrition_Mock), names(prevalence_attrition_BloodCancer))


    # Prevalence data

    # prevalence_attrition <- read_csv("results/resultsMock_CPRD/test database_prevalence_attrition_20230427.csv")

    if (!("reason_id" %in% names(prevalence_attrition))) {

      prevalence_attrition <- prevalence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16),
               number_subjects = current_n,
               excluded_subjects = excluded)

    } else {

      prevalence_attrition <- prevalence_attrition %>%
        mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                     reason == "Missing year of birth"  ~ 2,
                                     reason == "Missing sex"  ~ 3,
                                     reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                     reason == "No observation time available during study period"  ~ 5,
                                     reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                     reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                     reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                     reason == "Not Female"  ~ 9,
                                     reason == "Not Male"  ~ 10,
                                     reason == "Starting analysis population" ~ 11,
                                     reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                     reason == "Not observed during the complete database interval"  ~ 14,
                                     reason == "Do not satisfy full contribution requirement for an interval"  ~ 16))

    }

    # Table data

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

  tablePrevalenceAtt

  # Incidence data


  # incidence_attrition <- read_csv("results/resultsMock_CPRD/test database_incidence_attrition_20230427.csv")

  if (!("reason_id" %in% names(incidence_attrition))) {

    incidence_attrition <- incidence_attrition %>%
      mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                   reason == "Missing year of birth"  ~ 2,
                                   reason == "Missing sex"  ~ 3,
                                   reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                   reason == "No observation time available during study period"  ~ 5,
                                   reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                   reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                   reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                   reason == "Not Female"  ~ 9,
                                   reason == "Not Male"  ~ 10,
                                   reason == "Starting analysis population" ~ 11,
                                   reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                   reason == "Not observed during the complete database interval"  ~ 14,
                                   reason == "Do not satisfy full contribution requirement for an interval"  ~ 16),
             number_subjects = current_n,
             excluded_subjects = excluded)

  } else {

    incidence_attrition <- incidence_attrition %>%
      mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                   reason == "Missing year of birth"  ~ 2,
                                   reason == "Missing sex"  ~ 3,
                                   reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                   reason == "No observation time available during study period"  ~ 5,
                                   reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                   reason == "Prior history requirement not fulfilled during study period"  ~ 7,
                                   reason == "No observation time available after applying age and prior history criteria"  ~ 8,
                                   reason == "Not Female"  ~ 9,
                                   reason == "Not Male"  ~ 10,
                                   reason == "Starting analysis population" ~ 11,
                                   reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12,
                                   reason == "Not observed during the complete database interval"  ~ 14,
                                   reason == "Do not satisfy full contribution requirement for an interval"  ~ 16))

  }

  # Table data

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

  tableIncidenceAtt



  tablePrevIncData <- union(tablePrevalenceAtt, tableIncidenceAtt)

  databaseName <- unique(incidence_attrition$database_name)

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

  # for (i in databaseNamePrev[2:3]) {
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

  # for (i in seq(1, lengthNames, 2)) {
  #
  #   val <- (i+1)
  #
  #   huxTableAtt <- huxTableAtt %>% merge_cells(1, i:val)
  #
  # }

  huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")

  return(huxTableAtt)

}

#' table2IncOver
#'
#' @param incidence_estimates estimates of of the incidence
#'
#' @import here flextable dplyr
#' @export
table2IncOver <- function (incidence_estimates) {

  # incidence_estimates <- test_database_incidence_estimates_20230328 <- read_csv("results_zip/mock_results_IPCI/test database_incidence_estimates_20230328.csv")

  # incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  tableIncidence <- incidence_estimates %>%
    # filter(n_persons %in% c(8215316,
    #                         2283830,
    #                         14854799,
    #                         7310575,
    #                         339946,
    #                         925321)) %>%
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

  # incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    # filter(n_persons %in% c(8215316,
    #                         2283830,
    #                         14854799,
    #                         7310575,
    #                         339946,
    #                         925321)) %>%
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

  # incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    # filter(n_persons %in% c(8215316,
    #                         2283830,
    #                         14854799,
    #                         7310575,
    #                         339946,
    #                         925321)) %>%
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

  # incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  # databaseNamePrev <- unique(prevalence_attrition$database_name)
  #
  # databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    # filter(n_persons %in% c(8215316,
    #                         2283830,
    #                         14854799,
    #                         7310575,
    #                         339946,
    #                         925321)) %>%
    group_by(outcome_cohort_name,
             database_name,
             denominator_sex,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  # unique(tableIncidence$n_persons)

  return(flextable(tableIncidence))
}
