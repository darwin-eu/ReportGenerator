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

# Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>
incidenceTable1 <- function(incidenceData = NULL,
                            denominatorData = NULL) {

  incidenceTableData <- incidenceData %>%
    group_by(database_name) %>%
    summarise(total = sum(n_events))

  denominatorTable1 <- denominatorData %>%
    group_by(database_name) %>%
    summarise(subjects = length(unique(subject_id)))

  incidenceTableData <- left_join(incidenceTableData,
                               denominatorTable1,
                               by = "database_name")

  return(incidenceTableData)

}
# Table 2. Incidence data according to figures 1 and 2
table2Incidence <- function(incidenceData) {

  table2Data <- incidenceData %>%
    select(database_name,
           incidence_start_date,
           denominator_sex,
           denominator_age_group,
           n_events,
           person_years,
           incidence_100000_pys) %>%
    mutate(person_years = round(person_years, 0),
           incidence_100000_pys = round(incidence_100000_pys, 0))

  table2Data <- table2Data[with(table2Data,
                                order(database_name,
                                      incidence_start_date,
                                      denominator_sex,
                                      denominator_age_group)),]

  colnames(table2Data) <- c("Database",
                            "Time",
                            "Sex",
                            "Age group",
                            "N events",
                            "Person-years",
                            "IR 10000 pys")
  return(table2Data)
}
utils::globalVariables(c("incidenceData",
                         "n_cases",
                         "n_population",
                         "incidence_100000_pys_95CI_lower",
                         "incidence_100000_pys_95CI_upper",
                         "incidence_100000_pys",
                         "prevalence_start_date",
                         "prevalence"))
