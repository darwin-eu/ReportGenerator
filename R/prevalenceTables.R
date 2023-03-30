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

# Table 3. Number of cases and total number of patients per database
prevalenceTable3 <- function(prevalenceData) {

  prevalenceTableData <- prevalenceData %>%
    group_by(database_name) %>%
    summarise(n_cases = sum(n_cases),
              n_population = sum(n_population))

  return(prevalenceTableData)
}
# Table 4. Numeric values of displayed figures 5-8
table4Prevalence <- function(prevalenceData) {

  table4Data <- prevalenceData %>%
    select(database_name,
           prevalence_start_date,
           denominator_sex,
           denominator_age_group,
           n_cases,
           n_population,
           prevalence) %>%
    mutate(prevalence = scales::percent(prevalence,
                                        accuracy = 1))

  table4Data <- table4Data[with(table4Data,
                                order(database_name,
                                      prevalence_start_date,
                                      denominator_sex,
                                      denominator_age_group)),]

  colnames(table4Data) <- c("Database",
                            "Time",
                            "Sex",
                            "Age group",
                            "Number of cases",
                            "Denominator population",
                            "Prevalence")
  return(table4Data)
}
