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

# Table 1 Paragraph. Incidence rate/s of drug/s use over calendar incidence_start_date (per month/year) overall
table1IncidenceParagraph <- function(indcidenceData) {

  incidenceTable1 <- incidenceTable1(incidenceData)

  totalPopulationIncidence <- sum(incidenceTable1$total)

  minYearIncidence <- format(min(incidenceData$incidence_start_date),
                             format = "%Y")

  maxYearIncidence <- format(max(incidenceData$incidence_end_date),
                             format = "%Y")

  incidenceTitleIntroTable1 <- "Number of new cases during study period"

  incidenceIntroTable1 <- paste("Table 1 describes the total number of new events with at least one day of observation time during the study period. For this study, we investigated use of <outcome 1> in more than ",
                                totalPopulationIncidence,
                                " patients during the study period ",
                                minYearIncidence,
                                " to ",
                                maxYearIncidence,
                                " .",
                                sep = "")

  return(incidenceIntroTable1)

}
