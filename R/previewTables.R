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

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  # databaseNamePrev <- databaseNamePrev[1:3]

  tablePrevalenceAtt <- prevalence_attrition %>%
    filter(database_name == databaseNamePrev[1]) %>%
    group_by(step,
             reason,
             current_n,
             excluded) %>%
    summarise()

  databaseNameInc <- unique(incidence_attrition$database_name)

  # databaseNameInc <- databaseNameInc[1:3]

  tableIncidenceAtt <- incidence_attrition %>%
    filter(database_name == databaseNameInc[1],
           step == "Estimating incidence",
           analysis_interval == "years") %>%
    group_by(step,
             reason) %>%
    summarise(current_n = round(mean(current_n), 0),
              excluded = round(mean(excluded), 0))

  tablePrevIncData <- bind_rows(tablePrevalenceAtt, tableIncidenceAtt)

  for (i in databaseNamePrev[2:length(databaseNamePrev)]) {

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

  for (i in seq(1, lengthNames, 2)) {

    val <- (i+1)

    huxTableAtt <- huxTableAtt %>% merge_cells(1, i:val)

    }

  huxTableAtt <- huxTableAtt %>% set_align(1, everywhere, "center")

  return(huxTableAtt)
}
