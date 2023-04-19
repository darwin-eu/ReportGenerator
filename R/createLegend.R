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

#' Write html file for the sunburst plot legend.
#'
#' @param fileName Output file name for the legend.
#' @param inputJSON JSON-data to create legend
#'
#' @returns NULL
createLegend <- function(inputJSON, fileName) {
  # Load template HTML file
  html <- paste(
    readLines(file.path(
      system.file(package = "ReportGenerator"),
      "TreatmentPatterns",
      "legend.html"
    )),
    collapse = "\n"
  )

  html <- sub("@insert_data", inputJSON, html)

  # Save HTML file as sunburst_@studyName
  writeLines(
    text = html,
    con = fileName)
}
