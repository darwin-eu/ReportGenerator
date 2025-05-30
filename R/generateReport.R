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

#' Generate report
#'
#' @param reportDocx the report document
#' @param dataReportList the list of items to be added
#' @param fileName the name of the report file
#' @param logger logger object
#' @param reportApp If TRUE, it will pull the menuConfig yaml file from the reportApp folder.
#'
#' @return NULL, the report is written to given file
generateReport <- function(reportDocx, dataReportList, fileName, logger, reportApp = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(reportDocx, "rdocx", add = errorMessage)
  checkmate::assertList(dataReportList, add = errorMessage)
  checkmate::assertPathForOutput(fileName, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  cli::cli_h2("Generating report for {} data")

  if (length(dataReportList) > 0) {
    # Loop through ever object selected in the menu
    log4r::info(logger, glue::glue("Start generating report, number of items: {length(dataReportList)}"))
    for (i in seq(1:length(dataReportList))) {
      # i <- 1
      # Get the function to generate and print in report
      titleText <- names(dataReportList[[i]])

      if (titleText == "Sankey Diagram - TreatmentPatterns") {
        body_add_img(x = reportDocx,
                     src = dataReportList[[i]][[1]][["fileImage"]],
                     height = 3,
                     width = 7)
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
      } else {

        expression <- getItemConfig(input = "object",
                                    output = "function",
                                    inputValue = titleText,
                                    reportApp = reportApp)

        dataReportListInternal <- dataReportList[[i]]

        # Save function as an variable
        arguments <- dataReportList[[i]][[titleText]][setdiff(names(dataReportList[[i]][[titleText]]), "caption")]
        object <- do.call(expression, args = arguments)

        # Generic dispatch to add either a table or a figure into the reportDocx object
        reportDocx <- print_items(object = object,
                                  reportDocx = reportDocx,
                                  titleText = titleText,
                                  dataReportListInternal = dataReportListInternal,
                                  logger = logger)

      }


    }
  } else {
    log4r::warn(logger, "No items added to the report")
  }
  body_add_toc(reportDocx)
  log4r::info(logger, "Printing report")
  print(reportDocx,
        target = fileName)
}

getGGPlotDimensions <- function() {
  return(list("width" = 10.5, "height" = 4.75))
}

# Generic function
print_items <- function(object, ...) {
  if (inherits(object, "ggplot")) {
    return(print_items.ggplot(object, ...))
  } else if (inherits(object, "gt_tbl")) {
    return(print_items.gt_tbl(object, ...))
  }
}

# Method for class "data.frame"
print_items.gt_tbl <- function(object,
                               reportDocx,
                               titleText,
                               dataReportListInternal,
                               logger, ...) {

  log4r::info(logger, glue::glue("Generating GT table object"))
  body_end_section_landscape(reportDocx)
  body_add_gt(reportDocx, value = object)
  body_add(reportDocx,
           value = dataReportListInternal[[1]][["caption"]])
  body_add(reportDocx,
           value = titleText,
           style = "heading 1")
  body_end_section_portrait(reportDocx)
  return(reportDocx)
}

# Method for class "numeric"
print_items.ggplot <- function(object,
                               reportDocx,
                               titleText,
                               dataReportListInternal,
                               logger,
                               ...) {

  log4r::info(logger, glue::glue("Generating ggplot object"))
  body_end_section_landscape(reportDocx)
  plotDim <- getGGPlotDimensions()
  body_add_gg(x = reportDocx,
              value = object,
              width = plotDim[["width"]],
              height = plotDim[["height"]],
              style = "Normal")
  body_add(reportDocx,
           value = dataReportListInternal[[1]][["caption"]])
  body_add(reportDocx,
           value = titleText,
           style = "heading 1")
  body_end_section_portrait(reportDocx)

  return(reportDocx)
}
