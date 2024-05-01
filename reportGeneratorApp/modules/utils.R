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

#' Checks items available for ReportGenerator
#'
#' Get the items that the user can choose from in the report generator. The list is loaded from the configuration file
#' and filtered by the files that have been uploaded.
#'
#' @param items vector of uploaded filenames.
#'
#' @return a dataframe with the properties of the items
getItemsList <- function(items) {
  menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  menuList <- lapply(menuData, function(menuData, itemsList) {
    if (all(menuData[["type"]] %in% itemsList)) {
      menuData[["title"]]
    }
  }, itemsList = items)
  result <- list()
  for (i in menuList) {
    if (!is.null(i)) {
      result <- rbind(result, i)
    }
  }
  result <- unlist(result)
  return(result)
}

#' Checks items available for ReportGenerator
#'
#' Get the items that the user can choose from in the report generator. The list is loaded from the configuration file
#' and filtered by the files that have been uploaded.
#'
#' @param input Declare the type of value such as "title" to look up in the yml file.
#' @param output Expected value from the yml file such as the "function" to evaluate.
#' @param inputValue The actual value in character to look up.
#'
#' @return a dataframe with the properties of the items
getItemConfig <- function(input = NULL,
                          output = NULL,
                          inputValue = NULL) {

  checkmate::assertSetEqual(length(input), 1)
  checkmate::assertSetEqual(length(output), 1)
  checkmate::assertSetEqual(length(inputValue), 1)
  checkmate::assertCharacter(input)
  checkmate::assertCharacter(output)
  checkmate::assertCharacter(inputValue)

  menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  functionText <- lapply(menuData, function(menuData, title) {
    if (menuData[[input]] == inputValue) {
      menuData[[output]]
    }
  }, title = title)
  result <- list()
  for (i in functionText) {
    if (!is.null(i)) {
      result <- rbind(result, i)
    }
  }
  result <- unlist(result)
  return(result)
}

#' Adds the given type to the current previewItem string.
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemType <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'cdm_name'"
  } else if (previewItemType == "Facet by database, colour by strata_name"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'strata_name'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'outcome_cohort_name'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeSex <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }

  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_sex'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_sex'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeAge <- function(previewItemString, previewItemType) {

  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_age_group'"
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_age_group'"
  }
  result <- gsub("colour", colour, previewItemString)
  result <- gsub("facet", facet, result)
  return(result)
}

#' Adds the given ribbon to the current previewItem string.
#'
#' @param previewItemString string representing the previewItem
#' @param ribbon ribbon value
#'
#' @return the updated preview item string
addPreviewItemRibbon <- function(previewItemString, ribbon) {
  ribbonStr <- paste0("ribbon = ", ribbon)
  return(gsub("ribbon", ribbonStr, previewItemString))
}

#' Adds plot options to the current previewItem string.
#'
#' @param previewItemString string representing the previewItem
#' @param showCI if confidence interval should be shown
#' @param stackPlots if subplots should be stacked (on top of each other) or not
#'
#' @return the updated preview item string
addPlotOptions <- function(previewItemString, showCI, stackPlots) {
  optionsStr <- "options = list("
  showCI <- as.logical(showCI)
  stackPlots <- as.logical(stackPlots)
  if (!showCI) {
    optionsStr <- paste0(optionsStr, "hideConfidenceInterval = TRUE")
  }
  if (stackPlots) {
    optionsStr <- paste0(optionsStr, ifelse(!showCI, ",", ""), "facetNcols = 1")
  }
  optionsStr <- paste0(optionsStr, ")")
  return(gsub("options", optionsStr, previewItemString))
}

#' Export list of package results
#'
#' @param resultList Named list with results from a darwin package
#' @param zipName name to give zip folder
#' @param outputFolder directory to save zip folder containing results as a set
#' of CSV files
#'
#' @return zip folder of results saved in the outputFolder
#' @export
#'
#' @examples
#' \donttest{
#' exportResults(
#'   resultList = list("mtcars" = mtcars),
#'   zipName = "test",
#'   outputFolder = tempdir()
#' )
#' }
exportResults <- function(resultList,
                          zipName,
                          outputFolder) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(resultList, add = errorMessage)
  checkmate::assertNamed(resultList, add = errorMessage)
  checkmate::assertCharacter(zipName, add = errorMessage)
  checkmate::assertDirectoryExists(outputFolder, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  tempDir <- zipName
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }

  # write results to disk
  for (i in seq_along(resultList)) {
    workingResult <- resultList[[i]]
    workingName <- names(resultList)[[i]]
    if (is.null(workingName)) {
      workingName <- paste0("result_", i)
    }
    utils::write.csv(workingResult,
                     file = file.path(
                       tempDir,
                       paste0(
                         unique(workingResult$cdm_name), "_",
                         workingName, "_",
                         format(Sys.Date(), format = "%Y_%m_%d"),
                         ".csv"
                       )
                     ),
                     row.names = FALSE
    )
  }

  zip::zip(
    zipfile = file.path(outputFolder, paste0(zipName, ".zip")),
    files = list.files(tempDir, full.names = TRUE)
  )
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }

  invisible(resultList)
}

getRandomId <- function() {
  chars <- c(0:9, letters, LETTERS)
  randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "")
  return(randomId)
}

createCaptionInput <- function(inputId, value, height = "50px") {
  textAreaInput(inputId = inputId,
                label = "Caption",
                value = value,
                width = '100%',
                height = height)
}

saveGGPlot <- function(file, plot, height = 10, width = 20, dpi = 300) {
  ggplot2::ggsave(file,
                  plot = plot,
                  device = "png",
                  height = height,
                  width = width,
                  dpi = dpi,
                  units = "cm")
}

createAddItemToReportUI <- function(id) {
  tagList(column(2, shiny::HTML("<label class = 'control-label'>&#8205;</label>"),
                    shiny::br(), actionButton(id, "Add item to report")))
}

createDownloadPlotUI <- function(ns) {
  tagList(column(2, div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns("plotHeight"), "", 10, width = "50px"))),
          column(2, div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns("plotWidth"), "", 20, width = "50px"))),
          column(2, div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns("plotDpi"), "", 300, width = "50px"))),
          column(2, tagList(shiny::HTML("<label class = 'control-label'>&#8205;</label>"),
                            shiny::br(), downloadButton(ns("downloadFigure"), "Download Plot"))))
}

createDataTable <- function(data, tableName = "result") {
  DT::datatable(data,
                extensions = 'Buttons',
                options = list(pageLength = 10,
                               paging = TRUE,
                               searching = TRUE,
                               fixedColumns = TRUE,
                               autoWidth = TRUE,
                               ordering = TRUE,
                               scrollX = TRUE,
                               dom = 'Bfrtip',
                               buttons =
                                 list(list(
                                   extend = "collection",
                                   buttons = list(
                                     list(extend = "csv", title = tableName),
                                     list(extend = "excel", title = tableName)),
                                   text = "Download"
                                 ))),
                class = "display")
}

selectCols <- function(data) {
  result <- data %>%
    select(c(result_id, cdm_name, result_type, package_name, package_version,
             group_name, group_level, strata_name, strata_level, variable_name,
             variable_level, estimate_name, estimate_type, estimate_value,
             additional_name, additional_level))
  return(result)
}

autoCaptionCharac <- function(summarisedCharacteristics) {
  groupLevels <- glue::glue_collapse(unique(summarisedCharacteristics$group_level), ", ", last = " and ")
  result <- glue::glue("Demographic characteristics of {groupLevels} patients.")
  return(result)
}

generateReport <- function(reportDocx, dataReportList, fileName, logger) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertClass(reportDocx, "rdocx", add = errorMessage)
  checkmate::assertList(dataReportList, add = errorMessage)
  checkmate::assertPathForOutput(fileName, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (length(dataReportList) > 0) {
    # Loop through ever object selected in the menu
    log4r::info(logger, glue::glue("Start generating report, number of items: {length(dataReportList)}"))
    for (i in seq(1:length(dataReportList))) {
      # i <- 1
      # Get the function to generate and print in report
      titleText <- names(dataReportList[[i]])
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = titleText)
      # Get relevant options for the function
      itemOptions <- getItemConfig(input = "title",
                                   output = "options",
                                   inputValue = titleText)

      # Additional parameter if there are options for the graphs
      if (!is.null(itemOptions)) {
        if (grepl("by sex", titleText)) {
          expression <- expression %>%
            addPreviewItemTypeSex(dataReportList[[i]][[1]][["plotOption"]])
        } else if (grepl("by age", titleText)) {
          expression <- expression %>%
            addPreviewItemTypeAge(dataReportList[[i]][[1]][["plotOption"]])
        } else  {
          expression <- expression %>%
            addPreviewItemType(dataReportList[[i]][[1]][["plotOption"]])
        }
        if (grepl("Ribbon", itemOptions)) {
          expression <- expression %>%
            addPreviewItemRibbon(dataReportList[[i]][[1]][["ribbon"]])
        }
        if (grepl("Options", itemOptions)) {
          expression <- do.call(addPlotOptions, append(list(expression),
                                                       as.list(dataReportList[[i]][[1]][["options"]])))
        }
      }

      # Evaluate function
      object <- eval(parse(text = expression), envir = dataReportList[[i]][[1]])

      # Check class of every function and add it to the word report accordingly
      if ("gt_tbl" %in% class(object)) {
        log4r::info(logger, glue::glue("Generating gt_tble object"))
        body_end_section_landscape(reportDocx)
        body_add_gt(reportDocx, value = object)
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)

      } else if ("ggplot" %in% class(object)) {
        body_end_section_landscape(reportDocx)
        plotDim <- getGGPlotDimensions()
        body_add_gg(x = reportDocx,
                    value = object,
                    width = plotDim[["width"]],
                    height = plotDim[["height"]],
                    style = "Normal")
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)

      } else if ("huxtable" %in% class(object)) {
        body_end_section_landscape(reportDocx)
        body_add_table(reportDocx,
                       value = object,
                       style = "Table Paragraph",
                       header = FALSE)
        body_add_par(reportDocx, " ")
        body_add(reportDocx,
                 value = dataReportList[[i]][[1]][["caption"]])
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
        body_end_section_portrait(reportDocx)
      }

      if (titleText == "Sunburst Plot - TreatmentPatterns") {
        body_add_img(x = reportDocx,
                     src = dataReportList[[i]][[1]][["fileImage"]],
                     height = 5.5,
                     width = 7)
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")

      }  else if (titleText == "Sankey Diagram - TreatmentPatterns") {
        body_add_img(x = reportDocx,
                     src = dataReportList[[i]][[1]][["fileImage"]],
                     height = 3,
                     width = 7)
        body_add(reportDocx,
                 value = titleText,
                 style = "heading 1")
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
  return(list("width" = 10.5,
              "height" = 4.75))
}

dataCleanAttrition <- function(attrition) {
  if (!is.null(attrition)) {
    attrition$reason <- gsub("Prior history requirement not fullfilled during study period",
                             "Prior history requirement not fulfilled during study period ",
                             attrition$reason)
    if (!("reason_id" %in% names(attrition))) {
      attrition <- attrition %>%
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
      attrition <- attrition %>%
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
    return(attrition)
  }
}
