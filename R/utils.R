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
    if (any(menuData[["type"]] %in% itemsList)) {
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
    # i <- 1
    for (k in seq_along(resultList[[i]])) {
      # k <- 1
    workingResult <- resultList[[i]][[k]]
    classWorkingResult <- class(workingResult)
    workingName <- names(resultList[[i]])[[k]]
    if (workingName == "summarise_large_scale_characteristics") {
      fileName <- paste0(
        unique(workingResult$cdm_name), "_",
        "large_scale_characteristics", "_",
        format(Sys.Date(), format = "%Y_%m_%d"),
        ".csv")
    } else {
      fileName <- paste0(
        unique(workingResult$cdm_name), "_",
        workingName, "_",
        format(Sys.Date(), format = "%Y_%m_%d"),
        ".csv")
    }

    if (is.null(workingName)) {
      workingName <- paste0("result_", i)
    }

    if ("summarised_result" %in% classWorkingResult) {
      omopgenerics::exportSummarisedResult(workingResult, fileName = fileName, path = tempDir)
    } else if (workingName == "treatmentPathways" | workingName == "metadata" | workingName == "summaryStatsTherapyDuration" | workingName == "incidence_attrition" | workingName == "prevalence_point_attrition" | workingName == "prevalence_period_attrition") {
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
  }
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
  tagList(column(4, actionButton(id, "Add item to report")))
}

createDownloadTableUI <- function(ns) {
  tagList(column(2, tagList(shiny::HTML("<label class = 'control-label'>&#8205;</label>"),
                            shiny::br(), downloadButton(ns("downloadSurvivalTable"), "Download Plot"))))
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

analysisNamesSum <- function(settingsData) {
  # settingsData <- settings(uploadedFilesList)
  analysisNamesSum <- settingsData %>%
    pull(result_type) %>%
    unique()

  if ("survival" %in% analysisNamesSum) {
    survivalAnalysisType <- settingsData %>%
      dplyr::filter(result_type == "survival") %>%
      pull(analysis_type) %>%
      unique()
    analysisNamesSum <- analysisNamesSum[-which(analysisNamesSum == "survival")]
    analysisNamesSum <- c(analysisNamesSum, survivalAnalysisType)
  }
  return(analysisNamesSum)
}

getSummarisedData <- function(uploadedData, type_result = "summarise_characteristics") {

  result_ids <- settings(uploadedData) %>%
    dplyr::filter(result_type == type_result) %>%
    pull(result_id)

  summarised_result <- uploadedData %>%
    dplyr::filter(result_id %in% result_ids)

  attr(summarised_result, "settings") <- settings(summarised_result) %>%
    dplyr::filter(result_id %in% result_ids)

  return(summarised_result)
}
