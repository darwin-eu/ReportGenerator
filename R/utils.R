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
#' @param name vector of uploaded filenames.
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

#' Get options for given item.
#'
#' @param item the menu item
#'
#' @return the options
getItemOptions <- function(title) {
  checkmate::assertSetEqual(length(title), 1)
  menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  functionText <- lapply(menuData, function(menuData, title) {
    if (all(menuData[["title"]] %in% title)) {
      menuData[["options"]]
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

  # previewItemString <- "plotIncidence(incidenceCommonData(), colour, facet)"
  # previewItemType <- "Facet by database"
  #
  #
  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'cdm_name'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)

  } else if (previewItemType == "Facet by database"){

    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'outcome_cohort_name'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)

  }
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeSex <- function(previewItemString, previewItemType) {

  # previewItemString <- "plotIncidence(incidenceCommonData(), colour, facet)"
  # previewItemType <- "Facet by database"
  #
  #
  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }

  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_sex'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)

  } else if (previewItemType == "Facet by database"){

    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_sex'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)

  }
  return(result)
}

#' Adds the given type to the current previewItem string, by sex
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemTypeAge <- function(previewItemString, previewItemType) {

  # previewItemString <- "plotIncidence(incidence_estimates, colour, facet)"
  # previewItemType <- "Facet by database"
  #
  #
  result <- previewItemString
  if (is.null(previewItemType)) {
    previewItemType <- "Facet by outcome"
  }
  if (previewItemType == "Facet by outcome") {
    facet <- "facet = 'outcome_cohort_name'"
    colour <- "colour = 'denominator_age_group'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)
  } else if (previewItemType == "Facet by database"){
    facet <- "facet = 'cdm_name'"
    colour <- "colour = 'denominator_age_group'"
    result <- gsub("colour", colour, previewItemString)
    result <- gsub("facet", facet, result)

  }
  return(result)
}

#' Increase the facet strip size for better readability.
#'
#' @param plotlyObject object generated by ggplotly()
#'
#' @return the updated object
increaseFacetStripSize <- function(plotlyObject) {
  n_facets <- c(1:length(plotlyObject[["x"]][["layout"]][["shapes"]]))
  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      plotlyObject[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- + 80 # increase as needed
      plotlyObject[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }
  return(plotlyObject)
}


facetReturn <- function(menuFun, objectChoice) {
  plotOptions <- menuFun %>%
    dplyr::filter(title == objectChoice) %>%
    getItemOptions()
  if (!identical(plotOptions, character(0))) {
    names(plotOptions) <- as.character(glue::glue("{toupper(letters)[1:length(plotOptions)]}: {plotOptions}"))
    return(selectizeInput("previewPlotOption", "Select plot type", choices = plotOptions))
  }
}
