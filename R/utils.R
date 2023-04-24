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
#' @param uploadedFiles vector of uploaded filenames.
#'
#' @return a dataframe with the properties of the items
getItemsList <- function(uploadedFiles) {
  menuFunctions <- read.csv(system.file("config/itemsConfig.csv", package = "ReportGenerator"),
                            sep = ";") %>%
    dplyr::mutate(signature = paste0(name, "(", arguments, ")"))

  checkNeeds <- function(menuFunctions, uploadedFiles) {
    unlist(lapply(menuFunctions, FUN = function(menuFunction) {
      required <- trimws(unlist(strsplit(menuFunction , ",")))
      exists <- required %in% uploadedFiles
      if (TRUE %in% exists) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    )
    )
  }
  menuFunctions  %>%
    dplyr::filter(checkNeeds(menuFunctions$arguments, uploadedFiles)) %>%
    dplyr::select(title, signature)
}

#' Adds the given type to the current previewItem string.
#'
#' @param previewItemString string representing the previewItem
#' @param previewItemType preview item type
#'
#' @return the updated preview item string
addPreviewItemType <- function(previewItemString, previewItemType) {
  result <- previewItemString
  if (class(previewItemString) == "character" && grepl(")", previewItemString) && !is.null(previewItemType)) {
    lastBracketStartPos <- stringi::stri_locate_last_fixed(previewItemString, ")")[1]
    result <- as.character(glue::glue('{substr(previewItemString, 0, lastBracketStartPos - 1)}, "{previewItemType}")'))
  }
  return(result)
}

#' Get options for given item.
#'
#' @param item the menu item
#'
#' @return the options
getItemOptions <- function(item) {
  item %>%
    dplyr::pull(options) %>%
    strsplit(split = ", ") %>%
    unlist()
}

