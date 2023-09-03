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

createSankeyDiagram <- function(
    treatmentPathways,
    groupCombinations = FALSE,
    minFreq = 5) {
  data <- treatmentPathways %>%
    rowwise() %>%
    dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    dplyr::mutate(freq = as.integer(.data$freq))

  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")

  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")

  data[is.na(data)] <- "Stopped"

  result1 <- data %>%
    mutate(
      source = paste("1.", .data$path1),
      target = paste("2.", .data$path2)
    ) %>%
    select("source", "target", "freq")


  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste("2.", .data$path2),
        target = paste("3.", .data$path3)
      ) %>%
      select("source", "target", "freq")

    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }

  links <- links %>%
    dplyr::filter(.data$freq >= minFreq) %>%
    dplyr::mutate(`%` = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")

  if (groupCombinations) {
    links$source <- stringr::str_replace_all(
      string = links$source, "\\w+\\+\\w+", replacement = "Combination"
    )
    links$target <- stringr::str_replace_all(
      string = links$target, "\\w+\\+\\w+", replacement = "Combination"
    )
  }

  # Draw sankey network
  plot <- googleVis::gvisSankey(
    links,
    from = "source",
    to = "target",
    weight = "%",
    chartid = 1,
    options = list(sankey = "{node: { colors: ['#B5482A'], width: 5}}")
  )

  return(plot)

  # message(sprintf("Writing Sankey diagram to %s", file.path(outputFile)))
  # writeLines(
  #   text = plot$html$chart,
  #   con = file.path(outputFile)
  # )
  # return(invisible(NULL))
}


createSankeyDiagramHTML <- function(
    treatmentPathways,
    outputFile,
    groupCombinations = FALSE,
    minFreq = 5) {
  data <- treatmentPathways %>%
    rowwise() %>%
    dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    dplyr::mutate(freq = as.integer(.data$freq))

  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")

  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")

  data[is.na(data)] <- "Stopped"

  result1 <- data %>%
    mutate(
      source = paste("1.", .data$path1),
      target = paste("2.", .data$path2)
    ) %>%
    select("source", "target", "freq")


  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste("2.", .data$path2),
        target = paste("3.", .data$path3)
      ) %>%
      select("source", "target", "freq")

    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }

  links <- links %>%
    dplyr::filter(.data$freq >= minFreq) %>%
    dplyr::mutate(`%` = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")

  if (groupCombinations) {
    links$source <- stringr::str_replace_all(
      string = links$source, "\\w+\\+\\w+", replacement = "Combination"
    )
    links$target <- stringr::str_replace_all(
      string = links$target, "\\w+\\+\\w+", replacement = "Combination"
    )
  }

  # Draw sankey network
  plot <- googleVis::gvisSankey(
    links,
    from = "source",
    to = "target",
    weight = "%",
    chartid = 1,
    options = list(sankey = "{node: { colors: ['#B5482A'], width: 5}}")
  )

  message(sprintf("Writing Sankey diagram to %s", file.path(outputFile)))
  writeLines(
    text = plot$html$chart,
    con = file.path(outputFile)
  )
  return(invisible(NULL))
}

#' `saveAsFile()` saves an SVG-image from an html-file
#'
#' @param fileName HTML-Filename.
#' @param fileNameOut Filename of image, either with .pdf or .png extension.
#' @param zoom Zoom factor, default = 3.
#' @param vwidth Width of frame to capture, default = 430.
#' @param selector Default: `NULL`; May be set to `"svg"` only works if the output file is pdf, otherwise use `NULL`
#' @param ... Other parameters to be passed to webshot2::webshot()
#'
#' @import checkmate
#' @import webshot2
#'
#' @return Invisibly returns the normalized path to all screenshots taken. The character vector will have a class of '"webshot"'.
#' @export
#'
#' @examples
#' \dontrun{
#'   saveAsFile("webFile.html", "outputFile.png")}
saveAsFile <- function(fileName, fileNameOut, zoom = 3, vwidth = 430, selector = NULL, ...) {
  # Assertions
  checkmate::assertCharacter(x = fileName, pattern = "*.html", null.ok = FALSE)
  checkmate::assertCharacter(x = fileNameOut, pattern = "*.pdf|*.png", null.ok = FALSE)
  checkmate::assertNumeric(x = zoom, lower = 0, len = 1, null.ok = FALSE)
  checkmate::assertNumeric(x = vwidth, lower = 0, len = 1, null.ok = FALSE)

  webshot2::webshot(
    url = fileName,
    file = fileNameOut,
    zoom = zoom,
    vwidth = vwidth,
    ...)
}

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

#' Help function to group combinations
#'
#' @param data
#'     Data
#' @param groupCombinations
#'     Group combinations
#'
#' @importFrom data.table data.table as.data.table
#'
#' @returns data.table
groupInfrequentCombinations <- function(data, groupCombinations) {
  data <- as.data.frame(data)

  # Find all non-fixed combinations occurring
  findCombinations <- apply(
    X = data,
    MARGIN = 2,
    FUN = function(x) {
      grepl("+", x, fixed = TRUE)
    }
  )

  # Group all non-fixed combinations in one group if TRUE
  if (groupCombinations == TRUE) {
    data[findCombinations] <- "Other"
  } else {
    # Otherwise: group infrequent treatments below groupCombinations as "other"
    combinations <- as.matrix(data)[findCombinations == TRUE]

    freqCombinations <- matrix(
      rep(data$freq, times = ncol(data)),
      ncol = ncol(data))[findCombinations == TRUE]

    summaryCombinations <- data.table::data.table(
      combination = combinations,
      freq = freqCombinations
    )

    if (nrow(summaryCombinations) > 0) {
      summaryCombinations <- summaryCombinations[
        , list(freq = sum(freq)),
        by = combination
      ][order(-freq)]

      summarizeCombinations <- summaryCombinations$combination[
        summaryCombinations$freq <= as.numeric(groupCombinations)]

      selectedCombinations <- apply(
        X = data,
        MARGIN = 2,
        FUN = function(x) {
          x %in% summarizeCombinations
        }
      )
      data[selectedCombinations] <- "Other"
    }
  }
  return(data.table::as.data.table(data))
}


