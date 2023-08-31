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

#' `createSunburstPlot()` exports sunburst plot from a data frame
#'
#' @param data A data frame containing two columns: 1) column "path" should specify the
#'     event cohorts separated by dashes - (combinations can be indicated using
#'     &) and 2) column "freq" should specify how often that (unique) path
#'     occurs.
#' @param folder Root folder to store the results.
#' @param fileName File name for the results.
#' @importFrom utils write.table
#'
#' @export
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' createSunburstPlot(
#'   data = data.frame(
#'     path = c("1", "2"),
#'     freq = c("0.5", "0.5")))
#' }
createSunburstPlot <- function(data, folder, fileName) {

  # Assertions
  checkmate::assertDataFrame(x = data)
  checkmate::checkSubset(x = names(data), choices = c("freq", "path"))
  checkmate::assertCharacter(x = folder, null.ok = TRUE)
  checkmate::assertCharacter(x = fileName, null.ok = TRUE)
  outcomes <- unique(unlist(strsplit(
    data$path,
    split = "-", fixed = TRUE
  )))

  # Load CSV file and convert to JSON
  json <- transformCSVtoJSON(
    data = data,
    outcomes = outcomes,
    folder = folder,
    fileName = unlist(stringr::str_split(string = fileName, "\\."))[2])

  # Load template HTML file
  html <- paste(
    readLines(
      file.path(
        system.file(package = "ReportGenerator"),
        "TreatmentPatterns",
        "sunburstPlot.html"
      )
    ),
    collapse = "\n"
  )

  # Replace @insert_data
  html <- sub("@insert_data", json, html)

  # Save HTML file
  writeLines(
    text = html,
    con = normalizePath(paste0(folder, fileName), mustWork = FALSE))
}

#' `outputSankeyDiagram()` generates the required Sankey Diagram
#'
#' @param data Dataframe with event cohorts of the target cohort in different columns.
#' @param outputFolder Path to the output folder.
#' @param groupCombinations Select to group all non-fixed combinations in one category "other" in the sunburst plot.
#' @param fileName File name of the html-file to output.
#'
#' @import dplyr
#' @importFrom googleVis gvisSankey
#'
#' @export
#'
#' @returns NULL
#' @examples
#' \dontrun{
#' outputSankeyDiagram(
#'     data = data
#'     outputFolder = "~/output",
#'     groupCombinations = "other",
#'     fileName = "sankeyDiagram.html")
#' }
outputSankeyDiagram <- function(
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

  # writeLines(
  #   text = plot$html$chart,
  #   con = normalizePath(paste0(outputFolder, fileName), mustWork = FALSE)
  # )
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

#' Finds the depth of a list element.
#'
#' @param x input list (element)
#' @param thisdepth current list depth
#'
#' @return the depth of the list element
depth <- function(x, thisdepth = 0) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertNumeric(x = thisdepth, len = 1, lower = 0, null.ok = FALSE)

  if (!is.list(x)) {
    return(thisdepth)
  } else {
    return(max(unlist(
      lapply(x, depth, thisdepth = thisdepth + 1)
    )))
  }
}
#' Removes the name from all levels of list.
#'
#' @param x input list
#' @param name the name of the list item from which the names will be removed
#'
#' @return list with removed names
stripname <- function(x, name) {
  # Assertions
  checkmate::assertTRUE(!is.null(x))
  checkmate::assertCharacter(x = name, len = 1, null.ok = FALSE)

  thisdepth <- depth(x)
  if (thisdepth == 0) {
    return(x)
  } else if (length(nameIndex <- which(names(x) == name)) > 0) {
    element <- names(x)[nameIndex]
    x[[element]] <- unname(x[[element]])
  }
  return(lapply(x, stripname, name))
}
#' Creates a hierarchical data structure.
#'
#' @param csv
#'     CSV
#'
#' @import stringr
#' @importFrom rjson toJSON
#'
#' @returns JSON
buildHierarchy <- function(csv) {
  root <- list("name" = "root", "children" = list())

  # Create nested structure of lists
  for (i in 1:nrow(csv)) {
    sequence <- csv[i, 1]
    size <- csv[i, 2]

    parts <- unlist(stringr::str_split(sequence, pattern = "-"))

    currentNode <- root

    for (j in 1:length(parts)) {
      children <- currentNode[["children"]]
      nodeName <- parts[j]

      if (j < length(parts)) {
        # Not yet at the end of the sequence; move down the tree
        foundChild <- FALSE

        if (length(children) != 0) {
          for (k in 1:length(children)) {
            if (children[[k]]$name == nodeName) {
              childNode <- children[[k]]
              foundChild <- TRUE
              break
            }
          }
        }

        # If we dont already have a child node for this branch, create it
        if (!foundChild) {
          childNode <- list("name" = nodeName, "children" = list())
          children[[nodeName]] <- childNode

          # Add to main root
          if (j == 1) {
            root[["children"]] <- children
          } else if (j == 2) {
            root[["children"]][[parts[1]]][["children"]] <- children
          } else if (j == 3) {
            root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]] <-
              children
          } else if (j == 4) {
            root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]] <-
              children
          } else if (j == 5) {
            root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]][[parts[4]]][["children"]] <-
              children
          }
        }
        currentNode <- childNode
      } else {
        # Reached the end of the sequence; create a leaf node
        childNode <- list("name" = nodeName, "size" = size)
        children[[nodeName]] <- childNode

        # Add to main root
        if (j == 1) {
          root[["children"]] <- children
        } else if (j == 2) {
          root[["children"]][[parts[1]]][["children"]] <- children
        } else if (j == 3) {
          root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]] <-
            children
        } else if (j == 4) {
          root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]] <-
            children
        } else if (j == 5) {
          root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]][[parts[4]]][["children"]] <-
            children
        }
      }
    }
  }

  # Remove list names
  root <- suppressWarnings(stripname(root, "children"))

  # Convert nested list structure to json
  json <- rjson::toJSON(root)
  return(json)
}
#' Transforms CSV into JSON for HTML
#'
#' @param data input data.frame
#' @param outcomes character vector containing all event cohorts
#' @param folder output folder
#' @param fileName output file name
#'
#' @import stringr
#' @importFrom stringi stri_replace_all_fixed
#'
#' @return the transformed csv as a json string
transformCSVtoJSON <- function(data, outcomes, folder, fileName) {
  # Assertions
  checkmate::assertDataFrame(x = data)
  checkmate::assertCharacter(x = outcomes, null.ok = FALSE)
  #checkmate::assertDirectoryExists(x = folder)
  checkmate::assertCharacter(x = fileName, len = 1, null.ok = FALSE)
  # Add bitwise numbers to define combination treatments
  bitwiseNumbers <- sapply(
    X = seq_along(outcomes),
    FUN = function(o) {
      2 ^ (o - 1)
    })

  linking <- data.frame(outcomes, bitwiseNumbers)

  # Generate lookup file
  series <- sapply(
    X = seq_len(nrow(linking)),
    FUN = function(row) {
      paste0(
        '{ "key": "', linking$bitwiseNumbers[row],
        '", "value": "', linking$outcomes[row], '"}')
    })

  series <- c(series, '{ "key": "End", "value": "End"}')
  lookup <- paste0("[", paste(series, collapse = ","), "]")

  # Order names from longest to shortest to adjust in the right order
  linking <- linking[
    order(-sapply(linking$outcomes, function(x) stringr::str_length(x))), ]

  # Apply linking
  # Change all outcomes to bitwise number
  updated_path <- sapply(data$path, function(p) {
    stringi::stri_replace_all_fixed(
      p,
      replacement = as.character(linking$bitwiseNumbers),
      pattern = as.character(linking$outcomes),
      vectorize = FALSE)
  })

  # Sum the bitwise numbers of combinations (indicated by +)
  digitsPlusRegex <- "[[:digit:]]+[+][[:digit:]]+"
  updated_path <- sapply(
    X = updated_path,
    FUN = function(p) {
      while (!is.na(stringr::str_extract(p, digitsPlusRegex))) {
        pattern <- stringr::str_extract(p, digitsPlusRegex)
        p <- sub(digitsPlusRegex, eval(parse(text = pattern)), p)
      }
      return(p)
    })

  transformed_json <- buildHierarchy(cbind(oath = updated_path,
                                           freq = data$freq))

  result <- paste0(
    "{ \"data\" : ", transformed_json, ", \"lookup\" : ", lookup, "}")

  file <- paste0(folder, fileName)
  writeLines(text = result, con = file)
  # close(file)
  return(result)
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


