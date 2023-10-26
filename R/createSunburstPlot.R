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

addChild <- function(j, children, parts, root) {
  switch(j,
         root[["children"]] <- children,
         root[["children"]][[parts[1]]][["children"]] <- children,
         root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]] <- children,
         root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]] <- children,
         root[["children"]][[parts[1]]][["children"]][[parts[2]]][["children"]][[parts[3]]][["children"]][[parts[4]]][["children"]] <- children
  )
  return(root)
}

buildHierarchy <- function(csv) {
  root <- list(
    name = "root",
    children = list()
  )

  # Create nested structure of lists
  for (i in seq_len(nrow(csv))) {
    sequence <- csv[i, 1]
    size <- csv[i, 2]

    parts <- unlist(stringr::str_split(sequence, pattern = "-"))

    currentNode <- root

    for (j in seq_len(length(parts))) {
      children <- currentNode[["children"]]
      nodeName <- parts[j]

      if (j < length(parts)) {
        # Not yet at the end of the sequence; move down the tree
        foundChild <- FALSE

        if (length(children) != 0) {
          for (k in seq_len(length(children))) {
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
          root <- addChild(j, children, parts, root)
        }
        currentNode <- childNode
      } else {
        # Reached the end of the sequence; create a leaf node
        childNode <- list("name" = nodeName, "size" = size)
        children[[nodeName]] <- childNode

        # Add to main root
        root <- addChild(j, children, parts, root)
      }
    }
  }

  # Remove list names
  root <- suppressWarnings(stripname(root, "children"))

  # Convert nested list structure to json
  json <- rjson::toJSON(root)
  return(json)
}

transformCSVtoJSON <- function(data, outcomes) {
  # Add bitwise numbers to define combination treatments
  bitwiseNumbers <- sapply(
    X = seq_along(outcomes),
    FUN = function(o) {
      2^(o - 1)
    }
  )

  linking <- data.frame(outcomes, bitwiseNumbers)

  # Generate lookup file
  series <- sapply(
    X = seq_len(nrow(linking)),
    FUN = function(row) {
      paste0(
        '{ "key": "', linking$bitwiseNumbers[row],
        '", "value": "', linking$outcomes[row], '"}'
      )
    }
  )

  series <- c(series, '{ "key": "End", "value": "End"}')
  lookup <- paste0("[", paste(series, collapse = ","), "]")

  # Order names from longest to shortest to adjust in the right order
  linking <- linking[
    order(-sapply(linking$outcomes, function(x) stringr::str_length(x))),
  ]

  # Apply linking
  # Change all outcomes to bitwise number
  updated_path <- sapply(data$path, function(p) {
    stringi::stri_replace_all_fixed(
      p,
      replacement = as.character(linking$bitwiseNumbers),
      pattern = as.character(linking$outcomes),
      vectorize = FALSE
    )
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
    }
  )
  transformed_csv <- data.frame(
    path = updated_path,
    freq = data$freq,
    path_length = stringr::str_count(updated_path, "-")
  )

  transformed_csv <- transformed_csv[order(transformed_csv$path_length),]
  transformed_json <- buildHierarchy(transformed_csv)

  result <- paste0(
    "{ \"data\" : ", transformed_json, ", \"lookup\" : ", lookup, "}"
  )
  # close(file)
  return(result)
}


createTreatmentPathways <- function(treatmentHistory) {
  treatmentPathways <- treatmentHistory %>%
    dplyr::group_by(.data$personId, .data$indexYear) %>%
    dplyr::summarise(
      pathway = list(.data$eventCohortName[.data$eventSeq]),
      .groups = "drop"
    )

  # layers <- treatmentPathways %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(l = length(.data$pathway)) %>%
  #   dplyr::select("l") %>%
  #   max()

  treatmentPathways <- treatmentPathways %>%
    dplyr::group_by(.data$indexYear, .data$pathway) %>%
    dplyr::summarise(freq = length(.data$personId), .groups = "drop")

  return(treatmentPathways)
}

prepData <- function(treatmentHistory, year) {
  treatmentPathways <- createTreatmentPathways(treatmentHistory)

  dat <- treatmentPathways %>%
    rowwise() %>%
    mutate(path = paste(.data$pathway, collapse = "-")) %>%
    select("indexYear", "path", "freq")

  if (!is.na(year) || !is.null(year)) {
    if (year == "all") {
      dat <- dat %>%
        group_by(.data$path) %>%
        summarise(freq = sum(.data$freq))
    } else {
      dat <- dat %>%
        filter(.data$indexYear == year)
      if (nrow(dat) == 0) {
        NULL
        # message(sprintf("Not enough data for year: %s", year))
      }
    }
  }
  return(dat)
}

toList <- function(json) {
  # Load template HTML file
  html <- paste(
    readLines(
      system.file(
        package = "ReportGenerator",
        "templates",
        "htmlTemplates", "sunburst_shiny.html"
      )
    ),
    collapse = "\n"
  )

  legend <- paste(
    readLines(
      system.file(
        package = "ReportGenerator",
        "templates",
        "htmlTemplates", "legend.html"
      )
    ),
    collapse = "\n"
  )

  legend <- sub("@insert_data", json, legend)

  # Replace @insert_data
  html <- sub("@insert_data", json, html)

  list(sunburst = html, legend = legend)
}

toFile <- function(json, treatmentPathways, outputFile) {
  # Load template HTML file
  html <- paste(
    readLines(
      system.file(
        package = "ReportGenerator",
        "templates",
        "htmlTemplates", "sunburst_standalone.html"
      )
    ),
    collapse = "\n"
  )

  # Replace @insert_data
  html <- sub("@insert_data", json, html)
  html <- sub(
    "@name",
    sprintf(
      "Strata:\n\nAges: %s\nSex: %s\nYears: %s\n",
      paste(unique(treatmentPathways$age), collapse = ", "),
      paste(unique(treatmentPathways$sex), collapse = ", "),
      paste(unique(treatmentPathways$indexYear), collapse = ", ")
    ),
    html
  )

  message(sprintf(
    "Writing sunburst plot to %s",
    file.path(outputFile)
  ))

  # Save HTML file
  writeLines(
    text = html,
    con = file.path(outputFile)
  )
}

createSunburstPlot <- function(treatmentPathways, outputFile, groupCombinations = FALSE, returnHTML = FALSE) {
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )

  data <- treatmentPathways %>%
    mutate(freq = as.integer(.data$freq)) %>%
    select("path", "freq")

  outcomes <- unique(unlist(strsplit(
    data$path,
    split = "-", fixed = TRUE
  )))

  # Load CSV file and convert to JSON
  json <- transformCSVtoJSON(
    data = data,
    outcomes = outcomes
  )

  if (returnHTML) {
    return(toList(json))
  } else {
    toFile(json, treatmentPathways, outputFile)
  }
}

doGroupCombinations <- function(treatmentPathways, groupCombinations) {
  if (groupCombinations) {
    treatmentPathways$path <- treatmentPathways$path %>%
      stringr::str_replace_all(
        pattern = "\\w+\\+\\w+",
        replacement = "Combination"
      )
  }
  return(treatmentPathways)
}

createSunburstPlot2 <- function(treatmentPathways, groupCombinations = FALSE, ...) {
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )

  sunburstR::sunburst(data = treatmentPathways, ...)
}
