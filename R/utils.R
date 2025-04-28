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
  result <- unlist(result) %>%
    unique()
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
#' @param reportApp If TRUE, it will pull the menuConfig yaml file from the reportApp folder.
#'
#' @return a dataframe with the properties of the items
getItemConfig <- function(input = NULL,
                          output = NULL,
                          inputValue = NULL,
                          reportApp = FALSE) {

  checkmate::assertSetEqual(length(input), 1)
  checkmate::assertSetEqual(length(output), 1)
  checkmate::assertSetEqual(length(inputValue), 1)
  checkmate::assertCharacter(input)
  checkmate::assertCharacter(output)
  checkmate::assertCharacter(inputValue)

  if (!reportApp) {
    menuData <- yaml.load_file(system.file("config", "menuConfig.yaml", package = "ReportGenerator"))
  } else {
    menuData <- yaml.load_file(here::here("config", "menuConfig.yaml"))
  }
  functionText <- lapply(menuData, function(menuData, type) {
    if (any(inputValue %in% menuData[[input]])) {
      menuData[[output]]
    }
  }, type = type)
  result <- list()
  for (i in functionText) {
    if (!is.null(i)) {
      result <- rbind(result, i)
    }
  }
  result <- unlist(result)
  if (is.null(result)) {
    cli::cli_alert_danger("No item found in the menuConfig.yaml file.")
  }
  return(result)
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
    } else if (workingName == "treatment_pathways" | workingName == "metadata" | workingName == "cdm_source_info" | workingName == "summary_event_duration" | workingName == "incidence_attrition" | workingName == "prevalence_point_attrition" | workingName == "prevalence_period_attrition") {
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

createDownloadPlotUI <- function(ns, type = "download_plot") {
  tagList(column(2, div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns(paste0(type, "_height")), "", 10, width = "50px"))),
          column(2, div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns(paste0(type, "_width")), "", 20, width = "50px"))),
          column(2, div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
                 div(style = "display: inline-block;margin-top:5px", textInput(ns(paste0(type, "_dpi")), "", 300, width = "50px"))),
          column(2, tagList(shiny::HTML("<label class = 'control-label'>&#8205;</label>"),
                            shiny::br(), downloadButton(ns(type), "Download Plot"))))
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

analysisNamesAvailable <- function(settingsData) {
  # settingsData <- settings(uploaded_filesList)
  result_types_available <- settingsData %>%
    pull(result_type) %>%
    unique()

  result_types_available <- result_types_available[grepl("incidence|incidence_attrition|incidence_attrition|prevalence|prevalence_attrition|summarise_large_scale_characteristics|summarise_characteristics", result_types_available)]

  if ("analysis_type" %in% colnames(settingsData)) {

    analysis_types_available <- settingsData %>%
      pull(analysis_type) %>%
      unique()

    analysis_types_available <- analysis_types_available[grepl("single_event|competing_risk", analysis_types_available)]

  } else {

    analysis_types_available <- c()

  }



  items_available <- c(result_types_available, analysis_types_available)

  return(items_available)
}

sunburstPathways <- function(pathwaysData) {

  data_all <- pathwaysData %>% mutate(freq = as.numeric(freq))

  data_split <- data_all %>%
    separate(path, into = c("level1", "level2"), sep = "/", fill = "right", extra = "merge") %>%
    mutate(level2 = if_else(is.na(level2), level1, level2))

  ring_inner <- data_split %>%
    group_by(cdm_name, level1) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    mutate(level1 = forcats::fct_inorder(level1)) %>%
    arrange(cdm_name, level1) %>%
    group_by(cdm_name) %>%
    mutate(prop = freq / sum(freq),
           start = lag(cumsum(prop), default = 0),
           stop = cumsum(prop)) %>%
    ungroup()

  ring_outer <- data_split %>%
    group_by(cdm_name, level1, level2) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    group_by(cdm_name, level1) %>%
    mutate(prop_within = freq / sum(freq),
           cum_within = cumsum(prop_within),
           cum_within_prev = lag(cum_within, default = 0)) %>%
    ungroup() %>%
    left_join(ring_inner %>% select(cdm_name, level1, start, stop),
              by = c("cdm_name", "level1")) %>%
    mutate(outer_start = start + cum_within_prev * (stop - start),
           outer_stop = start + cum_within * (stop - start))

  ggplot2::ggplot() +
    ggplot2::geom_rect(data = ring_outer,
                       ggplot2::aes(xmin = outer_start, xmax = outer_stop,
                  ymin = 1, ymax = 20, fill = level2),
              color = "white") +
    ggplot2::geom_rect(data = ring_inner,
                       ggplot2::aes(xmin = start, xmax = stop,
                  ymin = -20, ymax = 0, fill = level1),
              color = "white") +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text  = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          legend.position = "right") +
    ggplot2::ylim(-40, 30) +
    ggplot2::labs(fill = "Category") +
    ggplot2::facet_wrap(~ cdm_name, ncol = 2)
}
getDarwinStyle <- function() {
  style <- list(header = list(gt::cell_fill(color = "#003399"),
                              gt::cell_text(weight = "bold",
                                            align = "center")),
                header_name = list(gt::cell_fill(color = "#003399"),
                                   gt::cell_text(weight = "bold",
                                                 color = "white",
                                                 align = "center")),
                header_level = list(gt::cell_fill(color = "#003399"),
                                    gt::cell_text(color = "white",
                                                  weight = "bold",
                                                  align = "center")),
                column_name = list(gt::cell_fill(color = "#003399"),
                                   gt::cell_text(weight = "bold",
                                                 color = "white",
                                                 align = "center")),
                group_label = list(gt::cell_fill(color = "#e9e9e9"),
                                   gt::cell_text(weight = "bold")),
                title = list(gt::cell_text(weight = "bold",
                                           size = 15,
                                           align = "center")),
                subtitle = list(gt::cell_text(weight = "bold",
                                              size = 12,
                                              align = "center")),
                body = list())
  return(style)
}
