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

#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
reportGenerator <- function() {

  # TODO filter itemsList based on the files that have been actually uploaded
  # uploadedFiles <- c("incidence_attrition_example_data.csv",
  #                    "incidence_estimates_example_data.csv",
  #                    "prevalence_attrition_example_data.csv")
  # itemsList <- getItemsList(uploadedFiles)

  ui <- fluidPage(
    # tags$head(
    #   tags$style(HTML(".bucket-list-container {min-height: 700px;}"))
    # ),
    fluidRow(
      column(
        width = 12,
        tags$b("Load data"),
        column(
          width = 12,
          # File input field
          fileInput("datasetLoad",
                    "Upload zip folder or csv file",
                    accept = c(".zip",
                               ".csv"),
                    multiple = TRUE
          ),

          # Print to monitor
          verbatimTextOutput("uploadedFiles"),

          # # Print to monitor
          verbatimTextOutput("incidence_estimates"),

          # Reset data button
          actionButton('resetData',
                       'Reset data')
        )
      )
    ),
    fluidRow(

      # Item selection menu

      uiOutput("itemSelectionMenu")

      # column(
      #   tags$b("Item selection"),
      #   width = 12,
      #   bucket_list(
      #     header = "Select the figures you want in the report",
      #     group_name = "bucket_list_group",
      #     orientation = "horizontal",
      #     add_rank_list(
      #       text = "Drag from here",
      #       labels = itemsList$title,
      #       input_id = "objectsList1"
      #     ),
      #     add_rank_list(
      #       text = "to here",
      #       labels = NULL,
      #       input_id = "objectsList2"
      #     )
      #   )
      # )



    ),
    fluidRow(
      column(
        width = 12,
        tags$b("Preview"),
        column(
          width = 12,

          # tags$p("input$objectsList1"),
          # verbatimTextOutput("results_1"),
          # tags$p("Tables and figures in the report"),

          # Preview, list and objects

          tableOutput("table"),


          actionButton("generateReport", "Generate Report")
        )
      )
    )
  )


  server <- function(input,output) {

    # Load data

    #
    # dataVariables <- reactiveValues()
    #
    # observeEvent(input$datasetLoad, {
    #
    #     inFile <- req(input$datasetLoad)
    #     dataVariables <- unzip(inFile$datapath, list = TRUE)
    #
    #
    #   })
    #
    # output$uploadedFiles <-
    #   renderPrint(
    #     dataVariables # Matches the group_name of the bucket list
    #   )


    # 1. Load files

    # Reset data variables

    resetDatasetLoad <- reactiveValues(data = NULL)

    observeEvent(input$datasetLoad, {

      resetDatasetLoad$data <- NULL

      })

    observeEvent(input$resetData, {

      resetDatasetLoad$data <- "resetDatasetLoad"

      })

    # Upload files

    uploadedFiles <- reactive({

      inFile <- input$datasetLoad

      applyReset <- resetDatasetLoad$data

      if (is.null(inFile)) {

        return(NULL)

      } else if (!is.null(applyReset)) {

        return(NULL)

      } else if (!is.null(inFile)) {

        uploadedFiles <- inFile$datapath

        if (grepl(".zip",
                  uploadedFiles,
                  fixed = TRUE)) {

          csvLocation <- tempdir()

          unzip(uploadedFiles, exdir = csvLocation)

          csvFiles <- list.files(path = csvLocation,
                                 pattern = ".csv",
                                 full.names = TRUE)

        }

        return(csvFiles)

      }

    })

    # To UI

    output$uploadedFiles <- renderPrint(
        uploadedFiles()
      )

    # Render UI itemSelectionMenu

    itemsList <- reactive({

      inFile <- input$datasetLoad

      applyReset <- resetDatasetLoad$data

      if (is.null(inFile)) {

        return(NULL)

      } else if (!is.null(applyReset)) {

        return(NULL)

      } else if (!is.null(inFile)) {

        itemsList <- c()

        # TEST uploadedFiles
        # uploadedFiles <- list.files(path = "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\Rtmponi7Jt",
        #                             pattern = ".csv",
        #                             full.names = TRUE)

        # PROD
        for (i in uploadedFiles()) {

        # TEST for loop uploadedFiles
        # for (i in uploadedFiles) {

          # TEST i
          # i <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\Rtmponi7Jt/indicationSummaryWide.csv"

          resultsData <- read_csv(i)

          resultsColumns <- names(resultsData)

          configData <- read.csv(system.file("config/variablesConfig.csv",
                                             package = "ReportGenerator"))

          configDataTypes <- unique(configData$name)

          for (val in configDataTypes) {

            # TEST val
            # val <- "largeScaleSummary"

            configColumns <- configData %>% filter(name == val)

            configColumns <- configColumns$variables

            if (length(configColumns) == length(resultsColumns)) {

              if (identical(configColumns, resultsColumns)) {

                itemsList <- append(itemsList, val)

            }

            }

          }

        }

        result <- itemsList

        menuList <- read.csv(system.file("config/itemsConfig.csv",
                                         package = "ReportGenerator"),
                             sep = ";") %>%
          dplyr::mutate(signature = paste0(name, "(", arguments, ")"))

        checkNeeds <- function(menuList, uploadedFiles) {
          unlist(lapply(menuList, FUN = function(menuList) {
            required <- trimws(unlist(strsplit(menuList, ",")))
            exists <- required %in% uploadedFiles

            if (TRUE %in% exists) {

              return(TRUE)

              } else {

              return(FALSE)

              }

            })

          )}

        menuList %>%
          dplyr::filter(checkNeeds(menuList$arguments, result)) %>%
          dplyr::select(title, signature)

    }

      })

    # output$itemsList <- renderPrint(
    #   itemsList()
    # )
#
    output$itemSelectionMenu <- renderUI({

      column(
        tags$b("Item selection"),
        width = 12,
        bucket_list(
          header = "Select the figures you want in the report",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels = itemsList()$title,
            input_id = "objectsList1"
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = "objectsList2"
          )
        )
      )



    })


    # Objects list


    objectsList <- reactive({

      Index <- seq(1:length(input$objectsList2))

      Contents  <- input$objectsList2

      objectsDataFrame <- data.frame(Index, Contents)

      objectsDataFrame

    })

    output$table <- renderTable(objectsList())


    # Data variables

    # Uploaded object: Incidence Attrition

    incidence_attrition <- reactive({

      for (i in uploadedFiles()) {

        configColumns <- read.csv(system.file("config/variablesConfig.csv",
                                              package = "ReportGenerator")) %>%
          dplyr::filter(name == "incidence_attrition")

        configColumns <- configColumns$variables

        # i <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpAdrrwm/incidence_attrition.csv"
        csvData <- read_csv(i)

        resultsColumns <- names(csvData)

        if (length(configColumns) == length(resultsColumns)) {

          if (identical(configColumns, resultsColumns)) {

            result <- csvData

          }

        }

        }

      return(result)

    })

    # Uploaded object: Prevalence Attrition

    prevalence_attrition <- reactive({

      for (i in uploadedFiles()) {

        configColumns <- read.csv(system.file("config/variablesConfig.csv",
                                              package = "ReportGenerator")) %>%
          dplyr::filter(name == "prevalence_attrition")

        configColumns <- configColumns$variables

        # i <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpAdrrwm/prevalence_attrition.csv"

        csvData <- read_csv(i)

        resultsColumns <- names(csvData)

        if (length(configColumns) == length(resultsColumns)) {

          if (identical(configColumns, resultsColumns)) {

            result <- csvData

          }

        }

      }

      return(result)

    })

    # Uploaded object: Incidence Estimates

    incidence_estimates <- reactive({

      for (i in uploadedFiles()) {

        configColumns <- read.csv(system.file("config/variablesConfig.csv",
                                              package = "ReportGenerator")) %>%
          dplyr::filter(name == "incidence_estimates")

        configColumns <- configColumns$variables

        # i <- "C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpAdrrwm/incidence_estimates.csv"

        csvData <- read_csv(i)

        resultsColumns <- names(csvData)

        if (length(configColumns) == length(resultsColumns)) {

          if (identical(configColumns, resultsColumns)) {

            result <- csvData

          }

        }

      }

      return(result)

    })

    # Report generator

    observeEvent(input$generateReport, {

      incidencePrevalenceDocx <- read_docx(path = here("inst/templates/word/darwinTemplate.docx"))

      # styles_info(incidencePrevalenceDocx)

      reverseList <- rev(input$objectsList2)

      menuList <- read.csv(system.file("config/itemsConfig.csv",
                                       package = "ReportGenerator"),
                           sep = ";")

      menuList$arguments <- gsub("incidence_attrition" , "incidence_attrition()", menuList$arguments)
      menuList$arguments <- gsub("prevalence_attrition" , "prevalence_attrition()", menuList$arguments)
      menuList$arguments <- gsub("incidence_estimates" , "incidence_estimates()", menuList$arguments)

      menuList <- menuList %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))

      for (i in reverseList) {

        object <- eval(parse(text = menuList %>%
                               dplyr::filter(title == i) %>%
                               dplyr::pull(signature)))

        if ("flextable" %in% class(object)) {

          body_add_flextable(incidencePrevalenceDocx,
                             value = object)

          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")

          } else if ("ggplot" %in% class(object)) {

            body_add_gg(x = incidencePrevalenceDocx,
                        value = object,
                        style = "Normal")

            body_add(incidencePrevalenceDocx,
                     value = i,
                     style = "Heading 1 (Agency)")

            } else {

              body_add_table(incidencePrevalenceDocx,
                             value = object,
                             style = "TableOverall")

              body_add(incidencePrevalenceDocx,
                       value = i,
                       style = "Heading 1 (Agency)")

            }

        }

      body_add_toc(incidencePrevalenceDocx)

      print(incidencePrevalenceDocx,
            target = here("Reports/generatedReport.docx"))
    })
  }
  shinyApp(ui, server)
}

if(getRversion() >= "2.15.1")    utils::globalVariables(c("incidence_attrition",
                                                          "prevalence_attrition",
                                                          "incidence_estimates"))


