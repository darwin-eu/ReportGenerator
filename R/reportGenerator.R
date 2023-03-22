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
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable DT flextable
reportGenerator <- function() {

  ui <- fluidPage(
    # tags$head(
    #   tags$style(HTML(".bucket-list-container {min-height: 700px;}"))
    # ),
    fluidRow(

      column(width = 12, tags$b("Load data"),

        column(width = 12,
               # File input field
               fileInput("datasetLoad",
                         "Upload zip folder or csv file",
                         accept = c(".zip", ".csv"),
                         multiple = TRUE),
               # Print to monitor
               verbatimTextOutput("uploadedFiles"),

               # Reset data button
               actionButton('resetData', 'Reset data')
               )
        )
      ),

    fluidRow(
      # Item selection menu
      uiOutput("itemSelectionMenu")

    ),

    fluidRow(

      column(width = 12,
             tags$b("Item preview"),

        fluidRow(

          column(width = 4,
            fluidRow(DT::dataTableOutput("tableContents")),
            fluidRow(actionButton("generateReport", "Generate Report"))),

          column(width = 8,
                 uiOutput("itemPreview"))

        )

      )
    )
  )


  server <- function(input,output) {

    # Load data

    # 1. Load files

    # 1.1 Reset data variables

    resetDatasetLoad <- reactiveValues(data = NULL)

    observeEvent(input$datasetLoad, {

      resetDatasetLoad$data <- NULL

      })

    observeEvent(input$resetData, {

      if (!is.null(uploadedFiles())) {

        lapply(uploadedFiles(), file.remove)

        resetDatasetLoad$data <- "resetDatasetLoad"

      } else {

        resetDatasetLoad$data <- "resetDatasetLoad"

      }

      })

    # 1.2 Upload files

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

    # Test to UI

    output$uploadedFiles <- renderPrint(
        uploadedFiles()
      )

    # 2. Item selection menu

    # 2.1 Items list

    itemsList <- reactive({

      inFile <- input$datasetLoad

      applyReset <- resetDatasetLoad$data

      if (is.null(inFile)) {

        return(NULL)

      } else if (!is.null(applyReset)) {

        return(NULL)

      } else if (!is.null(inFile)) {

        itemsList <- c()

        for (i in uploadedFiles()) {

          resultsData <- read_csv(i)

          resultsColumns <- names(resultsData)

          configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))

          configDataTypes <- unique(configData$name)

          for (val in configDataTypes) {

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

        menuFun  <- read.csv(system.file("config/itemsConfig.csv", package = "ReportGenerator"),
                             sep = ";") %>%
          dplyr::mutate(signature = paste0(name, "(", arguments, ")"))

        checkNeeds <- function(menuFun , uploadedFiles) {

          unlist(lapply(menuFun , FUN = function(menuFun ) {

              required <- trimws(unlist(strsplit(menuFun , ",")))

              exists <- required %in% uploadedFiles

            if (TRUE %in% exists) {

              return(TRUE)

              } else {

              return(FALSE)

              }

            })

          )}

        menuFun  %>%
          dplyr::filter(checkNeeds(menuFun $arguments, result)) %>%
          dplyr::select(title, signature)

        }

      })

<<<<<<< HEAD
    # output$itemsList <- renderPrint(
    #   itemsList()
    # )


    ## UI Menu

=======
>>>>>>> itemPreview
    output$itemSelectionMenu <- renderUI({

      column(tags$b("Item selection"),

             width = 12,

             bucket_list(header = "Select the figures you want in the report",

                         group_name = "bucket_list_group",

                         orientation = "horizontal",

                         add_rank_list(text = "Drag from here",

                                       labels = itemsList()$title,

                                       input_id = "objectMenu"),

                         add_rank_list(text = "to here",

                                       labels = NULL,

                                       input_id = "objectSelection"

                           )

                         )
             )

      })

    # 2.3 Load Data variables

    # Uploaded object: Incidence Attrition

    incidence_attrition <- reactive({

      for (i in uploadedFiles()) {

        configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%

          dplyr::filter(name == "incidence_attrition")

        configColumns <- configColumns$variables

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

        configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%

          dplyr::filter(name == "prevalence_attrition")

        configColumns <- configColumns$variables

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

        configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%

          dplyr::filter(name == "incidence_estimates")

        configColumns <- configColumns$variables

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

    # 3. Item preview

    # 3.1 Objects list. From the sortable menu.

    menu <- reactive({

      contents  <- input$objectSelection

      objectsDataFrame <- data.frame(contents)

      objectsDataFrame

    })

    # Functions available from the configuration file

    menuFun  <- reactive({

      menuFun  <- read.csv(system.file("config/itemsConfig.csv", package = "ReportGenerator"), sep = ";")

      menuFun$arguments <- gsub("incidence_attrition" , "incidence_attrition()", menuFun$arguments)

      menuFun$arguments <- gsub("prevalence_attrition" , "prevalence_attrition()", menuFun$arguments)

      menuFun$arguments <- gsub("incidence_estimates" , "incidence_estimates()", menuFun$arguments)

      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))

      menuFun

    })

    # 3.2 Preview Table

    # Table of contents to the UI

    output$tableContents <- DT::renderDataTable(menu(),
                                                options = list(dom = "t"),
                                                selection = list(mode = "single", target = "cell"),
                                                rownames = FALSE)

    # Item choice data

    objectChoice  <- reactive({

      req(input$tableContents_cells_selected)

      objectSelection  <- menu()[input$tableContents_cells_selected,]

      # objectSelection <- "Plot - Incidence rate per year"

      if (length(objectSelection) >= 1) {
        object <- eval(parse(text = menuFun() %>%
                               dplyr::filter(title == objectSelection) %>%
                               dplyr::pull(signature)))

        object
      }
    })

    # Renders item preview depending on the object class

    output$itemPreview <- renderUI({

      req(objectChoice())

      if (is(objectChoice(), "flextable")) {

        tableOutput("tableContentTable")

      } else {

        plotlyOutput("tableContentPlot")

      }

    })

    # Objects to be rendered in the UI

    output$tableContentTable <- renderUI({

      req(objectChoice())

      if (is(objectChoice(), "flextable")) {

        htmltools_value(objectChoice())

        }

      })

    output$tableContentPlot<- renderPlotly({

      req(objectChoice())

      if (!is(objectChoice(), "flextable")) {

        objectChoice()

        }

      })

    # 4. Word report generator

    observeEvent(input$generateReport, {

      incidencePrevalenceDocx <- read_docx(path = here("inst/templates/word/darwinTemplate.docx"))

      # styles_info(incidencePrevalenceDocx)

<<<<<<< HEAD
      reverseList <- rev(input$objectsList2)

      menuList <- read.csv(system.file("config/itemsConfig.csv",
                                       package = "ReportGenerator"), sep = ";")

      menuList$arguments <- gsub("incidence_attrition" ,
                                 "incidence_attrition()",
                                 menuList$arguments)

      menuList$arguments <- gsub("prevalence_attrition" ,
                                 "prevalence_attrition()",
                                 menuList$arguments)

      menuList$arguments <- gsub("incidence_estimates" ,
                                 "incidence_estimates()",
                                 menuList$arguments)

      menuList <- menuList %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
=======
      reverseList <- rev(input$objectSelection)
>>>>>>> itemPreview

      for (i in reverseList) {

        object <- eval(parse(text = menuFun() %>%
                               dplyr::filter(title == i) %>%
                               dplyr::pull(signature)))

        if ("flextable" %in% class(object)) {

          body_add_flextable(incidencePrevalenceDocx, value = object)

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


