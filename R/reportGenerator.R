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

#' ReportGenerator Shiny App
#'
#' `ReportGenerator()` launches the package's main app. The user can upload a zip folder, and the function detects what figures and tables are available to generate a Word report.
#'
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer flextable waldo readr yaml data.tree
#' @importFrom sortable bucket_list add_rank_list
#' @export
reportGenerator <- function() {

  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)

  ui <- dashboardPage(
    dashboardHeader(title = "Report Generator"),
    dashboardSidebar(
      sidebarMenu(
        tagList(tags$br(),
                tags$br(),
                tags$div(tags$h4("Load data"), class = "form-group shiny-input-container"),
                fileInput("datasetLoad",
                          "Upload zip folder or csv file",
                          accept = c(".zip", ".csv"),
                          multiple = TRUE),
                actionButton('resetData', 'Reset data')
        ))
    ),
    dashboardBody(
      tabsetPanel(
        tabPanel("Item selection",
        # Item selection menu
        fluidRow(
          box(uiOutput("itemSelectionMenu"),
              tags$br())
        )),
        tabPanel("Item preview",
          fluidRow(
          column(width = 12,
                 fluidRow(
                   box(tags$b("Item preview"),
                       DT::dataTableOutput("tableContents"),
                       actionButton("generateReport", "Generate Report"),
                       width = 4),
                   box(uiOutput("plotFilters"),
                       uiOutput("itemPreview"),
                       width = 8)
                   )
                 )
          )
        )
      )
    )
  )


  server <- function(input,output) {

    # Load data

    # 1. Load files

    # 1.1 Reset data variables

    uploadedFiles <- reactiveValues(data = NULL)
    # Test
    # uploadedFiles <- list()

    itemsList <- reactiveValues(objects = NULL)
    # Test
    # itemsList <- list()

    observeEvent(input$datasetLoad, {

      inFile <- input$datasetLoad

      fileDataPath <- inFile$datapath

      # Test
      # fileDataPath <- here("OtherResults", "resultsMock_CPRD.zip")

      # If a single file is loaded

      if (length(fileDataPath) == 1) {
        # If a zip file is loaded
        if (grepl(".zip", fileDataPath, fixed = TRUE)) {
          # Creates a temp dir to allocate files
          csvLocation <- tempdir()
          # Unlinks previous files to the temp dir
          lapply(list.files(path = csvLocation, full.names = TRUE), unlink)
          # Unzips files
          unzip(fileDataPath, exdir = csvLocation)
          # Lists unzipped files
          csvFiles <- list.files(path = csvLocation, pattern = ".csv", full.names = TRUE)
          # Empty items list
          items <- list()
          # Gets the config file that is used to define the type of uploaded file
          configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
          # Lists all datatypes available to compare
          configDataTypes <- unique(configData$name)
          # Iterated through all uploaded files
          for (fileLocation in csvFiles) {
            # Reads the first uploaded file
            resultsData <- read_csv(fileLocation)
            # Get the columns of the first loaded file
            resultsColumns <- names(resultsData)
            # Checks if the columns of the file matches with the config file
            for (val in configDataTypes) {
              # Gets the columns from the config file
              configColumns <- configData %>% filter(name == val)
              configColumns <- configColumns$variables
              # Searches for a match comparing the number of columns
              if (length(configColumns) == length(resultsColumns)) {
                # Then checks if the names of the columns are identical
                if (identical(configColumns, resultsColumns)) {
                  # Transfers the data from the file into the reactiveValues variable with the proper label name
                  uploadedFiles$data[[val]] <- resultsData
                  # A list of all available items
                  items <- append(items, val)
                  }
                }
              }
          }
          # Gets items list
          itemsList$objects[["items"]] <- getItemsList(items)
          } else if (grepl(".csv", fileDataPath, fixed = TRUE)) {
              resultsData <- read_csv(fileDataPath)
              resultsColumns <- names(resultsData)
              configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
              configDataTypes <- unique(configData$name)
              items <- list()
              for (val in configDataTypes) {
                configColumns <- configData %>% filter(name == val)
                configColumns <- configColumns$variables
                if (length(configColumns) == length(resultsColumns)) {
                  if (identical(configColumns, resultsColumns)) {
                    uploadedFiles$data[[val]] <- resultsData
                    items <- append(items, val)
                  }
                }
              }
            itemsList$objects[["items"]] <- getItemsList(items)
          }
      }

      else if (length(fileDataPath) > 1) {
        if (grepl(".zip", fileDataPath[1], fixed = TRUE)) {
          csvFiles <- joinZipFiles(fileDataPath)
          items <- list()
          configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
          configDataTypes <- unique(configData$name)
          for (fileLocation in csvFiles) {
            resultsData <- read_csv(fileLocation)
            resultsColumns <- names(resultsData)
            for (val in configDataTypes) {
              configColumns <- configData %>% filter(name == val)
              configColumns <- configColumns$variables
              if (length(configColumns) == length(resultsColumns)) {
                if (identical(configColumns, resultsColumns)) {
                  uploadedFiles$data[[val]] <- resultsData
                  items <- append(items, val)
                }
              }
            }
          }
          itemsList$objects[["items"]] <- getItemsList(items)
        } else if (grepl(".csv", fileDataPath[1], fixed = TRUE)) {
          csvFiles <- joinZipFiles(fileDataPath)
          items <- list()
          configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
          configDataTypes <- unique(configData$name)
          for (fileLocation in csvFiles) {
            resultsData <- read_csv(fileLocation)
            resultsColumns <- names(resultsData)
            for (val in configDataTypes) {
              configColumns <- configData %>% filter(name == val)
              configColumns <- configColumns$variables
              if (length(configColumns) == length(resultsColumns)) {
                if (identical(configColumns, resultsColumns)) {
                  uploadedFiles$data[[val]] <- resultsData
                  items <- append(items, val)
                }
              }
            }
          }
          itemsList$objects[["items"]] <- getItemsList(items)
        }
        }
      })

    observeEvent(input$resetData, {
      if (!is.null(uploadedFiles)) {
        uploadedFiles$data <- NULL
        itemsList$objects <- NULL
      }

    })

    output$itemSelectionMenu <- renderUI({

      column(tags$b("Item selection"),
             width = 12,
             bucket_list(header = "Select the figures you want in the report",
                         group_name = "bucket_list_group",
                         orientation = "horizontal",
                         add_rank_list(text = "Drag from here",
                                       labels = itemsList$objects$items$title,
                                       input_id = "objectMenu"),
                         add_rank_list(text = "to here",
                                       labels = NULL,
                                       input_id = "objectSelection"
                         )
             )
      )
    })

    # IncidenceCommonData
    incidenceCommonData <- reactive({
      commonData <- uploadedFiles$data$incidence_estimates
      commonData[is.na(commonData)] = 0

      # Database
      if (length(input$databaseIncidence) != 1 || input$databaseIncidence != "All") {
        commonData <- commonData %>%
          filter(database_name %in% c(input$databaseIncidence))
      }

      # Outcome
      commonData <- commonData %>%
        filter(outcome_cohort_id == input$outcomeIncidence)

      # Sex
      if (input$sexIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexIncidence)
      }

      # Age group
      if (input$ageIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_age_group == input$ageIncidence)
      }

      # Start Time
      commonData <- commonData %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidence),
                       as.Date(input$timeToIncidence)))

      # Analysis

      # Interval
      commonData <- commonData %>%
        filter(analysis_interval == input$intervalIncidence)

      # Repeated events
      commonData <- commonData %>%
        filter(analysis_repeated_events == input$repeatedIncidence)
    })

    # 3. Item preview

    # 3.1 Objects list. From the sortable menu.
    menu <- reactive({
      contents <- input$objectSelection
      objectsDataFrame <- data.frame(contents)
      objectsDataFrame
    })

    # Functions available from the configuration file
    menuFun  <- reactive({
      menuFun  <- read.csv(system.file("config/itemsConfig.csv", package = "ReportGenerator"), sep = ";")
      menuFun$arguments <- gsub("incidence_attrition",
                                "incidence_attrition()",
                                menuFun$arguments)

      menuFun$arguments <- gsub("prevalence_attrition",
                                "prevalence_attrition()",
                                menuFun$arguments)

      menuFun$arguments <- gsub("incidence_estimates",
                                "incidenceCommonData()",
                                menuFun$arguments)

      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun
    })

    # 3.2 Preview Table

    # Table of contents to the UI
    output$tableContents <- DT::renderDataTable(createPreviewMenuTable(menu()))

    # Item choice data
    objectChoice  <- reactive({
      req(uploadedFiles)
      req(input$tableContents_cells_selected)

      objectChoiceTitle <- menu()[input$tableContents_cells_selected,]
      objectChoiceTitle
    })

    observe({
      req(objectChoice())

      if (objectChoice() == "Plot - Incidence rate per year") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))

      } else if (objectChoice()== "Plot - Incidence rate per year group by sex") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))

      } else if (objectChoice() == "Plot - Incidence rate per year color by age") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_age_group)))

      } else if (objectChoice() == "Plot - Incidence rate per year facet by database, age group") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_age_group)))
      }
    })

    # Renders item preview depending on the object class
    output$itemPreview <- renderUI({
      req(objectChoice())

      if (grepl("Table", objectChoice())) {
        tableOutput("previewTable")
      } else {
        plotOptions <- menuFun() %>%
          dplyr::filter(title == objectChoice()) %>%
          getItemOptions()
        if (identical(plotOptions, character(0))) {
          plotlyOutput("previewPlot")
        } else {
          names(plotOptions) <- as.character(glue::glue("{toupper(letters)[1:length(plotOptions)]}: {plotOptions}"))
          tagList(selectizeInput("previewPlotOption", "Select plot type", choices = plotOptions, width = "32%"),
                  plotlyOutput("previewPlot"))
        }
      }
    })

    # Objects to be rendered in the UI

    output$previewTable <- renderTable({
      req(objectChoice())

      if (grepl("Table", objectChoice())) {
        object <- eval(parse(text = menuFun() %>%
                               dplyr::filter(title == objectChoice()) %>%
                               dplyr::pull(signature)))

        object
      }
    })

    output$plotFilters <- renderUI({
      req(objectChoice())

      selectionPlotFilter()

    })

    selectionPlotFilter <- reactive({

      if (req(objectChoice()) == "Table - Number of participants") {

      tagList(
        fluidRow(
          column(4,
                 pickerInput(inputId = "databaseIncidence",
                             label = "Database",
                             choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                             selected = "All",
                             multiple = TRUE)
          ),
          column(4,
                 selectInput(inputId = "outcomeIncidence",
                             label = "Outcome",
                             choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
          )
        )
      )

      } else {

        tagList(
          fluidRow(
            column(4,
                   pickerInput(inputId = "databaseIncidence",
                               label = "Database",
                               choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                               selected = "All",
                               multiple = TRUE)
            ),
            column(4,
                   selectInput(inputId = "outcomeIncidence",
                               label = "Outcome",
                               choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "sexIncidence",
                               label = "Sex",
                               choices = c("All", unique(uploadedFiles$data$incidence_estimates$denominator_sex)))
            ),
            column(4,
                   selectInput(inputId = "ageIncidence",
                               label = "Age",
                               choices = c("All", unique(uploadedFiles$data$incidence_estimates$denominator_age_group)))
            ),
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "intervalIncidence",
                               label = "Interval",
                               choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
            ),
            column(4,
                   selectInput(inputId = "repeatedIncidence",
                               label = "Repeated Events",
                               choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "timeFromIncidence",
                               label = "From",
                               choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                               selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
            ),
            column(4,
                   selectInput(inputId = "timeToIncidence",
                               label = "To",
                               choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                               selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
            )
          )
        )

      }

    })

    output$previewPlot<- renderPlotly({
      req(objectChoice())

      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice())
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      if (!identical(itemOptions, character(0))) {
        expression <- expression %>%
          addPreviewItemType(input$previewPlotOption)
      }
      object <- eval(parse(text = expression))

      if (grepl("Plot", objectChoice())) {
        object %>%
          plotly::ggplotly() %>%
          increaseFacetStripSize()
      }
    })

    # 4. Word report generator
    observeEvent(input$generateReport, {
      incidencePrevalenceDocx <- read_docx(path = file.path(system.file("templates/word/darwinTemplate.docx", package = "ReportGenerator")))
      reverseList <- rev(menu()$contents)
      for (i in reverseList) {
        menuFunction <- menuFun() %>%
          dplyr::filter(title == i)
        itemOptions <- menuFunction %>% getItemOptions()
        expression <- menuFunction %>%
          dplyr::pull(signature)
        if (!identical(itemOptions, character(0))) {
          expression <- expression %>%
            addPreviewItemType(input$previewPlotOption)
        }
        object <- eval(parse(text = expression))
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
                         style = "TableOverall",
                         header = FALSE)
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")
        }
      }
      body_add_toc(incidencePrevalenceDocx)
      print(incidencePrevalenceDocx,
            target = here("generatedReport.docx"))
    })
  }
  shinyApp(ui, server)
}
