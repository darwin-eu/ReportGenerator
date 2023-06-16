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
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer flextable waldo readr yaml
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom IncidencePrevalence plotIncidence plotPrevalence
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
                uiOutput("datasetLoadUI"),
                actionButton('resetData', 'Reset data')
        ))
    ),
    dashboardBody(
      tabsetPanel(
        id = "mainPanel",
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
                       tableOutput("testReportData"),
                       uiOutput("itemPreview"),
                       width = 8)
                   )
                 )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # Load data

    # 1. Load files
    output$datasetLoadUI <- renderUI({
      fileInput("datasetLoad",
                "Upload your results",
                accept = c(".zip", ".csv"),
                multiple = TRUE,
                placeholder = "ZIP or CSV file(s)")
    })
    # ReactiveValues
    uploadedFiles <- reactiveValues(data = NULL)
    itemsList <- reactiveValues(objects = NULL)
    # After loading data
    observeEvent(input$datasetLoad, {
      # Get data path from file input
      inFile <- input$datasetLoad
      fileDataPath <- inFile$datapath
      # Retrieve the config file that is used to define the type of uploaded file
      configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
      # Lists all datatypes available to compare
      configDataTypes <- unique(configData$name)
      # Checks if single or multiple files
      if (length(fileDataPath) == 1) {
        # If a zip file is loaded, unzip
        if (grepl(".zip", fileDataPath, fixed = TRUE)) {
          csvLocation <- file.path(tempdir(), "dataLocation")
          dir.create(csvLocation)
          csvFiles <- list.files(path = csvLocation,
                                 pattern = ".csv",
                                 full.names = TRUE,
                                 recursive = TRUE)
          # Check columns and items
          # Add data to reactiveValues
          uploadedFiles$data <- columnCheck(csvFiles, configData, configDataTypes)
          items <- names(uploadedFiles$data)
          itemsList$objects[["items"]] <- getItemsList(items)
          # Unlink tempdir
          unlink(csvLocation, recursive = TRUE)
          } else if (grepl(".csv", fileDataPath, fixed = TRUE)) {
              uploadedFiles$data <- columnCheck(csvFiles = fileDataPath, configData, configDataTypes)
              items <- names(uploadedFiles$data)
              itemsList$objects[["items"]] <- getItemsList(items)
          }
      }
      else if (length(fileDataPath) > 1) {
        if (grepl(".zip", fileDataPath[1], fixed = TRUE)) {
          csvLocation <- file.path(tempdir(), "dataLocation")
          csvFiles <- joinZipFiles(fileDataPath, csvLocation)
          uploadedFiles$data <- columnCheck(csvFiles, configData, configDataTypes)
          items <- names(uploadedFiles$data)
          itemsList$objects[["items"]] <- getItemsList(items)
          unlink(csvLocation, recursive = TRUE)
        } else if (grepl(".csv", fileDataPath[1], fixed = TRUE)) {
          uploadedFiles$data <- columnCheck(csvFiles = fileDataPath, configData, configDataTypes)
          items <- names(uploadedFiles$data)
          itemsList$objects[["items"]] <- getItemsList(items)
        }
        }
      })
    # Reset and back to initial tab
    observeEvent(input$resetData, {
      if (!is.null(uploadedFiles)) {
        uploadedFiles$data <- NULL
        itemsList$objects <- NULL
        updateTabsetPanel(session, "mainPanel",
                          selected = "Item selection")
        output$datasetLoadUI <- renderUI({
          fileInput("datasetLoad",
                    "Upload your results",
                    accept = c(".zip", ".csv"),
                    multiple = TRUE,
                    placeholder = "ZIP or CSV file(s)")
        })
      }
    })

    # Data: prevalence_attrition

    prevalence_attrition <- reactive({
      commonData <- uploadedFiles$data$prevalence_attrition
      commonData[is.na(commonData)] = 0
      commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
    })

    # Data: incidence_attrition

    incidence_attrition <- reactive({
      commonData <- uploadedFiles$data$incidence_attrition
      commonData[is.na(commonData)] = 0
      commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
    })

    # Data: incidence_estimates
    incidence_estimates <- reactive({
      commonData <- uploadedFiles$data$incidence_estimates
      class(commonData) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
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

    # Data: prevalence_estimates
    prevalence_estimates <- reactive({
      commonData <- uploadedFiles$data$prevalence_estimates
      class(commonData) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      commonData[is.na(commonData)] = 0

      # Database
      if (length(input$databasePrevalence) != 1 || input$databasePrevalence != "All") {
        commonData <- commonData %>%
          filter(database_name %in% c(input$databasePrevalence))
      }

      # Outcome
      commonData <- commonData %>%
        filter(outcome_cohort_name == input$outcomePrevalence)

      # Sex
      if (input$sexPrevalence != "All") {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexPrevalence)
      }

      # Age group
      if (input$agePrevalence != "All") {
        commonData <- commonData %>%
          filter(denominator_age_group == input$agePrevalence)
      }

      # Start Time
      commonData <- commonData %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalence),
                       as.Date(input$timeToPrevalence)))

      # Analysis

      # Interval
      commonData <- commonData %>%
        filter(analysis_interval == input$intervalPrevalence)

      # Repeated events
      commonData <- commonData %>%
        filter(analysis_type == input$typePrevalence)
    })

    # 2. Render interactive sortable menu
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

    # 3. Item preview

    # Objects list selected by the user in the sortable menu into a dataframe

    objectDataFrame <- reactive({
      contents <- input$objectSelection
      objectsDataFrame <- data.frame(contents)
      objectsDataFrame
    })

    # Table of contents from objects list selected by the user data frame
    output$tableContents <- DT::renderDataTable(createPreviewMenuTable(objectDataFrame()))

    # Retrieves list of functions available and places the correct arguments
    menuFun  <- reactive({
      menuFun  <- read.csv(system.file("config/itemsConfigExternal.csv", package = "ReportGenerator"), sep = ";")
      menuFun$arguments <- gsub("incidence_attrition",
                                "incidence_attrition()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("prevalence_attrition",
                                "prevalence_attrition()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("incidence_estimates",
                                "incidence_estimates()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("prevalence_estimates",
                                "prevalence_estimates()",
                                menuFun$arguments)
      menuFun$arguments[menuFun$name == "table1SexAge"] <- "uploadedFiles$data$incidence_estimates"
      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun
    })

    # Item choice data
    objectChoice  <- reactive({
      req(uploadedFiles)
      req(input$tableContents_cells_selected)
      objectChoiceTitle <- objectDataFrame()[input$tableContents_cells_selected,]
      objectChoiceTitle
    })

    # 3.2 Filters UI

    # Filters that go into the UI for figures, tables, etc
    selectionPlotFilter <- reactive({
      req(objectChoice())
      if (objectChoice() == "Table - Number of participants") {
        tagList(
          fluidRow(
            column(4,
                   selectInput(inputId = "analysisIdTable1",
                               label = "Analysis ID",
                               choices = unique(uploadedFiles$data$incidence_attrition$analysis_id))
            ),
            column(8,
                   textAreaInput("captionTable1", "Caption", table1aAutText(uploadedFiles$data$incidence_attrition, uploadedFiles$data$prevalence_attrition), height = "130px")
            )
          )
        )
      } else if (objectChoice() == "Table - Number of participants by sex and age group") {
        tagList(
          fluidRow(
            column(8,
                   textAreaInput("captionTable1", "Caption", table1aAutText(uploadedFiles$data$incidence_attrition, uploadedFiles$data$prevalence_attrition), height = "130px")
            )
          )
        )
      } else if (grepl("Incidence", objectChoice())) {

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
      } else if (grepl("Prevalence", objectChoice())) {
        tagList(
          fluidRow(
            column(4,
                   pickerInput(inputId = "databasePrevalence",
                               label = "Database",
                               choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                               selected = "All",
                               multiple = TRUE)
            ),
            column(4,
                   selectInput(inputId = "outcomePrevalence",
                               label = "Outcome",
                               choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "sexPrevalence",
                               label = "Sex",
                               choices = c("All", unique(uploadedFiles$data$prevalence_estimates$denominator_sex)))
            ),
            column(4,
                   selectInput(inputId = "agePrevalence",
                               label = "Age",
                               choices = c("All", unique(uploadedFiles$data$prevalence_estimates$denominator_age_group)))
            ),
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "intervalPrevalence",
                               label = "Interval",
                               choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
            ),
            column(4,
                   selectInput(inputId = "typePrevalence",
                               label = "Analysis type",
                               choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "timeFromPrevalence",
                               label = "From",
                               choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                               selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
            ),
            column(4,
                   selectInput(inputId = "timeToPrevalence",
                               label = "To",
                               choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                               selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
            )
          )
        )
      }
    })

    # Updates to the filters according to the object
    observe({
      req(objectChoice())
      if (objectChoice() == "Plot - Incidence rate per year") {
        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))
      } else if (objectChoice() == "Plot - Incidence rate per year by sex") {

        if  (input$previewPlotOption == "Facet by outcome") {
          updatePickerInput(session,
                            inputId = "databaseIncidence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                            selected = uploadedFiles$data$incidence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            ))
        } else {

          updatePickerInput(session,
                            inputId = "databaseIncidence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))
        }
        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_sex)))
        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))

      } else if (objectChoice() == "Plot - Incidence rate per year by age") {

        if  (input$previewPlotOption == "Facet by outcome") {
          updatePickerInput(session,
                            inputId = "databaseIncidence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                            selected = uploadedFiles$data$incidence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            ))

        } else {

          updatePickerInput(session,
                            inputId = "databaseIncidence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))

        }
        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(uploadedFiles$data$incidence_estimates$denominator_age_group)))

      }
      else if (objectChoice() == "Plot - Prevalence rate per year") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))

      } else if (objectChoice() == "Plot - Prevalence rate per year by sex") {

        if  (input$previewPlotOption == "Facet by outcome") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                            selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            ))

        } else {

          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))

        }

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(uploadedFiles$data$prevalence_estimates$denominator_sex)))
        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))

      } else if (objectChoice() == "Plot - Prevalence rate per year by age") {

        if  (input$previewPlotOption == "Facet by outcome") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                            selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            ))

        } else {

          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))

        }

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))
      }
    })

    # Filters to the UI

    output$plotFilters <- renderUI({
      req(objectChoice())
      selectionPlotFilter()

    })

    # 3.3 Renders item preview depending on the object class
    output$itemPreview <- renderUI({
      req(objectChoice())
      if (grepl("Table", objectChoice())) {
        tableOutput("previewTable")
      } else {
        plotOptions <- menuFun() %>%
          dplyr::filter(title == objectChoice()) %>%
          getItemOptions()
        if (identical(plotOptions, character(0))) {
          plotOutput("previewPlot")
        } else {
          names(plotOptions) <- as.character(glue::glue("{toupper(letters)[1:length(plotOptions)]}: {plotOptions}"))
          tagList(selectizeInput("previewPlotOption", "Select plot type", choices = plotOptions, width = "32%"),
                  plotOutput("previewPlot"))
        }
      }
    })

    # Objects to be rendered in the UI

    # Table
    output$previewTable <- renderTable({
      req(objectChoice())

      if (grepl("Table", objectChoice())) {
        object <- eval(parse(text = menuFun() %>%
                               dplyr::filter(title == objectChoice()) %>%
                               dplyr::pull(signature)))
        object
      }
    }, colnames = FALSE)


    # Figures
    output$previewPlot <- renderPlot({
      req(objectChoice())
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice())
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      if (!identical(itemOptions, character(0))) {
        if (grepl("by sex", objectChoice())) {
          expression <- expression %>%
            addPreviewItemTypeSex(input$previewPlotOption)
        } else if (grepl("by age", objectChoice())) {
          expression <- expression %>%
            addPreviewItemTypeAge(input$previewPlotOption)
        } else  {
          expression <- expression %>%
            addPreviewItemType(input$previewPlotOption)
        }
      }
      object <- eval(parse(text = expression))
      if (grepl("Plot", objectChoice())) {
        object
      }
    })

    # Reactive values to hold data for the Word Report

    # dataReport$data <- list()
    # #
    # objectChoice <- "Table - Number of participants"
    # #
    # objectReport <- menuFun %>%
    #   dplyr::filter(title == objectChoice) %>%
    #   dplyr::pull(arguments)
    #
    # objectReport <- gsub(" ", "", objectReport)
    #
    # argumentsData <- unlist(strsplit(objectReport, ","))
    #
    # # argumentsData <- c("incidence_attrition", "prevalence_attrition")
    #
    # argumentsData <- c("incidence_attrition", "prevalence_attrition")
    #
    # for (i in argumentsData) {
    #   # if (grepl("_estimates", i)) {
    #     dataReport$data[[objectChoice]][[i]] <- eval(parse(text = i))
    #   # }
    # }

    # for (i in argumentsData) {
    #   if (grepl("\\(\\)", i)) {
    #   functionReport[[objectChoice]][[i]] <- eval(parse(text = i))
    #   }
    # }

    dataReport <- reactiveValues(data = NULL)

    ###
    # objectChoice <- "Table - Number of participants"
    # objectReport <- menuFun %>%
    #   dplyr::filter(title == objectChoice) %>%
    #   dplyr::pull(arguments)
    # objectReport <- gsub(" ", "", objectReport)
    # argumentsData <- unlist(strsplit(objectReport, ","))
    #
    # for (i in argumentsData) {
    #   # if (grepl("\\(\\)", i)) {
    #     # arguments <- eval(parse(text = i))
    #     dataReport$data[[objectChoice()]][[i]] <- eval(parse(text = i))
    #   # }
    # }


    ###


    observe({
      req(objectChoice())
      objectReport <- menuFun() %>%
        dplyr::filter(title == objectChoice()) %>%
        dplyr::pull(arguments)
      objectReport <- gsub(" ", "", objectReport)
      argumentsData <- unlist(strsplit(objectReport, ","))
      for (i in argumentsData) {
        # if (grepl("\\(\\)", i)) {
          arguments <- eval(parse(text = i))
          dataReport$data[[objectChoice()]][[i]] <- eval(parse(text = i))
        # }
      }
    })


    output$testReportData <- renderTable({
      req(objectChoice())
      # uploadedFiles$data$incidence_attrition
      dataReport$data[[objectChoice()]][[argumentsData[2]]]
      })

    # output$testReportData <- renderTable({
    #   req(objectChoice())
    #   testReportData <- paste0("dataReport$`", objectChoice(), "`$incidence_attrition")
    #   testReportData <- eval(parse(text = testReportData))
    #   testReportData
    # })



    # Retrieves list of functions with correct selected data arguments
    menuFunSel  <- reactive({
      # req(objectChoice())
      # objectChoice <- objectChoice()

      menuFun  <- read.csv(system.file("config", "itemsConfigExternal.csv", package = "ReportGenerator"), sep = ";")
      # menuFun$arguments <- gsub("incidence_attrition",
      #                           paste0("dataReport$data$`", objectChoice, "`$incidence_attrition"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("prevalence_attrition",
      #                           paste0("dataReport$data$`", objectChoice, "`$prevalence_attrition"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("incidence_estimates",
      #                           paste0("dataReport$data$`", objectChoice, "`$incidence_estimates"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("prevalence_estimates",
      #                           paste0("dataReport$data$`", objectChoice, "`$prevalence_estimates"),
      #                           menuFun$arguments)
      # menuFun$arguments[menuFun$name == "table1SexAge"] <- "uploadedFiles$data$incidence_estimates"
      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun

      # menuFun  <- read.csv(system.file("config/itemsConfigExternal.csv", package = "ReportGenerator"), sep = ";")
      # menuFun$arguments <- gsub("incidence_attrition",
      #                           paste0("dataReport$data$`", objectChoice(), "`$incidence_attrition"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("prevalence_attrition",
      #                           paste0("dataReport$data$`", objectChoice(), "`$prevalence_attrition"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("incidence_estimates",
      #                           paste0("dataReport$data$`", objectChoice(), "`$incidence_estimates"),
      #                           menuFun$arguments)
      # menuFun$arguments <- gsub("prevalence_estimates",
      #                           paste0("dataReport$data$`", objectChoice(), "`$prevalence_estimates"),
      #                           menuFun$arguments)
      # menuFun$arguments[menuFun$name == "table1SexAge"] <- "uploadedFiles$data$incidence_estimates"
      # menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      # menuFun
    })

    # 4. Word report generator
    observeEvent(input$generateReport, {
      incidencePrevalenceDocx <- read_docx(path = file.path(system.file("templates/word/darwinTemplate.docx", package = "ReportGenerator")))
      reverseList <- rev(objectDataFrame()$contents)
      for (i in reverseList) {
        # i <- "Table - Number of participants"
        incidence_attrition <- eval(parse(text = paste0("dataReport$data$`", i, "`$incidence_attrition")))
        prevalence_attrition <- eval(parse(text = paste0("dataReport$data$`", i, "`$prevalence_attrition")))
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
          body_end_section_landscape(incidencePrevalenceDocx)
          body_add_table(incidencePrevalenceDocx,
                         value = object,
                         style = "TableOverall",
                         header = FALSE)
          body_add_par(incidencePrevalenceDocx, " ")
          body_add(incidencePrevalenceDocx,
                   value = input$captionTable1)
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")
          body_end_section_portrait(incidencePrevalenceDocx)
        }
      }
      body_add_toc(incidencePrevalenceDocx)
      print(incidencePrevalenceDocx,
            target = here("generatedReport.docx"))
    })
  }
  shinyApp(ui, server)
}
