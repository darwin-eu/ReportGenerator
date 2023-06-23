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
#' @import dplyr rmarkdown here ggplot2 shiny shinydashboard shinyWidgets officer flextable waldo readr yaml
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom IncidencePrevalence plotIncidence plotPrevalence
#' @importFrom utils read.csv tail unzip
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
                       textOutput("testReportData"),
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
          csvFiles <- joinZipFiles(fileDataPath, csvLocation)
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

    dataReport <- reactiveValues()

    prevalenceAttritionCommon <- reactive({
      commonData <- uploadedFiles$data$prevalence_attrition
      commonData[is.na(commonData)] = 0
      commonData <- commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
      dataReport[[objectChoice()]][["prevalence_attrition"]] <- commonData
      dataReport[[objectChoice()]][["prevalence_attrition"]]
    })

    # Data: incidence_attrition

    incidenceAttritionCommon <- reactive({
      commonData <- uploadedFiles$data$incidence_attrition
      commonData[is.na(commonData)] = 0
      commonData <- commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
      dataReport[[objectChoice()]][["incidence_attrition"]] <- commonData
      dataReport[[objectChoice()]][["incidence_attrition"]]
    })

    # Data: incidence_estimates
    incidenceCommonData <- reactive({
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

      if (length(input$sexIncidence) != 1 || input$sexIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_sex %in% input$sexIncidence)
      }

      # Age group

      if (length(input$ageIncidence) != 1 || input$ageIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_age_group %in% input$ageIncidence)
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
    prevalenceCommonData <- reactive({
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

      if (length(input$sexPrevalence) != 1 || input$sexPrevalence != "All") {
        commonData <- commonData %>%
          filter(denominator_sex %in% input$sexPrevalence)
      }

      # Age group

      if (length(input$agePrevalence) != 1 || input$agePrevalence != "All")  {
        commonData <- commonData %>%
          filter(denominator_age_group %in% input$agePrevalence)
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
                                "incidenceAttritionCommon()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("prevalence_attrition",
                                "prevalenceAttritionCommon()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("incidence_estimates",
                                "incidenceCommonData()",
                                menuFun$arguments)
      menuFun$arguments <- gsub("prevalence_estimates",
                                "prevalenceCommonData()",
                                menuFun$arguments)
      menuFun$arguments[menuFun$name == "table1SexAge"] <- "uploadedFiles$data$incidence_estimates"
      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun
    })

    # Item choice data
    objectChoice  <- reactive({
      req(uploadedFiles)
      req(input$tableContents_rows_selected)
      objectChoiceTitle <- objectDataFrame()[input$tableContents_rows_selected,]
      objectChoiceTitle
    })

    # 3.2 Filters UI

    # Filters that go into the UI for figures, tables, etc
    selectionPlotFilter <- reactive({
      req(objectChoice())
      if (objectChoice() == "Table - Number of participants") {
        tableNumParFilters(uploadedFiles)
      } else if (objectChoice() == "Table - Number of participants by sex and age group") {
        tableSexFilters(uploadedFiles)
      } else if (objectChoice() == "Plot - Incidence rate per year") {
        incPlotByYearFilters(uploadedFiles, menuFun(), objectChoice())
      } else if (objectChoice() == "Plot - Incidence rate per year by sex") {
        incPlotSexFilters(uploadedFiles, menuFun(), objectChoice())
      } else if (objectChoice() == "Plot - Incidence rate per year by age") {
        incPlotAgeFilters(uploadedFiles, menuFun(), objectChoice())
      } else if (objectChoice() == "Plot - Prevalence rate per year") {
        prevPlotByYearFilters(uploadedFiles, menuFun(), objectChoice())
      } else if (objectChoice() == "Plot - Prevalence rate per year by sex") {
        prevPlotSexFilters(uploadedFiles, menuFun(), objectChoice())
      } else if (objectChoice() == "Plot - Prevalence rate per year by age") {
        prevPlotAgeFilters(uploadedFiles, menuFun(), objectChoice())
      }
    })

    observeEvent(input$previewPlotOption, {
      if  (input$previewPlotOption == "Facet by database") {
        if (objectChoice() == "Plot - Incidence rate per year by sex") {
        updatePickerInput(session,
                          inputId = "databaseIncidence",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$data$incidence_estimates$database_name))+1)
                          ))
        } else if (objectChoice() == "Plot - Incidence rate per year by age") {
          updatePickerInput(session,
                            inputId = "databaseIncidence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$incidence_estimates$database_name))+1)
                            ))
          }
      } else {
        if (objectChoice() == "Plot - Incidence rate per year by sex") {
        updatePickerInput(session,
                          inputId = "databaseIncidence",
                          label = "Database",
                          choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                          selected = uploadedFiles$data$incidence_estimates$database_name[1],
                          options = list(
                            maxOptions = 1
                            )
                          )
          } else if (objectChoice() == "Plot - Incidence rate per year by age") {
            updatePickerInput(session,
                              inputId = "databaseIncidence",
                              label = "Database",
                              choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                              selected = uploadedFiles$data$incidence_estimates$database_name[1],
                              options = list(
                                maxOptions = 1
                                )
                              )
          }
        }
      })

    observeEvent(input$previewPlotOption, {
      if  (input$previewPlotOption == "Facet by database") {
        if (objectChoice() == "Plot - Prevalence rate per year by sex") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))
        } else if (objectChoice() == "Plot - Prevalence rate per year by age") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                            selected = "All",
                            options = list(
                              maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                            ))
        }
      } else {
        if (objectChoice() == "Plot - Prevalence rate per year by sex") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                            selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            )
          )
        } else if (objectChoice() == "Plot - Prevalence rate per year by age") {
          updatePickerInput(session,
                            inputId = "databasePrevalence",
                            label = "Database",
                            choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                            selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                            options = list(
                              maxOptions = 1
                            )
          )
        }
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
      } else if (grepl("Plot", objectChoice())) {
        plotOutput("previewPlot")
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

    # Data saving functions to print into Word
    observeEvent(input$lockDataIncidence, {
      if(input$lockDataIncidence == TRUE) {
        objectReport <- menuFun() %>%
          dplyr::filter(title == objectChoice()) %>%
          dplyr::pull(arguments)
        objectReport <- gsub(" ", "", objectReport)
        argumentsData <- unlist(strsplit(objectReport, ","))
        for (i in argumentsData) {
          if (grepl("incidence", i)) {
            arguments <- eval(parse(text = i))
            dataReport[[objectChoice()]][["incidence_estimates"]] <- arguments
            dataReport[[objectChoice()]][["plotOption"]] <- input$previewPlotOption
        }
        }
      } else {
        dataReport[[objectChoice()]] <- NULL
      }
    })

    observeEvent(input$lockDataPrevalence, {
      if(input$lockDataPrevalence == TRUE) {
        objectReport <- menuFun() %>%
          dplyr::filter(title == objectChoice()) %>%
          dplyr::pull(arguments)
        objectReport <- gsub(" ", "", objectReport)
        argumentsData <- unlist(strsplit(objectReport, ","))
        for (i in argumentsData) {
          if (grepl("prevalence", i)) {
            arguments <- eval(parse(text = i))
            dataReport[[objectChoice()]][["prevalence_estimates"]] <- arguments
            dataReport[[objectChoice()]][["plotOption"]] <- input$previewPlotOption
          }
        }
      } else {
        dataReport[[objectChoice()]] <- NULL
      }
    })

    output$testReportData <- renderText({
      req(objectChoice())
      if (!is.null(dataReport[[objectChoice()]])) {
        textData <- "Data addet to the report"
      } else {
        textData <- NULL
      }
      textData
      })

    menuSel  <- reactive({
      menuSel  <- read.csv(system.file("config/itemsConfigExternal.csv",
                                       package = "ReportGenerator"), sep = ";")
      menuSel  <- menuSel  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuSel
    })

    # 4. Word report generator
    observeEvent(input$generateReport, {
      incidencePrevalenceDocx <- read_docx(path = system.file("templates",
                                                              "word",
                                                              "darwinTemplate.docx",
                                                              package = "ReportGenerator"))
      reverseList <- rev(objectDataFrame()$contents)
      for (i in reverseList) {
        menuFunction <- menuSel() %>%
          dplyr::filter(title == i)
        itemOptions <- menuFunction %>%
          getItemOptions()
        expression <- menuFunction %>%
          dplyr::pull(signature)
        if (!identical(itemOptions, character(0))) {
          if (grepl("by sex", i)) {
            expression <- expression %>%
              addPreviewItemTypeSex(dataReport[[i]][["plotOption"]])
          } else if (grepl("by age", i)) {
            expression <- expression %>%
              addPreviewItemTypeAge(dataReport[[i]][["plotOption"]])
          } else  {
            expression <- expression %>%
              addPreviewItemType(dataReport[[i]][["plotOption"]])
          }
        }
        object <- eval(parse(text = expression), envir = dataReport[[i]])
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
