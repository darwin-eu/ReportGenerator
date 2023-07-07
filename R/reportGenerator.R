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
                selectInput(inputId = "dataVersion",
                            label = "Please select version",
                            choices = c("IncidencePrevalence v0.4.0 (Latest)",
                                        "IncidencePrevalence v0.2.1",
                                        "IncidencePrevalence v0.1.0"),
                            selected = "IncidencePrevalence v0.4.0 (Latest)"),
                uiOutput("datasetLoadUI"),
                actionButton('resetData', 'Reset data')
        ))
    ),
    dashboardBody(
      tabsetPanel(
        id = "mainPanel",
        tabPanel("Item selection",
        fluidRow(
          box(uiOutput("itemSelectionMenu"),
              tags$br())
        )),
        tabPanel("Item preview",
          fluidRow(
          column(width = 12,
                 fluidRow(
                   uiOutput("navPanelPreview"),
                   downloadButton("generateReport", "Generate Report")
                   )
                 )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {

    # 1. Load data

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

    # Check data
    observeEvent(input$datasetLoad, {
      inFile <- input$datasetLoad
      fileDataPath <- inFile$datapath
      configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
      versionData <- gsub(" \\(Latest\\)", "", input$dataVersion)
      configData <- configData[[versionData]]
      configDataTypes <- names(configData)
      if (length(fileDataPath) == 1) {
        if (grepl(".zip", fileDataPath, fixed = TRUE)) {
          csvLocation <- file.path(tempdir(), "dataLocation")
          csvFiles <- joinZipFiles(fileDataPath, csvLocation)
          uploadedFiles$data <- columnCheck(csvFiles, configData, configDataTypes)
          items <- names(uploadedFiles$data)
          itemsList$objects[["items"]] <- getItemsList(items)
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

    # 2.Assign Data

    dataReport <- reactiveValues()

    # prevalence_attrition

    prevalenceAttritionCommon <- reactive({
      commonData <- uploadedFiles$data$prevalence_attrition
      commonData[is.na(commonData)] = 0
      commonData <- commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
      commonData
    })

    # incidence_attrition

    incidenceAttritionCommon <- reactive({
      commonData <- uploadedFiles$data$incidence_attrition
      commonData[is.na(commonData)] = 0
      commonData <- commonData %>%
        filter(analysis_id %in% c(input$analysisIdTable1))
      commonData
    })

    # 3. Interactive menu

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

    # Item preview

    # Table of contents from objects list selected by the user data frame
    output$navPanelPreview <- renderUI({
      previewPanels <- lapply(input$objectSelection,
                              tabPanelSelection,
                              uploadedFiles = uploadedFiles,
                              menuFun = menuFun())
      do.call(navlistPanel, previewPanels)
    })

    # Retrieves list of functions available and places the correct arguments
    menuFun  <- reactive({
      menuFun  <- read.csv(system.file("config/itemsConfigExternal.csv", package = "ReportGenerator"), sep = ";")
      menuFun$arguments[menuFun$name == "table1SexAge"] <- "uploadedFiles$data$incidence_estimates"
      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun
    })

    # Objects to be rendered in the UI

    # Table 1

    output$previewTable1 <- renderTable({
      objectChoice <- "Table - Number of participants"
      prevalence_attrition <- prevalenceAttritionCommon()
      incidence_attrition <- incidenceAttritionCommon()
      if (input$lockTableNumPar == TRUE) {
        dataReport[[objectChoice]][["prevalence_attrition"]] <- prevalence_attrition
        dataReport[[objectChoice]][["incidence_attrition"]] <- incidence_attrition
      } else {
        dataReport[[objectChoice]][["prevalence_attrition"]] <- NULL
        dataReport[[objectChoice]][["incidence_attrition"]] <- NULL
      }
      object <- eval(parse(text = menuFun() %>%
                             dplyr::filter(title == objectChoice) %>%
                             dplyr::pull(signature)))
      object
    }, colnames = FALSE)

    # Table 2

    output$previewTableSex <- renderTable({
      objectChoice <- "Table - Number of participants by sex and age group"
      # Lock data
      if (input$lockTableSex == TRUE) {
        dataReport[[objectChoice]][["incidence_estimates"]] <- uploadedFiles$data$incidence_estimates
      } else {
        dataReport[[objectChoice]][["incidence_estimates"]] <- NULL
      }
      # Preview object
      object <- eval(parse(text = menuFun() %>%
                             dplyr::filter(title == objectChoice) %>%
                             dplyr::pull(signature)))
      object
    }, colnames = FALSE)

    # Figure 1

    output$previewFigure1 <- renderPlot({
      objectChoice <- "Plot - Incidence rate per year"
      incidence_estimates <- uploadedFiles$data$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Database
      if (length(input$databaseIncidenceYear) != 1 || input$databaseIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(database_name %in% c(input$databaseIncidenceYear))
      }
      # Outcome
      if (length(input$outcomeIncidenceYear) != 1 || input$outcomeIncidenceYear != "All") {
      incidence_estimates <- incidence_estimates %>%
        filter(outcome_cohort_name == input$outcomeIncidenceYear)
      }
      # Sex
      if (length(input$sexIncidenceYear) != 1 || input$sexIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_sex %in% input$sexIncidenceYear)
      }
      # Age group
      if (length(input$ageIncidenceYear) != 1 || input$ageIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_age_group %in% input$ageIncidenceYear)
      }
      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidenceYear),
                       as.Date(input$timeToIncidenceYear)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidenceYear)
      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidenceYear)
      # Lock data
      if (input$lockDataIncidenceYear == TRUE) {
        dataReport[[objectChoice]][["incidence_estimates"]] <- incidence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetIncidenceYear
      } else {
        dataReport[[objectChoice]][["incidence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      # Preview object
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
          expression <- expression %>%
            addPreviewItemType(input$facetIncidenceYear)
      object <- eval(parse(text = expression))
      object
      })

    # Figure 2

    output$previewFigure2 <- renderPlot({
      objectChoice <- "Plot - Incidence rate per year by sex"
      incidence_estimates <- uploadedFiles$data$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Database
      if (length(input$databaseIncidenceSex) != 1 || input$databaseIncidenceSex != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(database_name %in% c(input$databaseIncidenceSex))
      }
      # Outcome
      if (length(input$outcomeIncidenceSex) != 1 || input$outcomeIncidenceSex != "All") {
      incidence_estimates <- incidence_estimates %>%
        filter(outcome_cohort_name == input$outcomeIncidenceSex)
      }
      # Sex
      if (length(input$sexIncidenceSex) != 1 || input$sexIncidenceSex != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_sex %in% input$sexIncidenceSex)
      }
      # Age group
      if (length(input$ageIncidenceSex) != 1 || input$ageIncidenceSex != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_age_group %in% input$ageIncidenceSex)
      }

      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidenceSex),
                       as.Date(input$timeToIncidenceSex)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidenceSex)

      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidenceSex)
      # Lock data
      if (input$lockDataIncidenceSex == TRUE) {
        dataReport[[objectChoice]][["incidence_estimates"]] <- incidence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetIncidenceSex
      } else {
        dataReport[[objectChoice]][["incidence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      expression <- expression %>%
        addPreviewItemTypeSex(input$facetIncidenceSex)
      object <- eval(parse(text = expression))
      object
    })

    # Figure 3

    output$previewFigure3 <- renderPlot({
      objectChoice <- "Plot - Incidence rate per year by age"
      incidence_estimates <- uploadedFiles$data$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Database
      if (length(input$databaseIncidenceAge) != 1 || input$databaseIncidenceAge != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(database_name %in% c(input$databaseIncidenceAge))
      }
      # Outcome
      if (length(input$outcomeIncidenceAge) != 1 || input$outcomeIncidenceAge != "All") {
      incidence_estimates <- incidence_estimates %>%
        filter(outcome_cohort_name %in% input$outcomeIncidenceAge)
      }
      # Sex
      if (length(input$sexIncidenceAge) != 1 || input$sexIncidenceAge != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_sex %in% input$sexIncidenceAge)
      }
      # Age group
      if (length(input$ageIncidenceAge) != 1 || input$ageIncidenceAge != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_age_group %in% input$ageIncidenceAge)
      }
      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidenceAge),
                       as.Date(input$timeToIncidenceAge)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidenceAge)
      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidenceAge)
      # Lock data
      if (input$lockDataIncidenceAge == TRUE) {
        dataReport[[objectChoice]][["incidence_estimates"]] <- incidence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetIncidenceAge
      } else {
        dataReport[[objectChoice]][["incidence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      expression <- expression %>%
        addPreviewItemTypeAge(input$facetIncidenceAge)
      object <- eval(parse(text = expression))
      object
    })

    # Update according to facet incidence

    observeEvent(input$facetIncidenceSex, {
      if  (input$facetIncidenceSex == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databaseIncidenceSex",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$data$incidence_estimates$database_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomeIncidenceSex",
                          label = "Outcome",
                          choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$data$incidence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))

      } else {
        updatePickerInput(session,
                          inputId = "databaseIncidenceSex",
                          label = "Database",
                          choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                          selected = uploadedFiles$data$incidence_estimates$database_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomeIncidenceSex",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$data$incidence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    observeEvent(input$facetIncidenceAge, {
      if  (input$facetIncidenceAge == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databaseIncidenceAge",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$data$incidence_estimates$database_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomeIncidenceAge",
                          label = "Outcome",
                          choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$data$incidence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))
      } else {

        updatePickerInput(session,
                          inputId = "databaseIncidenceAge",
                          label = "Database",
                          choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                          selected = uploadedFiles$data$incidence_estimates$database_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomeIncidenceAge",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$data$incidence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    # Figure 4: Prevalence rate per year

    output$previewFigure4 <- renderPlot({
      objectChoice <- "Plot - Prevalence rate per year"
      prevalence_estimates <- uploadedFiles$data$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceYear) != 1 || input$databasePrevalenceYear != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(database_name %in% c(input$databasePrevalenceYear))
      }

      # Outcome
      if (length(input$outcomePrevalenceYear) != 1 || input$outcomePrevalenceYear != "All") {
      prevalence_estimates <- prevalence_estimates %>%
        filter(outcome_cohort_name %in% input$outcomePrevalenceYear)
      }
      # Sex

      if (length(input$sexPrevalenceYear) != 1 || input$sexPrevalenceYear != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_sex %in% input$sexPrevalenceYear)
      }

      # Age group

      if (length(input$agePrevalenceYear) != 1 || input$agePrevalenceYear != "All")  {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_age_group %in% input$agePrevalenceYear)
      }

      # Start Time
      prevalence_estimates <- prevalence_estimates %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalenceYear),
                       as.Date(input$timeToPrevalenceYear)))

      # Interval
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_interval == input$intervalPrevalenceYear)

      # Repeated events
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_type == input$typePrevalenceYear)
      # Lock data
      if (input$lockDataPrevalenceYear == TRUE) {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- prevalence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetPrevalenceYear
      } else {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      # Preview object
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      expression <- expression %>%
        addPreviewItemType(input$facetPrevalenceYear)
      object <- eval(parse(text = expression))
      object
    })

    # Figure 5: Prevalence rate per year by sex

    output$previewFigure5 <- renderPlot({
      objectChoice <- "Plot - Prevalence rate per year by sex"
      prevalence_estimates <- uploadedFiles$data$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceSex) != 1 || input$databasePrevalenceSex != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(database_name %in% c(input$databasePrevalenceSex))
      }

      # Outcome
      if (length(input$outcomePrevalenceSex) != 1 || input$outcomePrevalenceSex != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(outcome_cohort_name %in% input$outcomePrevalenceSex)
      }
      # Sex

      if (length(input$sexPrevalenceSex) != 1 || input$sexPrevalenceSex != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_sex %in% input$sexPrevalenceSex)
      }

      # Age group

      if (length(input$agePrevalenceSex) != 1 || input$agePrevalenceSex != "All")  {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_age_group %in% input$agePrevalenceSex)
      }

      # Start Time
      prevalence_estimates <- prevalence_estimates %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalenceSex),
                       as.Date(input$timeToPrevalenceSex)))

      # Interval
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_interval == input$intervalPrevalenceSex)

      # Repeated events
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_type == input$typePrevalenceSex)
      # Lock data
      if (input$lockDataPrevalenceSex == TRUE) {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- prevalence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetPrevalenceSex
      } else {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      # Preview object
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      expression <- expression %>%
        addPreviewItemTypeSex(input$facetPrevalenceSex)
      object <- eval(parse(text = expression))
      object
    })

    # Figure 6: Prevalence rate per year by age

    output$previewFigure6 <- renderPlot({
      objectChoice <- "Plot - Prevalence rate per year by age"
      prevalence_estimates <- uploadedFiles$data$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceAge) != 1 || input$databasePrevalenceAge != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(database_name %in% c(input$databasePrevalenceAge))
      }

      # Outcome
      if (length(input$outcomePrevalenceAge) != 1 || input$outcomePrevalenceAge != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(outcome_cohort_name %in% input$outcomePrevalenceAge)
      }
      # Sex

      if (length(input$sexPrevalenceAge) != 1 || input$sexPrevalenceAge != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_sex %in% input$sexPrevalenceAge)
      }

      # Age group

      if (length(input$agePrevalenceAge) != 1 || input$agePrevalenceAge != "All")  {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_age_group %in% input$agePrevalenceAge)
      }

      # Start Time
      prevalence_estimates <- prevalence_estimates %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalenceAge),
                       as.Date(input$timeToPrevalenceAge)))

      # Interval
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_interval == input$intervalPrevalenceAge)

      # Repeated events
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_type == input$typePrevalenceAge)
      # Lock data
      if (input$lockDataPrevalenceSex == TRUE) {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- prevalence_estimates
        dataReport[[objectChoice]][["plotOption"]] <- input$facetPrevalenceAge
      } else {
        dataReport[[objectChoice]][["prevalence_estimates"]] <- NULL
        dataReport[[objectChoice]][["plotOption"]] <- NULL
      }
      # Preview object
      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice)
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      expression <- expression %>%
        addPreviewItemTypeAge(input$facetPrevalenceAge)
      object <- eval(parse(text = expression))
      object
    })

    # Update according to facet prevalence

    observeEvent(input$facetPrevalenceSex, {
      if  (input$facetPrevalenceSex == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databasePrevalenceSex",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomePrevalenceSex",
                          label = "Outcome",
                          choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$data$prevalence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))

      } else {
        updatePickerInput(session,
                          inputId = "databasePrevalenceSex",
                          label = "Database",
                          choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                          selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomePrevalenceSex",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    observeEvent(input$facetPrevalenceAge, {
      if  (input$facetPrevalenceAge == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databasePrevalenceAge",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$data$prevalence_estimates$database_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomePrevalenceAge",
                          label = "Outcome",
                          choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$data$prevalence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))
      } else {

        updatePickerInput(session,
                          inputId = "databasePrevalenceAge",
                          label = "Database",
                          choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                          selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomePrevalenceAge",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    menuSel  <- reactive({
      menuSel  <- read.csv(system.file("config/itemsConfigExternal.csv", package = "ReportGenerator"), sep = ";")
      menuSel  <- menuSel  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuSel
    })

    # Word report generator
    output$generateReport <- downloadHandler(
      filename = function() {
        paste0("generatedReport.docx")
      },
      content = function(file) {
      incidencePrevalenceDocx <- read_docx(path = system.file("templates", "word", "darwinTemplate.docx", package = "ReportGenerator"))
      reverseList <- rev(input$objectSelection)
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
            target = file)
    }

    )

  }
  shinyApp(ui, server)
}
