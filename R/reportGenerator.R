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
#' @import dplyr shiny shinydashboard shinyWidgets shinycssloaders officer flextable waldo readr yaml googleVis TreatmentPatterns PatientProfiles
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom IncidencePrevalence plotIncidence plotPrevalence
#' @importFrom utils read.csv tail unzip
#' @importFrom gtools mixedsort
#' @importFrom ggplot2 ggsave
#' @importFrom gto body_add_gt
#' @importFrom here here
#' @importFrom TreatmentPatterns createSankeyDiagram
#' @importFrom DT renderDT DTOutput
#' @export
reportGenerator <- function() {

  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)

  ui <- dashboardPage(
    dashboardHeader(title = "ReportGenerator"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("IncidencePrevalence",
                 datasetLoadUI("IncidencePrevalence")
        ),
        menuItem("TreatmentPatterns",
                 datasetLoadUI("TreatmentPatterns"),
                 tags$br()
        ),
        menuItem("PatientProfiles",
                 datasetLoadUI("PatientProfiles"),
                 tags$br()
        ),
        menuItem("CohortSurival",
                 datasetLoadUI("CohortSurvival"),
                 tags$br()
        ),
        tags$br(),
        actionButton('resetData', 'Reset data'),
        tags$br()
      )
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
                          h2("1. Choose objects"),
                          uiOutput("navPanelPreview")
                   )
                 ),
                 tags$br(),
                 fluidRow(
                   column(width = 4,
                          h2("2. Objects to print"),
                          # DTOutput("dataReportMenu"),
                          verbatimTextOutput("dataReportMenu"),
                          downloadButton("generateReport", "Generate Report")
                          )
                 )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # 1. Load data

    # IncidencePrevalence Module
    datasetLoadServer("IncidencePrevalence")
    # TreatmentPatterns Module
    datasetLoadServer("TreatmentPatterns")
    # PatientProfiles Module
    datasetLoadServer("PatientProfiles")
    # CohortSurvival Module
    datasetLoadServer("CohortSurvival")

    # ReactiveValues
    uploadedFiles <- reactiveValues(dataIP = NULL,
                                    dataTP = NULL,
                                    dataPP = NULL)
    itemsList <- reactiveValues(objects = NULL)

    # Check input data
    # IncidencePrevalence
    observeEvent(input$datasetLoad, {
      # Read  file paths
      inFile <- input$datasetLoad
      fileDataPath <- inFile$datapath
      fileName <- inFile$name
      # Temp directory to unzip files
      csvLocation <- file.path(tempdir(), "dataLocation")
      dir.create(csvLocation)
      # Joins one or several zips from different data partners into the reactive value
      dataVersion <- input$dataVersion
      uploadedFiles$dataIP <- joinDatabase(fileDataPath = fileDataPath,
                                           fileName = fileName,
                                           package = "IncidencePrevalence",
                                           versionData = dataVersion,
                                           csvLocation = csvLocation)
      if (length(uploadedFiles$dataIP) == 0) {
        show_alert(title = "Data mismatch",
                   text = "Not a valid IncidencePrevalence file or check version")
      }
      # Get list of items to show in toggle menu
      itemsIP <- names(uploadedFiles$dataIP)
      itemsIPList <- getItemsList(itemsIP)
      # Items list into reactive value to show in toggle menu
      itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], itemsIPList)
      # itemsList$objects[["items"]] <- getItemsList(items)
      unlink(csvLocation, recursive = TRUE)
    })

    # TreatmentPatterns
    observeEvent(input$datasetLoadTP, {
      # Read  file paths
      inFile <- input$datasetLoadTP
      fileDataPath <- inFile$datapath
      fileName <- inFile$name
      # Temp directory to unzip files
      csvLocation <- file.path(tempdir(), "dataLocation")
      dir.create(csvLocation)
      # Joins one or several zips from different data partners into the reactive value
      versionData <- input$dataVersionTP
      uploadedFiles$dataTP <- joinDatabase(fileDataPath = fileDataPath,
                                           fileName = fileName,
                                           package = "TreatmentPatterns",
                                           versionData = versionData,
                                           csvLocation = csvLocation)
      if (length(uploadedFiles$dataTP) == 0) {
        show_alert(title = "Data mismatch",
                   text = "Not a valid TreatmentPathways file or check version")
      }
      # Get list of items to show in toggle menu
      itemsTP <- names(uploadedFiles$dataTP)
      itemsTPList <- getItemsList(itemsTP)
      # Items list into reactive value to show in toggle menu
      itemsList$objects[["items"]] <-  c(itemsList$objects[["items"]], itemsTPList)
      # itemsList$objects[["items"]] <- getItemsList(itemsTP)
      unlink(csvLocation, recursive = TRUE)
    })

    # PatientProfiles
    observeEvent(input$datasetLoadPP, {
      # Read  file paths
      inFile <- input$datasetLoadPP
      fileDataPath <- inFile$datapath
      fileName <- inFile$name
      # Temp directory to unzip files
      csvLocation <- file.path(tempdir(), "dataLocation")
      dir.create(csvLocation)
      # Joins one or several zips from different data partners into the reactive value
      versionData <- input$dataVersionPP
      uploadedFiles$dataPP <- joinDatabase(fileDataPath = fileDataPath,
                                           fileName = fileName,
                                           package = "PatientProfiles",
                                           versionData = versionData,
                                           csvLocation = csvLocation)
      if (length(uploadedFiles$dataPP) == 0) {
        show_alert(title = "Data mismatch",
                   text = "Not a valid PatientProfiles file or check version")
      }
      # Get list of items to show in toggle menu
      itemsPP <- names(uploadedFiles$dataPP)
      itemsPPList <- getItemsList(itemsPP)
      # Items list into reactive value to show in toggle menu
      itemsList$objects[["items"]] <-  c(itemsList$objects[["items"]], itemsPPList)
      # itemsList$objects[["items"]] <- getItemsList(itemsPP)
      unlink(csvLocation, recursive = TRUE)
    })

    # Reset and back to initial tab
    observeEvent(input$resetData, {
      # if (!is.null(uploadedFiles)) {
      itemsList$objects <- NULL
      uploadedFiles <- reactiveValues(dataIP = NULL, dataTP = NULL)
      dataReport <- reactiveValues()
      updateTabsetPanel(session, "mainPanel",
                        selected = "Item selection")
      datasetLoadServer("IncidencePrevalence")
      # TreatmentPatterns Module
      datasetLoadServer("TreatmentPatterns")
      # PatientProfiles Module
      datasetLoadServer("PatientProfiles")
      # CohortSurvival Module
      datasetLoadServer("CohortSurvival")
      # }
    })

    # 2.Assign Data

    dataReport <- reactiveValues()

    # prevalence_attrition

    prevalenceAttritionCommon <- reactive({
      if (!is.null(uploadedFiles$dataIP$prevalence_attrition)) {
        commonData <- uploadedFiles$dataIP$prevalence_attrition
        if (class(commonData$excluded_records) == "character") {
          commonData$excluded_records <- as.numeric(commonData$excluded_records)
        }
        if (class(commonData$excluded_subjects) == "character") {
          commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
        }
        commonData[is.na(commonData)] = 0
        commonData <- commonData %>%
          filter(analysis_id %in% c(input$analysisIdTable1))
        commonData
      } else {
        NULL
      }
    })

    # incidence_attrition

    incidenceAttritionCommon <- reactive({
      if (!is.null(uploadedFiles$dataIP$incidence_attrition)) {
        commonData <- uploadedFiles$dataIP$incidence_attrition
        if (class(commonData$excluded_records) == "character") {
          commonData$excluded_records <- as.numeric(commonData$excluded_records)
        }
        if (class(commonData$excluded_subjects) == "character") {
          commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
        }
        commonData[is.na(commonData)] = 0
        commonData <- commonData %>%
          filter(analysis_id %in% c(input$analysisIdTable1))
        commonData
      } else {
        NULL
      }
    })

    # 3. Interactive menu

    output$itemSelectionMenu <- renderUI({
      column(tags$b("Item selection"),
             width = 12,
             bucket_list(header = "Select the figures you want in the report",
                         group_name = "bucket_list_group",
                         orientation = "horizontal",
                         add_rank_list(text = "Drag from here",
                                       labels = itemsList$objects[["items"]],
                                       input_id = "objectMenu"),
                         add_rank_list(text = "to here",
                                       labels = NULL,
                                       input_id = "objectSelection")
             )
      )
    })

    # Item preview

    # Renders the objectSelection into the main dashboard space
    output$navPanelPreview <- renderUI({
      previewPanels <- lapply(input$objectSelection,
                              tabPanelSelection,
                              uploadedFiles = uploadedFiles,
                              version = input$dataVersionTP)
      do.call(navlistPanel, previewPanels)
    })

    # Objects to be rendered in the UI

    # Table 1

    output$previewTable1 <- renderTable({
      # objectChoice <- "Table - Number of participants"
      objectChoice <- "Table - Number of participants"
      prevalence_attrition <- prevalenceAttritionCommon()
      incidence_attrition <- incidenceAttritionCommon()
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = objectChoice)))
    }, colnames = FALSE)

    observeEvent(input$lockTableNumPar, {
      objectChoice <- "Table - Number of participants"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttritionCommon()
      dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttritionCommon()
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTable1
    })

    # lockItemsServer(id = "lockTableNumPar",
    #                 dataReport = dataReport,
    #                 prevalenceAttrition = prevalenceAttritionCommon(),
    #                 incidenceAttrition = incidenceAttritionCommon(),
    #                 captionInput = input$captionTable1)

    output$previewTableAttInc <- renderTable({
      objectChoice <- "Table - Incidence Attrition"
      incidence_attrition <- incidenceAttritionCommon()
      attritionDataType <- "incidence"
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = objectChoice)))
    }, colnames = FALSE)

    observeEvent(input$lockTableIncAtt, {
      objectChoice <- "Table - Incidence Attrition"
      attritionDataType <- "incidence"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttritionCommon()
      dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTableInc
    })


    # lockItemsServer(id = "lockTableIncAtt",
    #                 dataReport = dataReport,
    #                 incidenceAttrition = incidenceAttritionCommon(),
    #                 captionInput = input$captionTableInc)

    output$previewTableAttPrev <- renderTable({
      objectChoice <- "Table - Prevalence Attrition"
      attritionDataType <- "prevalence"
      prevalence_attrition <- prevalenceAttritionCommon()
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = objectChoice)))
    }, colnames = FALSE)

    observeEvent(input$lockTablePrevAtt, {
      objectChoice <- "Table - Prevalence Attrition"
      attritionDataType <- "prevalence"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttritionCommon()
      dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTablePrev
    })

    # lockItemsServer(id = "lockTablePrevAtt",
    #                 dataReport = dataReport,
    #                 prevalenceAttrition = prevalenceAttritionCommon(),
    #                 captionInput = input$captionTablePrev)

    # Table Att Inc

    output$previewTableSex <- render_gt({
      objectChoice <- "Table - Number of participants by sex and age group"
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      # Preview object
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = objectChoice)))
    })

    observeEvent(input$lockTableSex, {
      objectChoice <- "Table - Number of participants by sex and age group"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- uploadedFiles$dataIP$incidence_estimates
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTableSexAge
    })

    # lockItemsServer(id = "lockTableSex",
    #                 dataReport = dataReport,
    #                 incidenceEstimates = uploadedFiles$dataIP$incidence_estimates,
    #                 captionInput = input$captionTableSexAge)

    # Figure 1

    incidenceFigure1 <- reactive({

      objectChoice <- "Plot - Incidence rate per year"
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidenceYear))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidenceYear))
      # Database
      if (length(input$databaseIncidenceYear) != 1 || input$databaseIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(cdm_name %in% c(input$databaseIncidenceYear))
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

      return(incidence_estimates)
    })

    previewFigure1 <- reactive({
      objectChoice <- "Plot - Incidence rate per year"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemType(input$facetIncidenceYear)
      incidence_estimates <- incidenceFigure1()
      eval(parse(text = expression))
    })

    output$previewFigure1 <- renderPlot({
      req(previewFigure1())
      previewFigure1()
    })

    # Lock data Figure 1
    observeEvent(input$lockDataIncidenceYear, {
      objectChoice <- "Plot - Incidence rate per year"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- incidenceFigure1()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetIncidenceYear
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionIncYear
    })

    # Download Figure 1

    output$downloadFigure1Inc <- downloadHandler(
      filename = function() {
        paste("Figure1Inc", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure1(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # Figure 2

    incidenceFigure2Sex <- reactive({
      objectChoice <- "Plot - Incidence rate per year by sex"
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult",
                                      "IncidenceResult",
                                      "tbl_df",
                                      "tbl",
                                      "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidenceSex))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidenceSex))
      # Database
      if (length(input$databaseIncidenceSex) != 1 || input$databaseIncidenceSex != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(cdm_name %in% c(input$databaseIncidenceSex))
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

      return(incidence_estimates)
    })

    previewFigure2 <- reactive({
      objectChoice <- "Plot - Incidence rate per year by sex"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemTypeSex(input$facetIncidenceSex)
      incidence_estimates <- incidenceFigure2Sex()
      eval(parse(text = expression))
    })

    output$previewFigure2 <- renderPlot({
      req(previewFigure2())
      previewFigure2()
    })

    # Lock data Figure 2
    observeEvent(input$lockDataIncidenceSex, {
      objectChoice <- "Plot - Incidence rate per year by sex"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- incidenceFigure2Sex()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetIncidenceSex
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionIncSex
    })

    # Download Figure 2

    output$downloadFigure2IncSex <- downloadHandler(
      filename = function() {
        paste("Figure2IncSex", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure2(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # Figure 3

    incidenceFigure3Age <- reactive({

      objectChoice <- "Plot - Incidence rate per year by age"
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult",
                                      "IncidenceResult",
                                      "tbl_df",
                                      "tbl",
                                      "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidenceAge))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidenceAge))
      # Database
      if (length(input$databaseIncidenceAge) != 1 || input$databaseIncidenceAge != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(cdm_name %in% c(input$databaseIncidenceAge))
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

      return(incidence_estimates)


    })

    previewFigure3 <- reactive({
      objectChoice <- "Plot - Incidence rate per year by age"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemTypeAge(input$facetIncidenceAge)
      incidence_estimates <- incidenceFigure3Age()
      eval(parse(text = expression))
    })

    output$previewFigure3 <- renderPlot({
      req(previewFigure3())
      previewFigure3()
    })

    # Lock data Figure 3
    observeEvent(input$lockDataIncidenceAge, {
      objectChoice <- "Plot - Incidence rate per year by age"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- incidenceFigure3Age()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetIncidenceAge
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionIncAge
    })

    # Download Figure 3

    output$downloadFigure3IncAge <- downloadHandler(
      filename = function() {
        paste("Figure3IncAge", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure3(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # Update according to facet incidence

    observeEvent(input$facetIncidenceSex, {
      if  (input$facetIncidenceSex == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databaseIncidenceSex",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$dataIP$incidence_estimates$cdm_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomeIncidenceSex",
                          label = "Outcome",
                          choices = unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))

      } else {
        updatePickerInput(session,
                          inputId = "databaseIncidenceSex",
                          label = "Database",
                          choices = unique(uploadedFiles$dataIP$incidence_estimates$cdm_name),
                          selected = uploadedFiles$dataIP$incidence_estimates$cdm_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomeIncidenceSex",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    observeEvent(input$facetIncidenceAge, {
      if  (input$facetIncidenceAge == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databaseIncidenceAge",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$dataIP$incidence_estimates$cdm_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomeIncidenceAge",
                          label = "Outcome",
                          choices = unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))
      } else {

        updatePickerInput(session,
                          inputId = "databaseIncidenceAge",
                          label = "Database",
                          choices = unique(uploadedFiles$dataIP$incidence_estimates$cdm_name),
                          selected = uploadedFiles$dataIP$incidence_estimates$cdm_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomeIncidenceAge",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    # Figure 4: Prevalence rate per year

    prevalenceFigure4 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year"
      prevalence_estimates <- uploadedFiles$dataIP$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult",
                                       "PrevalenceResult",
                                       "tbl_df",
                                       "tbl",
                                       "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceYear) != 1 || input$databasePrevalenceYear != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(cdm_name %in% c(input$databasePrevalenceYear))
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

      return(prevalence_estimates)

    })

    previewFigure4 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemType(input$facetPrevalenceYear)
      prevalence_estimates <- prevalenceFigure4()
      eval(parse(text = expression))
    })

    output$previewFigure4 <- renderPlot({
      req(previewFigure4())
      previewFigure4()
    })

    # Lock data Figure 4
    observeEvent(input$lockDataPrevalenceYear, {
      objectChoice <- "Plot - Prevalence rate per year"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["prevalence_estimates"]] <- prevalenceFigure4()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetPrevalenceYear
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionPrevYear
    })

    # Download Figure 4

    output$downloadFigure4Prev <- downloadHandler(
      filename = function() {
        paste("Figure4Prev", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure4(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # Figure 5: Prevalence rate per year by sex

    prevalenceFigure5 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year by sex"
      prevalence_estimates <- uploadedFiles$dataIP$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceSex) != 1 || input$databasePrevalenceSex != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(cdm_name %in% c(input$databasePrevalenceSex))
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

      return(prevalence_estimates)

    })

    previewFigure5 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year by sex"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemTypeSex(input$facetPrevalenceSex)
      prevalence_estimates <- prevalenceFigure5()
      eval(parse(text = expression))
    })

    output$previewFigure5 <- renderPlot({
      req(previewFigure5())
      previewFigure5()
    })

    # Lock data Figure 5
    observeEvent(input$lockDataPrevalenceSex, {
      objectChoice <- "Plot - Prevalence rate per year by sex"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["prevalence_estimates"]] <- prevalenceFigure5()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetPrevalenceSex
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionPrevSex
    })

    # Download Figure 5

    output$downloadFigure5Prev <- downloadHandler(
      filename = function() {
        paste("Figure5PrevSex", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure5(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # Figure 6: Prevalence rate per year by age

    prevalenceFigure6 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year by age"
      prevalence_estimates <- uploadedFiles$dataIP$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult", "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalenceAge) != 1 || input$databasePrevalenceAge != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(cdm_name %in% c(input$databasePrevalenceAge))
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

      return(prevalence_estimates)
    })

    previewFigure6 <- reactive({
      objectChoice <- "Plot - Prevalence rate per year by age"
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = objectChoice) %>%
        addPreviewItemTypeAge(input$facetPrevalenceAge)
      prevalence_estimates <- prevalenceFigure6()
      eval(parse(text = expression))
    })

    output$previewFigure6 <- renderPlot({
      req(previewFigure6())
      previewFigure6()
    })

    # Lock data Figure 6
    observeEvent(input$lockDataPrevalenceAge, {
      objectChoice <- "Plot - Prevalence rate per year by age"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["prevalence_estimates"]] <- prevalenceFigure6()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- input$facetPrevalenceAge
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionPrevAge
    })

    # Download Figure 6

    output$downloadFigure6Prev <- downloadHandler(
      filename = function() {
        paste("Figure6PrevAge", ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = previewFigure6(), device = "png", height = 500, width = 845, units = "mm")
      }
    )

    # SunburstPlot

    treatmentDataSunburst <- reactive({
      if (input$dataVersionTP == "2.5.2") {
        treatmentPathways <- uploadedFiles[["dataTP"]][["treatmentPathways"]]
        treatmentPathways %>%
          filter(sex == input$sexSunburst,
                 age == input$ageSunburst,
                 indexYear == input$indexSunburst,
                 cdm_name == input$cdmSunburst)
      } else if (input$dataVersionTP == "2.5.0") {
        treatmentPathways <- uploadedFiles[["dataTP"]][["treatmentPathways"]]
        treatmentPathways %>%
          filter(sex == input$sexSunburst,
                 age == input$ageSunburst,
                 index_year == input$indexSunburst,
                 cdm_name == input$cdmSunburst)
      }
    })

    treatmentDataSankey <- reactive({
      if (input$dataVersionTP == "2.5.2") {
        treatmentPathways <- uploadedFiles[["dataTP"]][["treatmentPathways"]]
        treatmentPathways %>%
          filter(sex == input$sexSankey,
                 age == input$ageSankey,
                 indexYear == input$indexSankey,
                 cdm_name == input$cdmSankey)
      } else if (input$dataVersionTP == "2.5.0") {
        treatmentPathways <- uploadedFiles[["dataTP"]][["treatmentPathways"]]
        treatmentPathways %>%
          filter(sex == input$sexSankey,
                 age == input$ageSankey,
                 index_year == input$indexSankey,
                 cdm_name == input$cdmSankey)
      }
    })

    output$previewSunburstPlot <- renderUI({
      objectChoice <- "Sunburst Plot - TreatmentPatterns"
      treatmentPathways <- treatmentDataSunburst()
      TreatmentPatterns::createSunburstPlot2(treatmentPathways, groupCombinations = FALSE)
    })

    output$downloadSunburst <- downloadHandler(
      filename = function() {
        paste("SunburstDiagram", ".png", sep = "")
      },
      content = function(file) {
        sunburstHTML <- here::here("sunburstDiagram.html")
        createSunburstPlot(treatmentPathways = treatmentDataSunburst(),
                                              outputFile = sunburstHTML,
                                              returnHTML = FALSE)
        sunburstPNG <- tempfile(pattern = "sunburstPlot", fileext = ".png")
        webshot2::webshot(
          url = sunburstHTML,
          file = sunburstPNG,
          vwidth = 1200,
          vheight = 10)
        sunburstPath <- normalizePath(sunburstPNG)
        file.copy(sunburstPath, file)
      }
    )

    observeEvent(input$lockTreatmentSunburst, {
        objectChoice <- "Sunburst Plot - TreatmentPatterns"
        sunburstHTML <- here::here("sunburstDiagram.html")
        createSunburstPlot(treatmentPathways = treatmentDataSunburst(),
                                              outputFile = sunburstHTML,
                                              returnHTML = FALSE)
        sunburstPNG <- tempfile(pattern = "sunburstPlot", fileext = ".png")
        webshot2::webshot(
          url = sunburstHTML,
          file = sunburstPNG,
          vwidth = 1200,
          vheight = 10)
        sunburstPath <- normalizePath(sunburstPNG)
        message("Adding filename to dataReport")
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]][[objectChoice]][["treatmentPathways"]] <- treatmentDataSunburst()
        dataReport[[randomId]][[objectChoice]][["outputFile"]] <- sunburstHTML
        dataReport[[randomId]][[objectChoice]][["returnHTML"]] <- FALSE
        dataReport[[randomId]][[objectChoice]][["fileImage"]] <- sunburstPNG
        message("Filename added to dataReport")
    })

    # Sankey

    output$previewSankeyDiagram <- renderUI({
      objectChoice <- "Sankey Diagram - TreatmentPatterns"
      treatmentPathways <- treatmentDataSankey()
      TreatmentPatterns::createSankeyDiagram2(treatmentPathways, groupCombinations = TRUE)
    })

    output$downloadSankey <- downloadHandler(
      filename = function() {
        paste("SankeyDiagram", ".png", sep = "")
      },
      content = function(file) {
        sankeyHTML <- here::here("sankeyDiagram.html")
        TreatmentPatterns::createSankeyDiagram(treatmentPathways = treatmentDataSankey(),
                                               outputFile = sankeyHTML,
                                               returnHTML = FALSE,
                                               groupCombinations = FALSE,
                                               minFreq = 1)
        sankeytPNG <- tempfile(pattern = "sankeyPlot", fileext = ".png")
        webshot2::webshot(
          url = sankeyHTML,
          file = sankeytPNG,
          vwidth = 1200,
          vheight = 10)
        sankeyPath <- normalizePath(sankeytPNG)
        file.copy(sankeyPath, file)
      }
    )

    observeEvent(input$lockTreatmentSankey, {
      objectChoice <- "Sankey Diagram - TreatmentPatterns"
      sankeyHTML <- here::here("sankeyDiagram.html")
      TreatmentPatterns::createSankeyDiagram(treatmentPathways = treatmentDataSankey(),
                                             outputFile = sankeyHTML,
                                             returnHTML = FALSE,
                                             groupCombinations = FALSE,
                                             minFreq = 1)
      sankeytPNG <- tempfile(pattern = "sankeyPlot", fileext = ".png")
      webshot2::webshot(
        url = sankeyHTML,
        file = sankeytPNG,
        vwidth = 1200,
        vheight = 10)
      sankeyPath <- normalizePath(sankeytPNG)
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["treatmentPathways"]] <- treatmentDataSankey()
      dataReport[[randomId]][[objectChoice]][["outputFile"]] <- sankeyHTML
      dataReport[[randomId]][[objectChoice]][["returnHTML"]] <- FALSE
      dataReport[[randomId]][[objectChoice]][["fileImage"]] <- sankeytPNG
      message("Filename added to dataReport")
    })

    # PatientProfiles

    dataCharacteristics<- characteristicsServer(id = "characteristics",
                                              dataset = reactive(uploadedFiles$dataPP$`Summarised Characteristics`))

    observe({
      for (key in names(dataCharacteristics())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataCharacteristics()
      }
    }) %>%
      bindEvent(dataCharacteristics())

    dataLSC <- characteristicsServer(id = "lsc",
                                     dataset = reactive(uploadedFiles$dataPP$`Summarised Large Scale Characteristics`))

    observe({
      for (key in names(dataLSC())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataLSC()
      }
    }) %>%
      bindEvent(dataLSC())

    # Update according to facet prevalence

    observeEvent(input$facetPrevalenceSex, {
      if  (input$facetPrevalenceSex == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databasePrevalenceSex",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomePrevalenceSex",
                          label = "Outcome",
                          choices = unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))

      } else {
        updatePickerInput(session,
                          inputId = "databasePrevalenceSex",
                          label = "Database",
                          choices = unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name),
                          selected = uploadedFiles$dataIP$prevalence_estimates$cdm_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomePrevalenceSex",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    observeEvent(input$facetPrevalenceAge, {
      if  (input$facetPrevalenceAge == "Facet by database") {
        updatePickerInput(session,
                          inputId = "databasePrevalenceAge",
                          label = "Database",
                          choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)),
                          selected = "All",
                          options = list(
                            maxOptions = (length(unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name))+1)
                          ))

        updatePickerInput(session,
                          inputId = "outcomePrevalenceAge",
                          label = "Outcome",
                          choices = unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name),
                          selected = uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name[1],
                          options = list(
                            maxOptions = 1
                          ))
      } else {

        updatePickerInput(session,
                          inputId = "databasePrevalenceAge",
                          label = "Database",
                          choices = unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name),
                          selected = uploadedFiles$dataIP$prevalence_estimates$cdm_name[1],
                          options = list(
                            maxOptions = 1
                          )
        )
        updatePickerInput(session,
                          inputId = "outcomePrevalenceAge",
                          label = "Outcome",
                          choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                          selected = "All")

      }
    })

    objectsListPreview <- reactive({
      dataReportList <- reactiveValuesToList(dataReport)
      if (length(dataReportList) == 0) {
        return("None")
      } else {
        objectList <- list()
        for (i in seq(1:length(dataReportList))) {
          objectList <- rbind(objectList, names(dataReportList[[i]]))
        }
        result <- unlist(objectList)
        return(result)
      }
    })

    # output$dataReportMenu <- renderDT({
    #   dataReportFrame <- data.frame(
    #     Name = objectsListPreview()
    #   )
    #   DT::datatable(dataReportFrame, options = list(dom = 't'))
    # })

    output$dataReportMenu <- renderPrint({
      # dataReport
      dataReportList <- reactiveValuesToList(dataReport)
      dataReportList
      # length(dataReportList) == 0
      # objectsListPreview()
    })

    # Word report generator
    output$generateReport <- downloadHandler(
      filename = function() {
        paste0("generatedReport.docx")
      },
      content = function(file) {
        # Load template
        incidencePrevalenceDocx <- read_docx(path = system.file("templates",
                                                                "word",
                                                                "darwinTemplate.docx",
                                                                package = "ReportGenerator"))
        # Reverse selection menu list
        # reverseList <- rev(names(dataReport))

        dataReportList <- reactiveValuesToList(dataReport)
        dataReportList <- rev(dataReportList)

        # Loop through ever object selected in the menu
        for (i in seq(1:length(dataReportList))) {
          # i <- "Table - Incidence Attrition"
          # Get the function to generate and print in report
          titleText <- names(dataReportList[[i]])
          expression <- getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = titleText)
          # Get relevant options for the function
          itemOptions <- getItemConfig(input = "title",
                                       output = "options",
                                       inputValue = titleText)

          # Additional parameter if there are options for the graphs
          if (!is.null(itemOptions)) {
            if (grepl("by sex", titleText)) {
              expression <- expression %>%
                addPreviewItemTypeSex(dataReportList[[i]][[1]][["plotOption"]])
            } else if (grepl("by age", titleText)) {
              expression <- expression %>%
                addPreviewItemTypeAge(dataReportList[[i]][[1]][["plotOption"]])
            } else  {
              expression <- expression %>%
                addPreviewItemType(dataReportList[[i]][[1]][["plotOption"]])
            }
          }

          # Evaluate function
          object <- eval(parse(text = expression), envir = dataReportList[[i]][[1]])

          # Check class of every function and add it to the word report accordingly
          if ("gt_tbl" %in% class(object)) {
            body_end_section_landscape(incidencePrevalenceDocx)
            body_add_gt(incidencePrevalenceDocx, value = object)
            body_add(incidencePrevalenceDocx,
                     value = dataReportList[[i]][[1]][["caption"]])
            body_add(incidencePrevalenceDocx,
                     value = titleText,
                     style = "Heading 1 (Agency)")
            body_end_section_portrait(incidencePrevalenceDocx)

          } else if ("ggplot" %in% class(object)) {
            body_end_section_landscape(incidencePrevalenceDocx)
            body_add_gg(x = incidencePrevalenceDocx,
                        value = object,
                        style = "Normal")
            body_add(incidencePrevalenceDocx,
                     value = dataReportList[[i]][[1]][["caption"]])
            body_add(incidencePrevalenceDocx,
                     value = titleText,
                     style = "Heading 1 (Agency)")
            body_end_section_portrait(incidencePrevalenceDocx)

          } else if ("huxtable" %in% class(object)) {
            body_end_section_landscape(incidencePrevalenceDocx)
            body_add_table(incidencePrevalenceDocx,
                           value = object,
                           style = "TableOverall",
                           header = FALSE)
            body_add_par(incidencePrevalenceDocx, " ")
            body_add(incidencePrevalenceDocx,
                     value = dataReportList[[i]][[1]][["caption"]])
            body_add(incidencePrevalenceDocx,
                     value = titleText,
                     style = "Heading 1 (Agency)")
            body_end_section_portrait(incidencePrevalenceDocx)
          }

          if (titleText == "Sunburst Plot - TreatmentPatterns") {
            body_add_img(x = incidencePrevalenceDocx,
                         src = dataReportList[[i]][[1]][["fileImage"]],
                         height = 5.5,
                         width = 7)
            body_add(incidencePrevalenceDocx,
                     value = titleText,
                     style = "Heading 1 (Agency)")

            }  else if (titleText == "Sankey Diagram - TreatmentPatterns") {
              body_add_img(x = incidencePrevalenceDocx,
                           src = dataReportList[[i]][[1]][["fileImage"]],
                           height = 3,
                           width = 7)
              body_add(incidencePrevalenceDocx,
                       value = titleText,
                       style = "Heading 1 (Agency)")
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
