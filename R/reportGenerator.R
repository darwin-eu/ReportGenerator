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
  options(shiny.maxRequestSize = 1000*1024^2)

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
                          DTOutput("dataReportMenu"),
                          tags$br(),
                          downloadButton("generateReport", "Generate Report"),
                          tags$br()
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
                                    dataPP = NULL,
                                    dataCS = NULL)
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

    # CohortSurvival
    observeEvent(input$datasetLoadCS, {
      # Read file paths
      inFile <- input$datasetLoadCS
      fileDataPath <- inFile$datapath
      fileName <- inFile$name
      # Temp directory to unzip files
      csvLocation <- file.path(tempdir(), "dataLocation")
      dir.create(csvLocation)
      # Joins one or several zips from different data partners into the reactive value
      versionData <- input$dataVersionCS
      uploadedFiles$dataCS <- joinDatabase(fileDataPath = fileDataPath,
                                           fileName = fileName,
                                           package = "CohortSurvival",
                                           versionData = versionData,
                                           csvLocation = csvLocation)
      if (length(uploadedFiles$dataCS) == 0) {
        show_alert(title = "Data mismatch",
                   text = "Not a valid CohortSurvival file or check version")
      }
      # Get list of items to show in toggle menu
      itemsCS <- names(uploadedFiles$dataCS)
      itemsCSList <- getItemsList(itemsCS)
      # Items list into reactive value to show in toggle menu
      itemsList$objects[["items"]] <-  c(itemsList$objects[["items"]], itemsCSList)
      unlink(csvLocation, recursive = TRUE)
    })

    # Reset and back to initial tab
    observeEvent(input$resetData, {
      itemsList$objects <- NULL
      uploadedFiles <- reactiveValues(dataIP = NULL,
                                      dataTP = NULL,
                                      dataPP = NULL)
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

    # 1. Interactive menu

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

    # 2.Assign Data

    dataReport <- reactiveValues()

    # prevalence_attrition

    # prevalenceAttritionCommon <- reactive({
    #   if (!is.null(uploadedFiles$dataIP$prevalence_attrition)) {
    #     commonData <- uploadedFiles$dataIP$prevalence_attrition
    #     if (class(commonData$excluded_records) == "character") {
    #       commonData$excluded_records <- as.numeric(commonData$excluded_records)
    #     }
    #     if (class(commonData$excluded_subjects) == "character") {
    #       commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
    #     }
    #     commonData[is.na(commonData)] = 0
    #     commonData <- commonData %>%
    #       filter(analysis_id %in% c(input$analysisIdTable1))
    #     commonData
    #   } else {
    #     NULL
    #   }
    # })
    #
    # # incidence_attrition
    #
    # incidenceAttritionCommon <- reactive({
    #   if (!is.null(uploadedFiles$dataIP$incidence_attrition)) {
    #     commonData <- uploadedFiles$dataIP$incidence_attrition
    #     if (class(commonData$excluded_records) == "character") {
    #       commonData$excluded_records <- as.numeric(commonData$excluded_records)
    #     }
    #     if (class(commonData$excluded_subjects) == "character") {
    #       commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
    #     }
    #     commonData[is.na(commonData)] = 0
    #     commonData <- commonData %>%
    #       filter(analysis_id %in% c(input$analysisIdTable1))
    #     commonData
    #   } else {
    #     NULL
    #   }
    # })
    #
    #
    # # Objects to be rendered in the UI
    #
    # # Table 1
    #
    # output$previewTable1 <- renderTable({
    #   # objectChoice <- "Table - Number of participants"
    #   objectChoice <- "Table - Number of participants"
    #   prevalence_attrition <- prevalenceAttritionCommon()
    #   incidence_attrition <- incidenceAttritionCommon()
    #   eval(parse(text = getItemConfig(input = "title",
    #                                   output = "function",
    #                                   inputValue = objectChoice)))
    # }, colnames = FALSE)

    attritionServer(id = "Table - Number of participants", uploadedFiles)

    # observeEvent(input$lockTableNumPar, {
    #   objectChoice <- "Table - Number of participants"
    #   chars <- c(0:9, letters, LETTERS)
    #   randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
    #   dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttritionCommon()
    #   dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttritionCommon()
    #   dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTable1
    # })
    #
    # output$previewTableAttInc <- renderTable({
    #   objectChoice <- "Table - Incidence Attrition"
    #   incidence_attrition <- incidenceAttritionCommon()
    #   attritionDataType <- "incidence"
    #   eval(parse(text = getItemConfig(input = "title",
    #                                   output = "function",
    #                                   inputValue = objectChoice)))
    # }, colnames = FALSE)
    #
    # observeEvent(input$lockTableIncAtt, {
    #   objectChoice <- "Table - Incidence Attrition"
    #   attritionDataType <- "incidence"
    #   chars <- c(0:9, letters, LETTERS)
    #   randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
    #   dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttritionCommon()
    #   dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
    #   dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTableInc
    # })
    #
    # output$previewTableAttPrev <- renderTable({
    #   objectChoice <- "Table - Prevalence Attrition"
    #   attritionDataType <- "prevalence"
    #   prevalence_attrition <- prevalenceAttritionCommon()
    #   eval(parse(text = getItemConfig(input = "title",
    #                                   output = "function",
    #                                   inputValue = objectChoice)))
    # }, colnames = FALSE)
    #
    # observeEvent(input$lockTablePrevAtt, {
    #   objectChoice <- "Table - Prevalence Attrition"
    #   attritionDataType <- "prevalence"
    #   chars <- c(0:9, letters, LETTERS)
    #   randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
    #   dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttritionCommon()
    #   dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
    #   dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTablePrev
    # })
    #
    # output$previewTableSex <- render_gt({
    #   objectChoice <- "Table - Number of participants by sex and age group"
    #   incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
    #   # Preview object
    #   eval(parse(text = getItemConfig(input = "title",
    #                                   output = "function",
    #                                   inputValue = objectChoice)))
    # })
    #
    # observeEvent(input$lockTableSex, {
    #   objectChoice <- "Table - Number of participants by sex and age group"
    #   chars <- c(0:9, letters, LETTERS)
    #   randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
    #   dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- uploadedFiles$dataIP$incidence_estimates
    #   dataReport[[randomId]][[objectChoice]][["caption"]] <- input$captionTableSexAge
    # })

    # Incidence Modules

      # Year

    dataIncidenceYear <- incidenceServer(id = "Plot - Incidence rate per year",
                                     dataset = reactive(uploadedFiles$dataIP$incidence_estimates))

    observe({
      for (key in names(dataIncidenceYear())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataIncidenceYear()
      }
    }) %>%
      bindEvent(dataIncidenceYear())

    dataIncidenceSex <- incidenceServer(id = "Plot - Incidence rate per year by sex",
                                         dataset = reactive(uploadedFiles$dataIP$incidence_estimates))

      # Sex

    observe({
      for (key in names(dataIncidenceSex())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataIncidenceSex()
      }
    }) %>%
      bindEvent(dataIncidenceSex())

      # Age

    dataIncidenceAge <- incidenceServer(id = "Plot - Incidence rate per year by age",
                                        dataset = reactive(uploadedFiles$dataIP$incidence_estimates))

    observe({
      for (key in names(dataIncidenceAge())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataIncidenceAge()
      }
    }) %>%
      bindEvent(dataIncidenceAge())

    # Prevalence Modules

      # Year

    dataPrevalenceYear <- prevalenceServer(id = "Plot - Prevalence per year",
                                         dataset = reactive(uploadedFiles$dataIP$prevalence_estimates))

    observe({
      for (key in names(dataPrevalenceYear())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataPrevalenceYear()
      }
    }) %>%
      bindEvent(dataPrevalenceYear())

      # Sex

    dataPrevalenceSex <- prevalenceServer(id = "Plot - Prevalence per year by sex",
                                        dataset = reactive(uploadedFiles$dataIP$prevalence_estimates))

    observe({
      for (key in names(dataPrevalenceSex())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataPrevalenceSex()
      }
    }) %>%
      bindEvent(dataPrevalenceSex())

      # Age

    dataPrevalenceAge <- prevalenceServer(id = "Plot - Prevalence per year by age",
                                          dataset = reactive(uploadedFiles$dataIP$prevalence_estimates))

    observe({
      for (key in names(dataPrevalenceAge())) {
        chars <- c(0:9, letters, LETTERS)
        randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
        dataReport[[randomId]] <- dataPrevalenceAge()
      }
    }) %>%
      bindEvent(dataPrevalenceAge())

    # Treatment Patterns

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
      TreatmentPatterns::createSunburstPlot(treatmentPathways, groupCombinations = FALSE)
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
      TreatmentPatterns::createSankeyDiagram(treatmentPathways, groupCombinations = TRUE)
    })

    output$downloadSankey <- downloadHandler(
      filename = function() {
        paste("SankeyDiagram", ".png", sep = "")
      },
      content = function(file) {
        TreatmentPatterns::createSankeyDiagram(treatmentPathways = treatmentDataSankey(),
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
      TreatmentPatterns::createSankeyDiagram(treatmentPathways = treatmentDataSankey(),
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

    # PatientProfiles Modules
    dataCharacteristics <- characteristicsServer(id = "characteristics",
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

    # Cohort Survival Modules

    cohortSurvivalServer("survivalTable", uploadedFiles$dataCS$`Survival estimate`)
    cohortSurvivalServer("survivalPlot", uploadedFiles$dataCS$`Survival estimate`)
    cohortSurvivalServer("failureTable", uploadedFiles$dataCS$`Survival cumulative incidence`)
    cohortSurvivalServer("failurePlot", uploadedFiles$dataCS$`Survival cumulative incidence`)

    # Cohort Survival report items

    survivalEstimateData <- reactive({
      if (!is.null(uploadedFiles$dataCS$`Survival estimate`)) {
        uploadedFiles$dataCS$`Survival estimate` %>%
          dplyr::filter(cdm_name %in% input$`survivalTable-cdm_name`) %>%
          dplyr::filter(strata_name %in% input$`survivalTable-strata_name`) %>%
          dplyr::slice_head(n = input$`survivalTable-top_n`) %>%
          select(c("cdm_name", "result_type", "group_level", "strata_name",
                   "strata_level", "variable_type", "time", "estimate"))
      }
    })
    survivalEstimatePlotData <- reactive({
      if (!is.null(uploadedFiles$dataCS$`Survival estimate`)) {
        uploadedFiles$dataCS$`Survival estimate` %>%
          dplyr::filter(cdm_name %in% input$`survivalPlot-cdm_name`) %>%
          dplyr::filter(strata_name %in% input$`survivalPlot-strata_name`)
      }
    })
    cumulativeSurvivalData <- reactive({
      if (!is.null(uploadedFiles$dataCS$`Survival cumulative incidence`)) {
        uploadedFiles$dataCS$`Survival cumulative incidence` %>%
          dplyr::filter(cdm_name %in% input$`failureTable-cdm_name`) %>%
          dplyr::filter(strata_name %in% input$`failureTable-strata_name`) %>%
          dplyr::slice_head(n = input$`failureTable-top_n`) %>%
          select(c("cdm_name", "result_type", "group_level", "strata_name",
                   "strata_level", "variable_type", "time", "estimate"))
      }
    })
    cumulativeSurvivalPlotData <- reactive({
      if (!is.null(uploadedFiles$dataCS$`Survival cumulative incidence`)) {
        uploadedFiles$dataCS$`Survival cumulative incidence` %>%
          dplyr::filter(cdm_name %in% input$`failurePlot-cdm_name`) %>%
          dplyr::filter(strata_name %in% input$`failurePlot-strata_name`)
      }
    })

    observeEvent(input$locksurvivalTable, {
      objectChoice <- "Survival table"
      randomId <- stringr::str_c(sample(c(0:9, letters, LETTERS), 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["survivalEstimate"]] <- survivalEstimateData()
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$'survivalTable-captionSurvivalEstimateData'
    })

    observeEvent(input$locksurvivalPlot, {
      objectChoice <- "Survival plot"
      randomId <- stringr::str_c(sample(c(0:9, letters, LETTERS), 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["survivalEstimate"]] <- survivalEstimatePlotData()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- "Facet by database, colour by strata_name"
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$'survivalPlot-captionSurvivalEstimate'
    })

    observeEvent(input$lockfailureTable, {
      objectChoice <- "Cumulative incidence table"
      randomId <- stringr::str_c(sample(c(0:9, letters, LETTERS), 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["cumulativeSurvivalEstimate"]] <- cumulativeSurvivalData()
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$'failureTable-captionCumulativeIncidenceData'
    })

    observeEvent(input$lockfailurePlot, {
      objectChoice <- "Cumulative incidence plot"
      randomId <- stringr::str_c(sample(c(0:9, letters, LETTERS), 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]][["cumulativeSurvivalEstimate"]] <- cumulativeSurvivalPlotData()
      dataReport[[randomId]][[objectChoice]][["plotOption"]] <- "Facet by database, colour by strata_name"
      dataReport[[randomId]][[objectChoice]][["caption"]] <- input$'failurePlot-captionCumulativeIncidence'
    })

    # Data Report Preview

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

    output$dataReportMenu <- renderDT({
      dataReportFrame <- data.frame(
        Name = objectsListPreview()
      )
      DT::datatable(dataReportFrame, options = list(dom = 't'))
    })

    # To check data in report:

    # output$dataReportMenu <- renderPrint({
    #   # dataReport
    #   dataReportList <- reactiveValuesToList(dataReport)
    #   dataReportList
    #   # length(dataReportList) == 0
    #   # objectsListPreview()
    # })

    # Word report generator
    output$generateReport <- downloadHandler(
      filename = function() {
        "generatedReport.docx"
      },
      content = function(file) {
        shinyjs::disable("generateReport")
        # Load template and generate report
        reportDocx <- read_docx(path = system.file("templates",
                                                   "word",
                                                   "DARWIN_EU_Study_Report.docx",
                                                   package = "ReportGenerator"))
        generateReport(reportDocx,
                       rev(reactiveValuesToList(dataReport)),
                       file)
        shinyjs::enable("generateReport")
      }
    )
  }
  shinyApp(ui, server)
}
