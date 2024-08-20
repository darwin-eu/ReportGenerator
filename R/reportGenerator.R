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
#' @param logger optional logger object
#'
#' @import dplyr shiny shinydashboard shinyWidgets shinycssloaders officer flextable waldo readr yaml TreatmentPatterns PatientProfiles
#' @importFrom sortable bucket_list add_rank_list sortable_options
#' @importFrom utils read.csv tail unzip
#' @importFrom ggplot2 ggsave
#' @importFrom gto body_add_gt
#' @importFrom here here
#' @importFrom TreatmentPatterns createSankeyDiagram
#' @importFrom IncidencePrevalence plotIncidence plotPrevalence
#' @importFrom DT renderDT DTOutput
#' @importFrom stringr str_split
#' @importFrom ParallelLogger addDefaultConsoleLogger
#' @importFrom cli cli_progress_step
#' @export
reportGenerator <- function(logger = NULL) {

  logger <- ParallelLogger::addDefaultConsoleLogger()
  cli::cli_progress_step("Launching ReportGenerator", spinner = TRUE)
  # set global options
  options(shiny.maxRequestSize = 1000*1024^2, spinner.type = 5, spinner.color = "#0dc5c1",
          page.spinner.type = 5, page.spinner.color = "#0dc5c1")

  ui <- dashboardPage(
    dashboardHeader(title = "ReportGenerator"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("StudyPackage", datasetLoadUI("StudyPackage"),
                 startExpanded = TRUE),
        # Conditional panel to download data
        conditionalPanel(
          condition = "output.checkFileUploadedOut",
          tags$br(),
          actionButton('resetData', 'Reset data'),
          tags$br(),
          tags$br(),
          shinyjs::useShinyjs(),
          tags$head(tags$style(".dlStudyDataBtn{ margin-left:15px;margin-right:15px; color:#444 !important; }")),
          downloadButton("downloadStudyData", "Download dataset", class = "dlStudyDataBtn"),
          tags$br(),
          tags$br(),
          downloadButton("createReportApp", "Download Shiny App", class = "dlReportBtn"),
          checkboxInput("enableReporting", "Add reporting option", value = FALSE)
        )
        # ,
        # verbatimTextOutput("checkFileUploadedOut")
      )
    ),
    dashboardBody(
      tabsetPanel(
        id = "mainPanel",
        tabPanel("Item selection",
                 fluidRow(
                   box(uiOutput("itemSelectionMenu"),
                       tags$br())
                   # ,
                   # verbatimTextOutput("monitorData")
                 )),
        tabPanel("Item preview",
                 fluidRow(
                   column(width = 12,
                          h2("1. Choose objects"),
                          shinycssloaders::withSpinner(uiOutput("navPanelPreview"))
                   )
                 )
        ),
        tabPanel("Generate report",
                 fluidRow(
                   column(width = 6,
                          h2("2. Report Items"),
                          DTOutput("dataReportMenu"),
                          tags$br(),
                          tags$head(tags$style(".dlReportBtn{ margin-left:15px;margin-right:15px; margin-top:25px; color:#444 !important; }")),
                          splitLayout(
                            downloadButton("generateReport", "Generate Report", class = "dlReportBtn"),
                            downloadButton("saveReportData", "Save report items", class = "dlReportBtn"),
                            fileInput("loadReportItems",
                                      "Load report items",
                                      accept = c(".rds"),
                                      placeholder = "rds")
                          ),
                          div(id = "reportOutput")
                   )
                 )
        )
      )
    )
  )

  globalValues <- reactiveValues(count = 0)

  server <- function(input, output, session) {

    # create logger
    if (is.null(logger)) {
      log_file <- glue::glue("log.txt")
      logger <- log4r::logger(threshold = "INFO", appenders = list(log4r::console_appender(), log4r::file_appender(log_file)))
    }

    # 1. Load data
    datasetLoadServer("StudyPackage")

    # ReactiveValues for data and items menu
    uploadedFiles <- reactiveValues(IncidencePrevalence = NULL,
                                    TreatmentPatterns = NULL,
                                    CohortCharacteristics = NULL,
                                    CohortSurvival = NULL)
    itemsList <- reactiveValues(objects = NULL)

    # Check input data
    observeEvent(input$datasetLoad, {
      shinycssloaders::showPageSpinner()

      # Read  file paths
      inFile <- input$datasetLoad
      fileDataPath <- inFile$datapath
      fileName <- inFile$name
      # Temp directory to unzip files
      globalValues$count <- globalValues$count + 1
      csvLocation <- file.path(tempdir(), glue::glue("dataLocation{globalValues$count}"))
      dir.create(csvLocation)
      # Joins one or several zips into the reactive value
      uploadedFilesList <- joinDatabases(fileDataPath = fileDataPath,
                                         fileName = fileName,
                                         unzipDir = csvLocation,
                                         logger = logger)
      if (length(uploadedFilesList) == 0) {
        show_alert(title = "Data mismatch",
                   text = "No valid package files found")
      }
      pkgNames <- names(uploadedFiles)
      if ("IncidencePrevalence" %in% pkgNames) {
        uploadedFiles$IncidencePrevalence <- uploadedFilesList[["IncidencePrevalence"]]
      }
      if ("TreatmentPatterns" %in% pkgNames) {
        uploadedFiles$TreatmentPatterns <- uploadedFilesList[["TreatmentPatterns"]]
      }
      if ("CohortCharacteristics" %in% pkgNames) {
        uploadedFiles$CohortCharacteristics <- uploadedFilesList[["CohortCharacteristics"]]
      }
      if ("CohortSurvival" %in% pkgNames) {
        uploadedFiles$CohortSurvival <- uploadedFilesList[["CohortSurvival"]]
      }

      # # Generate verbatim text output dynamically based on user input
      # output$monitorData <- renderPrint({
      #   # reactiveValuesToList(uploadedFiles)
      #   # uploadedFiles[["CohortCharacteristics"]]
      #   uploadedFiles
      #
      # })

      # Get list of items to show in toggle menu
      for (pkgName in pkgNames) {
        pkgDataList <- uploadedFiles[[pkgName]]
        items <- names(pkgDataList)
        itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], getItemsList(items))
      }
      shinycssloaders::hidePageSpinner()
    })

    # Reset and back to initial tab
    observeEvent(input$resetData, {
      itemsList$objects <- NULL
      uploadedFiles <- reactiveValues(IncidencePrevalence = NULL,
                                      TreatmentPatterns = NULL,
                                      CohortCharacteristics = NULL,
                                      CohortSurvival = NULL)
      updateTabsetPanel(session, "mainPanel",
                        selected = "Item selection")
      datasetLoadServer("StudyPackage")
      dataReport$objects <- NULL
      shinyjs::html("reportOutput", "")
    })

    checkFileUploaded <- reactive({
      # return(!is.null(input$datasetLoad))
      return(!is.null(itemsList$objects))
    })

    output$checkFileUploadedOut <- reactive({
      checkFileUploaded()
    })

    outputOptions(output, "checkFileUploadedOut", suspendWhenHidden = FALSE)

    # 1. Interactive menu

    output$itemSelectionMenu <- renderUI({
      column(tags$b("Item selection"),
             width = 12,
             bucket_list(header = "Select the figures you want in the report",
                         group_name = "bucket_list_group",
                         orientation = "horizontal",
                         add_rank_list(text = "Drag from here",
                                       labels = itemsList$objects[["items"]],
                                       input_id = "objectMenu",
                                       options = sortable_options(multiDrag = TRUE)),
                         add_rank_list(text = "to here",
                                       labels = NULL,
                                       input_id = "objectSelection",
                                       options = sortable_options(multiDrag = TRUE))
             )
      )
    })

    # Item preview

    # Renders the objectSelection into the main dashboard space
    output$navPanelPreview <- renderUI({
      previewPanels <- lapply(input$objectSelection,
                              tabPanelSelection,
                              uploadedFiles = uploadedFiles)
      do.call(navlistPanel, c(previewPanels, list(widths = c(3, 9))))
    })

    # 2.Assign Data

    dataReport <- reactiveValues()

    # Inc/Prev Table Modules

      # Table w/ attrition data from Inc/Prev

    tableNumPar <- attritionServer(id = "Number of participants - Table",
                                   uploadedFiles = reactive(uploadedFiles))

    observe({
      for (key in names(tableNumPar())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- tableNumPar()
      }
    }) %>%
      bindEvent(tableNumPar())

    # Attrition Incidence only

    tableAttInc <- attritionServer(id = "Incidence Attrition - Table",
                                   uploadedFiles = reactive(uploadedFiles))

    observe({
      for (key in names(tableAttInc())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- tableAttInc()
      }
    }) %>%
      bindEvent(tableAttInc())

    # Attrition Prevelence only

    tableAttPrev <- attritionServer(id = "Prevalence Attrition - Table",
                                    uploadedFiles = reactive(uploadedFiles))

    observe({
      for (key in names(tableAttPrev())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- tableAttPrev()
      }
    }) %>%
      bindEvent(tableAttPrev())

    # Table Sex/Age

    tableSexAge <- tableServer(id = "Table - Number of participants by sex and age group",
                               reactive(uploadedFiles))

    observe({
      for (key in names(tableSexAge())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- tableSexAge()
      }
    }) %>%
      bindEvent(tableSexAge())


    # Incidence Modules

      # Year

    dataIncidenceYear <- incidenceServer(id = "Incidence rate per year - Plot",
                                         reactive(uploadedFiles))

    observe({
      for (key in names(dataIncidenceYear())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataIncidenceYear()
      }
    }) %>%
      bindEvent(dataIncidenceYear())

    # Sex

    dataIncidenceSex <- incidenceServer(id = "Incidence rate per year by sex - Plot",
                                        reactive(uploadedFiles))

    observe({
      for (key in names(dataIncidenceSex())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataIncidenceSex()
      }
    }) %>%
      bindEvent(dataIncidenceSex())

      # Age

    dataIncidenceAge <- incidenceServer(id = "Incidence rate per year by age - Plot",
                                        reactive(uploadedFiles))

    observe({
      for (key in names(dataIncidenceAge())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataIncidenceAge()
      }
    }) %>%
      bindEvent(dataIncidenceAge())

    # Prevalence Modules

      # Year

    dataPrevalenceYear <- prevalenceServer(id = "Prevalence per year - Plot",
                                           reactive(uploadedFiles))

    observe({
      for (key in names(dataPrevalenceYear())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPrevalenceYear()
      }
    }) %>%
      bindEvent(dataPrevalenceYear())

      # Sex

    dataPrevalenceSex <- prevalenceServer(id = "Prevalence per year by sex - Plot",
                                          reactive(uploadedFiles))

    observe({
      for (key in names(dataPrevalenceSex())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPrevalenceSex()
      }
    }) %>%
      bindEvent(dataPrevalenceSex())

      # Age

    dataPrevalenceAge <- prevalenceServer(id = "Prevalence per year by age - Plot",
                                          reactive(uploadedFiles))

    observe({
      for (key in names(dataPrevalenceAge())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPrevalenceAge()
      }
    }) %>%
      bindEvent(dataPrevalenceAge())

    # Treatment Patterns Interactive Plots

    dataPatterns <- patternsServer("Treatment Pathways Interactive Plots",
                                   reactive(uploadedFiles))

    observe({
      for (key in names(dataPatterns())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPatterns()
      }
    }) %>%
      bindEvent(dataPatterns())


    # PatientProfiles Modules
    dataCharacteristics <- characteristicsServer("characteristics",
                                                 reactive(uploadedFiles))

    observe({
      for (key in names(dataCharacteristics())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataCharacteristics()
      }
    }) %>%
      bindEvent(dataCharacteristics())

    dataLSC <- characteristicsServer(id = "lsc",
                                     reactive(uploadedFiles))

    observe({
      for (key in names(dataLSC())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataLSC()
      }
    }) %>%
      bindEvent(dataLSC())

    # Cohort Survival Modules

    dataSurvivalTable <- cohortSurvivalServer("survivalTable", reactive(uploadedFiles))

    observe({
      for (key in names(dataSurvivalTable())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataSurvivalTable()
      }
    }) %>%
      bindEvent(dataSurvivalTable())

    dataSurvivalPlot <- cohortSurvivalServer("survivalPlot", reactive(uploadedFiles))

    observe({
      for (key in names(dataSurvivalPlot())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataSurvivalPlot()
      }
    }) %>%
      bindEvent(dataSurvivalPlot())

    dataFailureTable <- cohortSurvivalServer("failureTable", reactive(uploadedFiles))

    observe({
      for (key in names(dataFailureTable())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataFailureTable()
      }
    }) %>%
      bindEvent(dataFailureTable())

    dataFailurePlot <- cohortSurvivalServer("failurePlot", reactive(uploadedFiles))

    observe({
      for (key in names(dataFailurePlot())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataFailurePlot()
      }
    }) %>%
      bindEvent(dataFailurePlot())


    # Data Report Preview

    objectsListPreview <- reactive({
      if (is.null(dataReport$objects)) {
        return("None")
      } else {
        dataReportList <- reactiveValuesToList(do.call(reactiveValues, dataReport$objects))
        result <- data.frame(name = character(0), caption = character(0))
        for (i in seq(1:length(dataReportList))) {
          name <- names(dataReportList[[i]])
          reportItem <- dataReportList[[i]][[name]]
          caption <- ""
          if ("caption" %in% names(reportItem)) {
            caption <- reportItem$caption
          }
          caption <- ifelse(is.null(caption), "", caption)
          result <- rbind(result, data.frame(name = name, caption = caption))
        }
        return(result)
      }
    })

    output$dataReportMenu <- renderDT({
      dataReportFrame <- objectsListPreview()
      if (inherits(dataReportFrame, "data.frame")) {
        DT::datatable(dataReportFrame, options = list(dom = 't'))
      }
    })

    # Word report generator
    output$generateReport <- downloadHandler(
      filename = function() {
        "generatedReport.docx"
      },
      content = function(file) {
        shinycssloaders::showPageSpinner()
        shinyjs::disable("generateReport")
        # Load template and generate report
        shinyjs::html("reportOutput", "<br>Generating report", add = TRUE)
        reportDocx <- read_docx(path = system.file("templates",
                                                   "word",
                                                   "DARWIN_EU_Study_Report.docx",
                                                   package = "ReportGenerator"))
        reportItems <- list()
        if (!is.null(dataReport$objects)) {
          reportItems <- rev(reactiveValuesToList(do.call(reactiveValues, dataReport$objects)))
        }
        generateReport(reportDocx,
                       reportItems,
                       file,
                       logger)
        shinyjs::enable("generateReport")
        shinycssloaders::hidePageSpinner()
      }
    )

    # save report
    output$downloadStudyData <- downloadHandler(
      filename = "uploadedFiles.rds",
      content = function(file) {
        shinycssloaders::showPageSpinner()
        if (!is.null(uploadedFiles)) {
          saveRDS(reactiveValuesToList(uploadedFiles),
                  file)
        }
        shinycssloaders::hidePageSpinner()
      }
    )

    output$saveReportData <- downloadHandler(
      filename = "reportItems.rds",
      content = function(file) {
        if (!is.null(dataReport$objects)) {
          shinycssloaders::showPageSpinner()
          shinyjs::html("reportOutput", "<br>Saving report items to rds file", add = TRUE)
          shinyjs::disable("saveReportData")
          saveRDS(list("reportItems" = reactiveValuesToList(do.call(reactiveValues, dataReport$objects)),
                       "uploadedFiles" = reactiveValuesToList(uploadedFiles),
                       "itemsList" = reactiveValuesToList(do.call(reactiveValues, itemsList$objects))),
                  file)
          shinyjs::enable("saveReportData")
          shinycssloaders::hidePageSpinner()
        }
      }
    )

    # Check input data
    observeEvent(input$loadReportItems, {
      inFile <- input$loadReportItems
      fileDataPath <- inFile$datapath
      reportData <- readRDS(fileDataPath)
      dataReport$objects <- reportData$reportItems
      itemsList$objects <- reportData$itemsList
      upFiles <- reportData$uploadedFiles
      uploadedFiles$CohortSurvival <- upFiles$CohortSurvival
      uploadedFiles$IncidencePrevalence <- upFiles$IncidencePrevalence
      uploadedFiles$CohortCharacteristics <- upFiles$CohortCharacteristics
      uploadedFiles$TreatmentPatterns <- upFiles$TreatmentPatterns
      shinyjs::html("reportOutput", "<br>Loaded report items from rds file", add = TRUE)
    })

    # save report
    output$createReportApp <- downloadHandler(
      filename = "reportApp.zip",
      content = function(file) {
        shinycssloaders::showPageSpinner()
        # log4r::info(logger, paste("Create shiny application, reportingEnabled = ", input$enableReporting))
        cli::cli_h2("Creating Shiny App")
        uploadedFiles <- reactiveValuesToList(uploadedFiles)
        uploadedFilesValues <- unlist(lapply(names(uploadedFiles), FUN = function(name) {uploadedFiles[[name]]}))
        if (!is.null(uploadedFilesValues)) {

          # Specify source directory
          cli::cli_alert("Specify source directory")
          packagePath <- system.file("reportApp", package = "ReportGenerator")

          # Copy files
          cli::cli_alert("Copying files")
          targetPath <- tempdir()
          file.copy(packagePath, targetPath, recursive = TRUE)
          reportAppPath <- file.path(targetPath, "reportApp")
          # Copy config file
          cli::cli_alert("Copying config file")
          reportAppConfigPath <- file.path(reportAppPath, "config")
          dir.create(reportAppConfigPath)
          menuConfigFile <- "menuConfig.yaml"
          menuConfig <- system.file("config", menuConfigFile, package = "ReportGenerator")
          file.copy(menuConfig, file.path(reportAppConfigPath, menuConfigFile))
          # Copy modules file
          cli::cli_alert("Copying modules file")
          reportAppModulesPath <- file.path(reportAppPath, "modules")
          modulesFilesLocation <- system.file("R", package = "ReportGenerator")
          modulesFiles <- list.files(modulesFilesLocation, full.names = TRUE)
          modules <- grep("IncModules|attModules|automaticText|createSunburstPlot|csModules|dataExtraction|generateReport|ppModules|prevModules|standardTables|tabPanelSelection|tableModules|tpModules|utils", modulesFiles, value = TRUE)
          dir.create(reportAppModulesPath)
          file.copy(modules, reportAppModulesPath)

          # Copy template
          if (input$enableReporting) {
            cli::cli_alert("Copying template for reporting")
            reportTemplateFile <- "DARWIN_EU_Study_Report.docx"
            reportTemplate <- system.file("templates",
                                          "word",
                                          reportTemplateFile,
                                          package = "ReportGenerator")
            file.copy(reportTemplate, file.path(reportAppConfigPath, reportTemplateFile))


            # update ui
            file.remove(file.path(reportAppPath, "ui.R"))
            file.rename(file.path(reportAppPath, "ui_total.R"), file.path(reportAppPath, "ui.R"))
          } else {
            file.remove(file.path(reportAppPath, "ui_total.R"))
          }
          # Create results dir
          cli::cli_alert("Creating results directory")
          targetPathResults <- file.path(reportAppPath, "results")
          dir.create(targetPathResults)

          # Insert results from app
          cli::cli_alert("Inserting results in app")
          log4r::info(logger, "Add uploadedFiles and session results")
          saveRDS(uploadedFiles,
                  file.path(targetPathResults, "uploadedFiles.rds"))

          # session files
          # itemsListObjects <- reactiveValuesToList(do.call(reactiveValues, itemsList$objects))
          # if (!is.null(dataReport$objects)) {
          #   dataReportObjects <- reactiveValuesToList(do.call(reactiveValues, dataReport$objects))
          #   saveRDS(list("itemsList" = itemsListObjects,
          #                "reportItems" = dataReportObjects),
          #           file.path(targetPathResults, "session.rds"))
          # } else {
          #   saveRDS(list("itemsList" = itemsListObjects,
          #                "reportItems" = NULL),
          #           file.path(targetPathResults, "session.rds"))
          # }
          cli::cli_alert("Zipping file")
          zip::zip(zipfile = file, files = "reportApp", recurse = TRUE, root = targetPath)
          cli::cli_alert("Cleaning temp folders")
          unlink(reportAppPath, recursive = TRUE)
          log4r::info(logger, glue::glue("Shiny app has been created in {targetPath}"))
          cli::cli_alert("Shiny app has been created and ready for download")
          # shinyjs::html("createAppOutput", glue::glue("<br>Shiny app created in {targetPath}"), add = TRUE)
        } else {
          log4r::info(logger, "No files have been uploaded, app not created.")
          shinyjs::html("createAppOutput", glue::glue("<br>Please upload files before generating the app"), add = TRUE)
        }
        shinycssloaders::hidePageSpinner()
      },
      contentType = "application/zip"
    )

    # observeEvent(input$createReportApp, {
    #   log4r::info(logger, paste("Create shiny application, reportingEnabled = ", input$enableReporting))
    #
    #   uploadedFiles <- reactiveValuesToList(uploadedFiles)
    #   uploadedFilesValues <- unlist(lapply(names(uploadedFiles), FUN = function(name) {uploadedFiles[[name]]}))
    #   if (!is.null(uploadedFilesValues)) {
    #     # Specify source directory
    #     reportAppDir <- "reportApp"
    #     packagePath <- system.file(reportAppDir, package = "ReportGenerator")
    #
    #     # Copy files
    #     targetPath <- getwd()
    #     file.copy(packagePath, targetPath, recursive = T)
    #     reportAppPath <- file.path(targetPath, reportAppDir)
    #     reportAppConfigPath <- file.path(reportAppPath, "config")
    #     dir.create(reportAppConfigPath)
    #
    #     # Copy template
    #     if (input$enableReporting) {
    #       reportTemplateFile <- "DARWIN_EU_Study_Report.docx"
    #       reportTemplate <- system.file("templates",
    #                                     "word",
    #                                     reportTemplateFile,
    #                                     package = "ReportGenerator")
    #       file.copy(reportTemplate, file.path(reportAppConfigPath, reportTemplateFile))
    #       menuConfigFile <- "menuConfig.yaml"
    #       menuConfig <- system.file("config", menuConfigFile, package = "ReportGenerator")
    #       file.copy(menuConfig, file.path(reportAppConfigPath, menuConfigFile))
    #
    #       # update ui
    #       file.remove(file.path(reportAppPath, "ui.R"))
    #       file.rename(file.path(reportAppPath, "ui_total.R"), file.path(reportAppPath, "ui.R"))
    #     } else {
    #       file.remove(file.path(reportAppPath, "ui_total.R"))
    #     }
    #     # Create results dir
    #     targetPathResults <- file.path(reportAppPath, "results")
    #     dir.create(targetPathResults)
    #
    #     # Insert results from app
    #     log4r::info(logger, "Add uploadedFiles and session results")
    #     saveRDS(list("uploadedFiles" = uploadedFiles),
    #             file.path(targetPathResults, "uploadedFiles.rds"))
    #
    #     # session files
    #     itemsListObjects <- reactiveValuesToList(do.call(reactiveValues, itemsList$objects))
    #     if (!is.null(dataReport$objects)) {
    #       dataReportObjects <- reactiveValuesToList(do.call(reactiveValues, dataReport$objects))
    #       saveRDS(list("itemsList" = itemsListObjects,
    #                    "reportItems" = dataReportObjects),
    #               file.path(targetPathResults, "session.rds"))
    #     } else {
    #       saveRDS(list("itemsList" = itemsListObjects,
    #                    "reportItems" = NULL),
    #               file.path(targetPathResults, "session.rds"))
    #     }
    #
    #     log4r::info(logger, glue::glue("Shiny app has been created in {targetPath}"))
    #     shinyjs::html("createAppOutput", glue::glue("<br>Shiny app created in {targetPath}"), add = TRUE)
    #   } else {
    #     log4r::info(logger, "No files have been uploaded, app not created.")
    #     shinyjs::html("createAppOutput", glue::glue("<br>Please upload files before generating the app"), add = TRUE)
    #   }
    # })

  }
  shinyApp(ui, server)
}
