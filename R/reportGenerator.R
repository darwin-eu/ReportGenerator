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
#' @importFrom glue glue
#' @export
reportGenerator <- function(logger = NULL) {

  logger <- ParallelLogger::addDefaultConsoleLogger()
  cli::cli_progress_step("Launching ReportGenerator", spinner = TRUE)
  # set global options
  options(shiny.maxRequestSize = 1000*1024^2, spinner.type = 5, spinner.color = "#0dc5c1",
          page.spinner.type = 5, page.spinner.color = "#0dc5c1")

  header <- glue::glue("ReportGenerator {packageVersion('ReportGenerator')}")

  ui <- dashboardPage(
    dashboardHeader(title = header),
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
    uploadedFiles <- reactiveValues(attrition = NULL,
                                    incidence = NULL,
                                    summarised_characteristics = NULL,
                                    summarised_large_scale_characteristics = NULL,
                                    single_event = NULL)
    itemsList <- reactiveValues(objects = NULL)
    uploadedData <- reactiveVal(NULL)
    settingsData <- reactiveVal(NULL)

    # Check input data
    observeEvent(input$datasetLoad, {
      shinycssloaders::showPageSpinner()

      # Read  file paths
      inFile <- input$datasetLoad
      fileDataPath <- inFile$datapath
      fileName <- inFile$name

      # Joins one or several zips into the reactive value
      # fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\latest\\p3-c1-010-results 2"
      # fileDataPath <- list.files(fileDataPath, full.names = TRUE, pattern = ".zip")
      # fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\ild"
      # fileDataPath <- list.files(fileDataPath, full.names = TRUE, pattern = ".zip")
      # fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\ild\\survival\\cdw_survival_results.csv"
      # fileDataPath <- list.files(fileDataPath, full.names = TRUE, pattern = ".zip")

      tryCatch({
        data_joined <- joinDatabases(fileDataPath = fileDataPath)
        uploadedData(data_joined)
        # Now access and store the settings data if uploadedData is not NULL
        if (!is.null(data_joined$summarised_result)) {
          settingsData(settings(data_joined$summarised_result))  # Set the settingsData reactive value
        } else {
          settingsData(NULL)  # Ensure settingsData is reset if summarised_result is NULL
        }
      }, error = function(e) {
        shinyalert::shinyalert("Error",
                               glue::glue("An error occurred: {e$message}"),type = "error")
        uploadedData(NULL)  # Set uploadedData to NULL in case of error
        settingsData(NULL)   # Reset settingsData in case of error
      })
      if (is.null(uploadedData()) || length(uploadedData()) == 0) {
        shinyalert::shinyalert("Data mismatch",
                   "No valid package files found", type = "warning")
        shinycssloaders::hidePageSpinner()
        return()  # Exit the observeEvent if no valid data is found
      }

      req(settingsData())
      items <- analysisNames(settingsData = settingsData())
      itemsList$objects[["items"]] <- getItemsList(items)

      if ("incidence_attrition" %in% items) {
        uploadedFiles$attrition <- uploadedData()$other_result$IncidencePrevalence$attrition
      }
      if ("incidence" %in% items) {
        uploadedFiles$incidence <- getSummarisedData(uploadedData = uploadedData()$summarised_result,
                                                     type_result = "incidence")
      }
      if ("summarised_characteristics" %in% items) {
        uploadedFiles$summarised_characteristics <- getSummarisedData(uploadedData = uploadedData()$summarised_result,
                                                                      type_result = "summarised_characteristics")
      }
      if ("summarised_large_scale_characteristics" %in% items) {
        uploadedFiles$summarised_large_scale_characteristics <- getSummarisedData(uploadedData = uploadedData()$summarised_result,
                                                                                  type_result = "summarised_large_scale_characteristics")
      }
      if ("single_event" %in% items) {
        uploadedFiles$single_event <- getSummarisedData(uploadedData = uploadedData()$summarised_result,
                                                        type_result = "survival")
      }
      # if ("TreatmentPatterns" %in% pkgNames) {
      #   uploadedFiles$TreatmentPatterns <- uploadedFilesList[["TreatmentPatterns"]]
      # }
      # if ("CohortSurvival" %in% pkgNames) {
      #   uploadedFiles$CohortSurvival <- uploadedFilesList[["CohortSurvival"]]
      # }
      # if ("cohortAttrition" %in% pkgNames) {
      #   uploadedFiles$cohortAttrition <- uploadedFilesList[["cohortAttrition"]]
      # }

      # # Generate verbatim text output dynamically based on user input
      # output$monitorData <- renderPrint({
      #   # reactiveValuesToList(uploadedFiles)
      #   # uploadedFiles[["CohortCharacteristics"]]
      #   uploadedFiles
      #
      # })

      # Get list of items to show in toggle menu
      # for (pkgName in pkgNames) {
      #   # pkgName <- "IncidencePrevalence"
      #   pkgDataList <- uploadedFiles[[pkgName]]
      #   items <- names(pkgDataList)
      #   itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], getItemsList(pkgNames))
      # }
      shinycssloaders::hidePageSpinner()
    })

    # Reset and back to initial tab
    observeEvent(input$resetData, {
      itemsList$objects <- NULL
      uploadedFiles <- reactiveValues(attrition = NULL,
                                      incidence = NULL,
                                      summarised_characteristics = NULL,
                                      summarised_large_scale_characteristics = NULL,
                                      single_event = NULL)
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
                                       options = sortable_options(multiDrag = TRUE))))
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

    # Attrition table

    # attritionTable <- attritionServer(id = "Incidence Attrition",
    #                                   uploadedFiles = reactive(uploadedFiles$attrition))
    #
    # observe({
    #   for (key in names(attritionTable())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- attritionTable()
    #   }
    # }) %>%
    #   bindEvent(attritionTable())


    # Inc/Prev Table Modules

    # tableNumPar <- attritionIncPrevServer(id = "Incidence Attrition",
    #                                       uploadedFiles = reactive(uploadedFiles$attrition))
    #
    # observe({
    #   for (key in names(tableNumPar())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- tableNumPar()
    #   }
    # }) %>%
    #   bindEvent(tableNumPar())
    #
    # # Attrition Incidence only
    #
    # tableAttInc <- attritionIncPrevServer(id = "Incidence Attrition - Table",
    #                                       uploadedFiles = reactive(uploadedFiles))
    #
    # observe({
    #   for (key in names(tableAttInc())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- tableAttInc()
    #   }
    # }) %>%
    #   bindEvent(tableAttInc())

    # Attrition Prevelence only

    # tableAttPrev <- attritionIncPrevServer(id = "Prevalence Attrition - Table",
    #                                        uploadedFiles = reactive(uploadedFiles))
    #
    # observe({
    #   for (key in names(tableAttPrev())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- tableAttPrev()
    #   }
    # }) %>%
    #   bindEvent(tableAttPrev())

    # Table Sex/Age

    # tableSexAge <- tableServer(id = "Table - Number of participants by sex and age group",
    #                            reactive(uploadedFiles))
    #
    # observe({
    #   for (key in names(tableSexAge())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- tableSexAge()
    #   }
    # }) %>%
    #   bindEvent(tableSexAge())


    # Incidence Modules

      # Incidence

    incidenceSumServer(id = "Incidence", reactive(uploadedFiles$incidence))

    # dataIncidenceYear <- incidenceSumServer(id = "Incidence rate per year - Plot",
    #                                      reactive(uploadedFiles$incidence))

    # observe({
    #   for (key in names(dataIncidenceYear())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- dataIncidenceYear()
    #   }
    # }) %>%
    #   bindEvent(dataIncidenceYear())

    # Prevalence Modules

      # Year

    # dataPrevalenceYear <- prevalenceServer(id = "Prevalence per year - Plot",
    #                                        reactive(uploadedFiles))
    #
    # observe({
    #   for (key in names(dataPrevalenceYear())) {
    #     randomId <- getRandomId()
    #     dataReport[["objects"]][[randomId]] <- dataPrevalenceYear()
    #   }
    # }) %>%
    #   bindEvent(dataPrevalenceYear())


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
                                                 reactive(uploadedFiles$summarised_characteristics))

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

    dataSurvivalTable <- cohortSurvivalServer("survivalTable", reactive(uploadedFiles$single_event))

    observe({
      for (key in names(dataSurvivalTable())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataSurvivalTable()
      }
    }) %>%
      bindEvent(dataSurvivalTable())

    dataSurvivalPlot <- cohortSurvivalServer("survivalPlot", reactive(uploadedFiles$single_event))

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
          modules <- modulesFiles[!grepl("sysdata\\.rda$", modulesFiles)]
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

  }
  shinyApp(ui, server)
}
