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
#' @import dplyr shiny shinydashboard shinyWidgets shinycssloaders officer ggplot2 flextable waldo readr yaml TreatmentPatterns PatientProfiles ggplot2
#' @importFrom sortable bucket_list add_rank_list sortable_options
#' @importFrom utils read.csv tail unzip
#' @importFrom gto body_add_gt
#' @importFrom here here
#' @importFrom TreatmentPatterns createSankeyDiagram
#' @importFrom IncidencePrevalence plotIncidence plotPrevalence
#' @importFrom DT renderDT DTOutput
#' @importFrom stringr str_split
#' @importFrom ParallelLogger addDefaultConsoleLogger
#' @importFrom cli cli_progress_step
#' @importFrom glue glue
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_replace_all str_to_title
#' @export
reportGenerator <- function(logger = NULL) {

  logger <- ParallelLogger::addDefaultConsoleLogger()
  cli::cli_progress_step("Launching ReportGenerator", spinner = TRUE)
  # set global options
  options(shiny.maxRequestSize = 1000*1024^2, spinner.type = 5, spinner.color = "#0dc5c1",
          page.spinner.type = 5, page.spinner.color = "#0dc5c1")

  header <- glue::glue("ReportGenerator {packageVersion('ReportGenerator')}")

  ui <- dashboardPage(
    dashboardHeader(title = tags$div(
      header,
      style = "font-size: 16px;" # Adjust the font size here
    )),
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
          checkboxInput("enableReporting", "Add reporting option", value = TRUE)
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
                 ),
                 fluidRow(
                   h2("Monitor"),
                   verbatimTextOutput("monitorReportItems")
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
    uploaded_files <- reactiveValues(attrition = NULL,
                                     incidence = NULL,
                                     prevalence = NULL,
                                     summarise_characteristics = NULL,
                                     summarise_large_scale_characteristics = NULL,
                                     single_event = NULL,
                                     competing_risk = NULL,
                                     treatment_pathways = NULL)
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
      items <- analysisNamesAvailable(settingsData = settingsData())
      if (!is.null(uploadedData()$other_result)) {
        items <- c(items, names(uploadedData()$other_result))
      }
      itemsList$objects[["items"]] <- getItemsList(items)

      if ("incidence" %in% items) {
        uploaded_files$incidence <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "incidence")
        uploaded_files$incidence_attrition <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "incidence_attrition")
      }
      if ("prevalence" %in% items) {
        uploaded_files$prevalence <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "prevalence")
        uploaded_files$prevalence_attrition <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "prevalence_attrition")
      }
      if ("summarise_characteristics" %in% items) {
        uploaded_files$summarise_characteristics <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "summarise_characteristics")
      }
      if ("summarise_large_scale_characteristics" %in% items) {
        uploaded_files$summarise_large_scale_characteristics <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(result_type == "summarise_large_scale_characteristics")
      }
      if ("single_event" %in% items) {
        uploaded_files$single_event <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(analysis_type == "single_event")
      }
      if ("competing_risk" %in% items) {
        uploaded_files$competing_risk <- uploadedData()$summarised_result %>%
          visOmopResults::filterSettings(analysis_type == "competing_risk")
      }
      if ("TreatmentPatterns" %in% items) {
        uploaded_files$treatment_pathways <- uploadedData()$other_result$TreatmentPatterns$treatmentPathways
      }

      # # Generate verbatim text output dynamically based on user input
      # output$monitorData <- renderPrint({
      #   # reactiveValuesToList(uploaded_files)
      #   # uploaded_files[["CohortCharacteristics"]]
      #   uploaded_files
      #
      # })

      # Get list of items to show in toggle menu
      # for (pkgName in pkgNames) {
      #   # pkgName <- "IncidencePrevalence"
      #   pkgDataList <- uploaded_files[[pkgName]]
      #   items <- names(pkgDataList)
      #   itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], getItemsList(pkgNames))
      # }
      shinycssloaders::hidePageSpinner()
    })

    # Reset and back to initial tab
    observeEvent(input$resetData, {
      itemsList$objects <- NULL
      uploaded_files <- reactiveValues(attrition = NULL,
                                      incidence = NULL,
                                      summarise_characteristics = NULL,
                                      summarise_large_scale_characteristics = NULL,
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
                              uploaded_files = uploaded_files)
      do.call(navlistPanel, c(previewPanels, list(widths = c(3, 9))))
    })

    # 2.Assign Data

    dataReport <- reactiveValues()

    # Attrition table

    incidenceAttritionTable <- attritionServer(id = "Incidence Attrition",
                                               uploaded_files = reactive(uploaded_files$incidence_attrition))

    observe({
      for (key in names(incidenceAttritionTable())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- incidenceAttritionTable()
      }
    }) %>%
      bindEvent(incidenceAttritionTable())

    prevalenceAttritionTable <- attritionServer(id = "Prevalence Attrition",
                                                uploaded_files = reactive(uploaded_files$prevalence_attrition))

    observe({
      for (key in names(prevalenceAttritionTable())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- prevalenceAttritionTable()
      }
    }) %>%
      bindEvent(prevalenceAttritionTable())

    # Incidence Modules

      # Incidence

    dataIncidence <- incidencePrevalenceServer(id = "Incidence", reactive(uploaded_files$incidence))

    observe({
      for (key in names(dataIncidence())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataIncidence()
      }
    }) %>%
      bindEvent(dataIncidence())

    # Prevalence Modules

    dataPrevalence <- incidencePrevalenceServer(id = "Prevalence", reactive(uploaded_files$prevalence))

    observe({
      for (key in names(dataPrevalence())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPrevalence()
      }
    }) %>%
      bindEvent(dataPrevalence())

    # Characteristics Modules
    dataCharacteristics <- characteristicsServer("characteristics",
                                                 reactive(uploaded_files$summarise_characteristics))

    observe({
      for (key in names(dataCharacteristics())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataCharacteristics()
      }
    }) %>%
      bindEvent(dataCharacteristics())

    dataLSC <- largeScaleServer(id = "lsc",
                                reactive(uploaded_files$summarise_large_scale_characteristics))

    observe({
      for (key in names(dataLSC())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataLSC()
      }
    }) %>%
      bindEvent(dataLSC())

    # Cohort Survival Modules

    dataSingleEvent <- cohortSurvivalServer("single_event", reactive(uploaded_files$single_event))

    observe({
      for (key in names(dataSingleEvent())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataSingleEvent()
      }
    }) %>%
      bindEvent(dataSingleEvent())

    dataCompetingRisk <- cohortSurvivalServer("competing_risk", reactive(uploaded_files$competing_risk))

    observe({
      for (key in names(dataCompetingRisk())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataCompetingRisk()
      }
    }) %>%
      bindEvent(dataCompetingRisk())

    # Treatment Patterns Interactive Plots

    dataPatterns <- pathwaysServer("Treatment Pathways", reactive(uploaded_files$treatment_pathways))

    observe({
      for (key in names(dataPatterns())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataPatterns()
      }
    }) %>%
      bindEvent(dataPatterns())

    # Data Report Preview

    objectsListPreview <- reactive({
      if (is.null(dataReport$objects)) {
        return("None")
      } else {
        dataReportList <- reactiveValuesToList(do.call(reactiveValues, dataReport$objects))
        result <- data.frame(`Result Type` = character(0), caption = character(0))
        for (i in seq(1:length(dataReportList))) {
          name <- names(dataReportList[[i]])
          name_normalised <- name %>%
            stringr::str_replace_all("_", " ") %>%
            stringr::str_to_title()
          reportItem <- dataReportList[[i]][[name]]
          caption <- ""
          if ("caption" %in% names(reportItem)) {
            caption <- reportItem$caption
          }
          caption <- ifelse(is.null(caption), "", caption)
          result <- rbind(result, data.frame(name = name_normalised, caption = reportItem$caption))
        }
        return(result)
      }
    })

    output$monitorReportItems <- renderPrint({
      reportItems <- rev(reactiveValuesToList(do.call(reactiveValues, dataReport$objects)))
      reportItems
    })

    output$dataReportMenu <- renderDT({
      dataReportFrame <- objectsListPreview()
      if (inherits(dataReportFrame, "data.frame")) {
        DT::datatable(dataReportFrame, options = list(pageLength = 100, dom = 't'))
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
        # Generates report iterating through the reportItems
        generateReport(reportDocx = reportDocx,
                       dataReportList = reportItems,
                       fileName = file,
                       logger = logger,
                       reportApp = FALSE)
        shinyjs::enable("generateReport")
        shinycssloaders::hidePageSpinner()
      }
    )

    # save report
    output$downloadStudyData <- downloadHandler(
      filename = "uploaded_files.rds",
      content = function(file) {
        shinycssloaders::showPageSpinner()
        if (!is.null(uploaded_files)) {
          saveRDS(reactiveValuesToList(uploaded_files),
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
                       "uploaded_files" = reactiveValuesToList(uploaded_files),
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
      upFiles <- reportData$uploaded_files
      uploaded_files$CohortSurvival <- upFiles$CohortSurvival
      uploaded_files$IncidencePrevalence <- upFiles$IncidencePrevalence
      uploaded_files$CohortCharacteristics <- upFiles$CohortCharacteristics
      uploaded_files$TreatmentPatterns <- upFiles$TreatmentPatterns
      shinyjs::html("reportOutput", "<br>Loaded report items from rds file", add = TRUE)
    })

    # save report
    output$createReportApp <- downloadHandler(
      filename = "reportApp.zip",
      content = function(file) {
        shinycssloaders::showPageSpinner()
        # log4r::info(logger, paste("Create shiny application, reportingEnabled = ", input$enableReporting))
        cli::cli_h2("Creating Shiny App")
        uploaded_files <- reactiveValuesToList(uploaded_files)
        uploaded_filesValues <- unlist(lapply(names(uploaded_files), FUN = function(name) {uploaded_files[[name]]}))
        if (!is.null(uploaded_filesValues)) {

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
          log4r::info(logger, "Add uploaded_files and session results")
          saveRDS(uploaded_files,
                  file.path(targetPathResults, "uploaded_files.rds"))

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
