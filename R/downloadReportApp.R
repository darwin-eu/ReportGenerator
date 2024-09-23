downloadReportApp <- function(reportItems, logger = NULL) {

  # set global options
  options(shiny.maxRequestSize = 1000*1024^2, spinner.type = 5, spinner.color = "#0dc5c1",
          page.spinner.type = 5, page.spinner.color = "#0dc5c1")

  ui <- dashboardPage(
    dashboardHeader(title = glue::glue("ReportGenerator {packageVersion('ReportGenerator')}")),
    dashboardSidebar(
      sidebarMenu(
        # uiOutput("navPanelPreview")
        # menuItem("StudyPackage", datasetLoadUI("StudyPackage"),
        #          startExpanded = TRUE),
        # tags$br(),
        # actionButton('resetData', 'Reset data'),
        # tags$br(), tags$br(), tags$br(),
        # shinyjs::useShinyjs(),
        # tags$head(tags$style(".dlStudyDataBtn{ margin-left:15px;margin-right:15px; color:#444 !important; }")),
        # downloadButton("downloadStudyData", "Sample dataset", class = "dlStudyDataBtn")
      )
    ),
    dashboardBody(
      tabsetPanel(
        id = "mainPanel",
        # tabPanel("Item selection",
        #          fluidRow(
        #            box(uiOutput("itemSelectionMenu"),
        #                tags$br())
        #          )),
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
                          h2("Report items"),
                          DTOutput("dataReportMenu"),
                          # verbatimTextOutput("dataReportMenu"),
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

  server <- function(input, output, session) {

    # create logger
    if (is.null(logger)) {
      log_file <- glue::glue("log.txt")
      logger <- log4r::logger(threshold = "INFO", appenders = list(log4r::console_appender(), log4r::file_appender(log_file)))
    }
    log4r::info(logger, "Start ReportGenerator")

    # 1. Load data
    datasetLoadServer("StudyPackage")

    # ReactiveValues

    reportItems <- readRDS(here::here("ReportApp", "reportItems.rds"))

    uploadedFiles <- reportItems$uploadedFiles
    itemsList <- reactiveValues(objects = NULL)

    # uploadedFiles <- reactiveValues(IncidencePrevalence = NULL,
    #                                 TreatmentPatterns = NULL,
    #                                 CohortCharacteristics = NULL,
    #                                 CohortSurvival = NULL)
    # itemsList <- reactiveValues(objects = NULL)

    # # Check input data
    # observeEvent(input$datasetLoad, {
    #   shinycssloaders::showPageSpinner()
    #
    #   # Read  file paths
    #   inFile <- input$datasetLoad
    #   fileDataPath <- inFile$datapath
    #   fileName <- inFile$name
    #   # Temp directory to unzip files
    #   csvLocation <- file.path(tempdir(), "dataLocation")
    #   dir.create(csvLocation)
    #   # Joins one or several zips into the reactive value
    #   uploadedFiles <- joinDatabases(fileDataPath = fileDataPath,
    #                                        fileName = fileName,
    #                                        csvLocation = csvLocation,
    #                                        logger = logger)
    #   if (length(uploadedFiles) == 0) {
    #     show_alert(title = "Data mismatch",
    #                text = "No valid package files found")
    #   }
    #   pkgNames <- names(uploadedFiles)
    #   if ("IncidencePrevalence" %in% pkgNames) {
    #     uploadedFiles$IncidencePrevalence <- uploadedFiles[["IncidencePrevalence"]]
    #   }
    #   if ("TreatmentPatterns" %in% pkgNames) {
    #     uploadedFiles$TreatmentPatterns <- uploadedFiles[["TreatmentPatterns"]]
    #   }
    #   if ("PatientProfiles" %in% pkgNames) {
    #     uploadedFiles$CohortCharacteristics <- uploadedFiles[["PatientProfiles"]]
    #   }
    #   if ("CohortSurvival" %in% pkgNames) {
    #     uploadedFiles$CohortSurvival <- uploadedFiles[["CohortSurvival"]]
    #   }
    #
    #   # Get list of items to show in toggle menu
    #   for (pkgName in pkgNames) {
    #     pkgDataList <- uploadedFiles[[pkgName]]
    #     items <- names(pkgDataList)
    #     itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], getItemsList(items))
    #   }
    #   unlink(csvLocation, recursive = TRUE)
    #   shinycssloaders::hidePageSpinner()
    # })
    #
    # # Reset and back to initial tab
    # observeEvent(input$resetData, {
    #   itemsList$objects <- NULL
    #   uploadedFiles <- reactiveValues(IncidencePrevalence = NULL,
    #                                   TreatmentPatterns = NULL,
    #                                   CohortCharacteristics = NULL)
    #   updateTabsetPanel(session, "mainPanel",
    #                     selected = "Item selection")
    #   datasetLoadServer("StudyPackage")
    #   dataReport$objects <- NULL
    #   shinyjs::html("reportOutput", "")
    # })

    # 1. Interactive menu

    # output$itemSelectionMenu <- renderUI({
    #   column(tags$b("Item selection"),
    #          width = 12,
    #          bucket_list(header = "Select the figures you want in the report",
    #                      group_name = "bucket_list_group",
    #                      orientation = "horizontal",
    #                      add_rank_list(text = "Drag from here",
    #                                    labels = itemsList$objects[["items"]],
    #                                    input_id = "objectMenu",
    #                                    options = sortable_options(multiDrag = TRUE)),
    #                      add_rank_list(text = "to here",
    #                                    labels = NULL,
    #                                    input_id = "objectSelection",
    #                                    options = sortable_options(multiDrag = TRUE))
    #          )
    #   )
    # })

    # Item preview

    objectSelection <- reportItems$itemsList$items

    # Renders the objectSelection into the main dashboard space
    output$navPanelPreview <- renderUI({
      previewPanels <- lapply(objectSelection,
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
                                                 reactive(uploadedFiles$CohortCharacteristics$summarised_characteristics))

    observe({
      for (key in names(dataCharacteristics())) {
        randomId <- getRandomId()
        dataReport[["objects"]][[randomId]] <- dataCharacteristics()
      }
    }) %>%
      bindEvent(dataCharacteristics())

    dataLSC <- characteristicsServer(id = "lsc",
                                     reactive(uploadedFiles$CohortCharacteristics$summarised_large_scale_characteristics))

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
      }
    )

    # download sample study data
    output$downloadStudyData <- downloadHandler(
      filename = function() { "StudyResults.zip" },
      content = function(file) {
        file.copy(system.file("extdata/examples/StudyResults.zip", package = "ReportGenerator"), file)
      },
      contentType = "application/zip"
    )

    # save report
    output$saveReportData <- downloadHandler(
      filename = "reportItems.rds",
      content = function(file) {
        if (!is.null(dataReport$objects)) {
          shinyjs::html("reportOutput", "<br>Saving report items to rds file", add = TRUE)
          shinyjs::disable("saveReportData")
          saveRDS(list("reportItems" = reactiveValuesToList(do.call(reactiveValues, dataReport$objects)),
                       "uploadedFiles" = reactiveValuesToList(uploadedFiles),
                       "itemsList" = reactiveValuesToList(do.call(reactiveValues, itemsList$objects))),
                  file)
          shinyjs::enable("saveReportData")
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

  }
  shinyApp(ui, server)
}

