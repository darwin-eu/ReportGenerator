#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
function(input, output, session) {
  logger <- log4r::logger()
  # create logger
  if (is.null(logger)) {
    log_file <- glue::glue("log.txt")
    logger <- log4r::logger(threshold = "INFO", appenders = list(log4r::console_appender(), log4r::file_appender(log_file)))
  }
  log4r::info(logger, "Start ReportGenerator")

  # 1. Load data
  # ReactiveValues
  uploadedFiles <- readRDS(here::here("results", "uploadedFiles.rds"))
  dataReport <- reactiveValues(objects = NULL)
  # dataReport$objects <- sessionItems$reportItems

  # Item preview
  pkgNames <- names(uploadedFiles)
  itemsList <- list()
  for (pkgName in pkgNames) {
    pkgDataList <- uploadedFiles[[pkgName]]
    items <- names(pkgDataList)
    itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], getItemsList(items))
  }

  objectSelection <- itemsList$objects[["items"]]

  # Renders the objectSelection into the main dashboard space
  output$navPanelPreview <- renderUI({
    previewPanels <- lapply(objectSelection,
                            tabPanelSelection,
                            uploadedFiles = uploadedFiles)
    do.call(navlistPanel, c(previewPanels, list(widths = c(3, 9))))
  })

  # 2.Assign Data
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
      shinyjs::disable("generateReport")
      # Load template and generate report
      shinyjs::html("reportOutput", "<br>Generating report", add = TRUE)
      reportDocx <- read_docx(path = file.path(getwd(), "config", "DARWIN_EU_Study_Report.docx"))
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
}
