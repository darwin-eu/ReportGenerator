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
  # Item preview
  itemsList <- list()
  for (i in 1:length(uploadedFiles)) {
    # i <- 1
    if (!is.null(uploadedFiles[[i]])) {
      item <- names(uploadedFiles[i])
      itemsList <- c(itemsList, getItemsList(item)) %>% unlist()
    }
  }

  # Renders the objectSelection into the main dashboard space
  output$navPanelPreview <- renderUI({
    previewPanels <- lapply(itemsList,
                            tabPanelSelection,
                            uploadedFiles = uploadedFiles)
    do.call(navlistPanel, c(previewPanels, list(widths = c(3, 9))))
  })

  # 2.Assign Data

  # Table w/ attrition data from Inc/Prev


  attritionServer(id = "Incidence Attrition",
                  uploadedFiles = reactive(uploadedFiles$incidence_attrition))

  attritionServer(id = "Prevalence Attrition",
                  uploadedFiles = reactive(uploadedFiles$prevalence_attrition))

  # Incidence Modules

  # Incidence

  dataIncidence <- incidenceServer(id = "Incidence", reactive(uploadedFiles$incidence))

  observe({
    for (key in names(dataIncidence())) {
      randomId <- getRandomId()
      dataReport[["objects"]][[randomId]] <- dataIncidence()
    }
  }) %>%
    bindEvent(dataIncidence())

  # Prevalence Modules

  dataPrevalence <- prevalenceServer(id = "Prevalence", reactive(uploadedFiles$prevalence))

  observe({
    for (key in names(dataPrevalence())) {
      randomId <- getRandomId()
      dataReport[["objects"]][[randomId]] <- dataPrevalence()
    }
  }) %>%
    bindEvent(dataPrevalence())

  # Characteristics Modules
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
                                   reactive(uploadedFiles$summarised_large_scale_characteristics))

  observe({
    for (key in names(dataLSC())) {
      randomId <- getRandomId()
      dataReport[["objects"]][[randomId]] <- dataLSC()
    }
  }) %>%
    bindEvent(dataLSC())

  # Cohort Survival Modules

  dataSingleEvent <- cohortSurvivalServer("single_event", reactive(uploadedFiles$single_event))

  observe({
    for (key in names(dataSingleEvent())) {
      randomId <- getRandomId()
      dataReport[["objects"]][[randomId]] <- dataSingleEvent()
    }
  }) %>%
    bindEvent(dataSingleEvent())

  dataCompetingRisk <- cohortSurvivalServer("competing_risk", reactive(uploadedFiles$competing_risk))

  observe({
    for (key in names(dataCompetingRisk())) {
      randomId <- getRandomId()
      dataReport[["objects"]][[randomId]] <- dataCompetingRisk()
    }
  }) %>%
    bindEvent(dataCompetingRisk())

  # Treatment Patterns Interactive Plots

  dataPatterns <- patternsServer("Treatment Pathways",
                                 reactive(uploadedFiles$treatment_pathways))

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
