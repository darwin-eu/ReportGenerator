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
  uploaded_files <- readRDS(here::here("results", "uploaded_files.rds"))
  dataReport <- reactiveValues(objects = NULL)
  # dataReport$objects <- sessionItems$reportItems

  # Item preview
  # Item preview
  itemsList <- list()
  for (i in 1:length(uploaded_files)) {
    # i <- 1
    if (!is.null(uploaded_files[[i]])) {
      item <- names(uploaded_files[i])
      itemsList <- c(itemsList, getItemsList(item)) %>% unlist()
    }
  }

  # Renders the objectSelection into the main dashboard space
  output$navPanelPreview <- renderUI({
    previewPanels <- lapply(itemsList,
                            tabPanelSelection,
                            uploaded_files = uploaded_files)
    do.call(navlistPanel, c(previewPanels, list(widths = c(3, 9))))
  })

  # 2.Assign Data

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

  dataPatterns <- pathwaysServer("Treatment Pathways",
                                 reactive(uploaded_files$treatment_pathways))

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

  output$monitorReportItems <- renderPrint({
    reportItems <- rev(reactiveValuesToList(do.call(reactiveValues, dataReport$objects)))
    reportItems
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
                     logger,
                     reportApp = TRUE)
      shinyjs::enable("generateReport")
      shinycssloaders::hidePageSpinner()
    }
  )
}
