datasetLoadUI <- function(id) {

  uiOutput(NS(id, "datasetLoad"))
}


datasetLoadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    configData <- yaml.load_file(system.file("config",
                                             "variablesConfig.yaml",
                                             package = "ReportGenerator"))
    if (id == "IncidencePrevalence") {
      datasetLoad <- "datasetLoad"
      inputId <- "dataVersion"
      accept <- c(".zip", ".csv")
      placeholder <- "ZIP or CSV"
    } else if (id == "TreatmentPatterns") {
      datasetLoad <- "datasetLoadTP"
      inputId <- "dataVersionTP"
      accept <- c(".zip", ".csv")
      placeholder <- "ZIP or CSV"
    } else if (id == "PatientProfiles") {
      datasetLoad <- "datasetLoadPP"
      inputId <- "dataVersionPP"
      accept <- c(".zip", ".csv")
      placeholder <- "ZIP or CSV"
    } else if (id == "CohortSurvival") {
      datasetLoad <- "datasetLoadCS"
      inputId <- "dataVersionCS"
      accept <- c(".zip", ".csv")
      placeholder <- "ZIP or CSV"
    }
    output$datasetLoad <- renderUI({
      tagList(tags$div(tags$h4("Load results"), class = "form-group shiny-input-container"),
              selectInput(inputId = inputId,
                          label = "Select version",
                          choices = gtools::mixedsort(names(configData[[id]]), decreasing = TRUE),
                          selected = gtools::mixedsort(names(configData[[id]]), decreasing = TRUE)[1]),
              fileInput(datasetLoad,
                        "Upload your files",
                        accept = accept,
                        multiple = TRUE,
                        placeholder = placeholder)
      )
    })
  })
}

characteristicsUI <- function(id, dataset) {
  ns <- NS(id)
  if (id == "characteristics") {
    lockName <- "lockSummary"
    captionText <- "Table 1. Baseline characteristics of drug user/s at the time of therapy initiation, including pre-specified indication/s. Number of participants per pre-specified strata will be included where necessary/applicable"
  } else {
    lockName <- "lockLSC"
    captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
  }
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "Database",
                         choices = unique(dataset$cdm_name),
                         selected = unique(dataset$cdm_name),
                         multiple = TRUE)
              ),
      column(4,
             pickerInput(inputId = ns("group_level"),
                         label = "Group Level",
                         choices = unique(dataset$group_level),
                         selected = unique(dataset$group_level),
                         multiple = TRUE)
             )
      ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("strata_name"),
                         label = "Strata Name",
                         choices = unique(dataset$strata_name),
                         selected = unique(dataset$strata_name),
                         multiple = TRUE)
             ),
      column(4,
             pickerInput(inputId = ns("variable"),
                         label = "Variable",
                         choices = sort(unique(dataset$variable)),
                         selected = unique(dataset$variable),
                         multiple = TRUE)
             )
      ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("estimate_type"),
                         label = "Estimate Type",
                         choices = sort(unique(dataset$estimate_type)),
                         selected = sort(unique(dataset$estimate_type)),
                         multiple = TRUE)
             )
      ),
    fluidRow(
      column(8,
             textAreaInput(inputId = ns("captionCharacteristics"),
                           label = "Caption",
                           value = captionText,
                           width = '100%',
                           height = "130px")
      ),
    ),
    fluidRow(
      column(4,
             actionButton(ns(lockName), "Add item to report")
             )
      ),
    tags$br(),
    fluidRow(
      column(12,
             DT::dataTableOutput(ns("summarisedTable"))
      )
    )
  )
}

createDataTable <- function(data, tableName = "result") {
  DT::datatable(data,
                extensions = 'Buttons',
                options = list(pageLength = 10,
                               paging = TRUE,
                               searching = TRUE,
                               fixedColumns = TRUE,
                               autoWidth = TRUE,
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               buttons =
                                 list(list(
                                   extend = "collection",
                                   buttons = list(
                                     list(extend = "csv", title = tableName),
                                     list(extend = "excel", title = tableName)),
                                   text = "Download"
                                 ))),
                class = "display")
}

characteristicsServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {

    if (id == "characteristics") {
      dataPP <- reactive({
        dataset() %>% filter(cdm_name %in% input$cdm_name,
                           group_level %in% input$group_level,
                           strata_name %in% input$strata_name,
                           variable %in% input$variable,
                           estimate_type %in% input$estimate_type) %>%
        # select(cdm_name, group_level, strata_name, variable, variable_level , estimate_type, estimate) %>%
        mutate(estimate = ifelse(estimate_type == "percentage", round(as.numeric(estimate), 2), estimate))
        })
    } else {
      dataPP <- reactive({
        dataset() %>% filter(cdm_name %in% input$cdm_name,
                             group_level %in% input$group_level,
                             strata_name %in% input$strata_name,
                             variable %in% input$variable,
                             estimate_type %in% input$estimate_type) %>%
          # select(cdm_name, group_level, strata_name, variable, variable_level, estimate_type, estimate) %>%
          mutate(estimate = ifelse(estimate_type == "percentage", round(as.numeric(estimate), 2), estimate))
      })
    }


    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(dataPP())
    })

    addObject <- reactiveVal()

    observeEvent(input$lockSummary, {
      addObject(
        list(`Summarised Characteristics` = list(summarisedCharacteristics = dataPP(),
                                                 caption = input$captionCharacteristics))
      )
    })

    observeEvent(input$lockLSC, {
      addObject(
        list(`Summarised Large Scale Characteristics` = list(summarisedCharacteristics = dataPP(),
                                                             caption = input$captionCharacteristics))
      )
    })
    addObject
  })
}
