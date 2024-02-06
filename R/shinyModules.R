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
  if (id == "lsc") {
    selectedVariables <- unique(dataset$variable)
  } else {
    selectedVariables <- c("Number subjects", "Treatment")
  }
  tagList(
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("cdm_name"),
        label = "Database",
        choices = unique(dataset$cdm_name),
        selected = unique(dataset$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("group_level"),
        label = "Group Level",
        choices = unique(dataset$group_level),
        selected = unique(dataset$group_level)[1],
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("strata_name"),
        label = "Strata Name",
        choices = unique(dataset$strata_name),
        selected = unique(dataset$strata_name)[1],
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("variable"),
        label = "Variable",
        choices = sort(unique(dataset$variable)),
        selected = selectedVariables,
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("estimate_type"),
        label = "Estimate Type",
        choices = sort(unique(dataset$estimate_type)),
        selected = sort(unique(dataset$estimate_type)),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    fluidRow(
      column(4,
             actionButton("lockSummarisedCharacteristics", "Add item to report"))
      ),
    tags$br(),
    fluidRow(
      column(12,
             DT::dataTableOutput(ns("dt_summary"))
      )
    )
  )
}

createDataTable <- function(data, tableName = "result") {
  DT::datatable(data,
                extensions = 'Buttons',
                options = list(pageLength = 50,
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
    output$dt_summary <- DT::renderDataTable(server = FALSE, {
      createDataTable(dataset %>%
                        filter(cdm_name %in% input$cdm_name,
                               group_level %in% input$group_level,
                               strata_name %in% input$strata_name,
                               variable %in% input$variable,
                               estimate_type %in% input$estimate_type
                        ) %>%
                        select(cdm_name, group_level, strata_name, variable, variable_level , estimate_type, estimate) %>%
                        mutate(estimate = ifelse(estimate_type == "percentage", round(as.numeric(estimate), 2), estimate))

      )
    })
  })
}

# CohortSurivial
cohortSurvivalTableUI <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("cdm_name"),
        label = "Database",
        choices = unique(dataset$cdm_name),
        selected = unique(dataset$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    fluidRow(
      column(4,
             actionButton("lockSurvivalTable", "Add item to report")
      )
    ),
    tags$br(),
    fluidRow(
      column(12,
             DT::dataTableOutput(ns("cs_data"))
      )
    )
  )
}

cohortSurvivalTableServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    output$cs_data <- DT::renderDataTable(server = FALSE, {
      createDataTable(dataset)
    })
  })
}

cohortSurvivalPlotUI <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(
        inputId = ns("cdm_name"),
        label = "Database",
        choices = unique(dataset$cdm_name),
        selected = unique(dataset$cdm_name),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = TRUE
      )
    ),
    fluidRow(
      column(4,
             actionButton("lockSurvivalPlot", "Add item to report")
      )
    ),
    tags$br(),
    fluidRow(
      column(12,
             plotOutput(ns("cs_plot"))
      )
    )
  )
}

cohortSurvivalPlotServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    output$cs_plot <- renderPlot({
      CohortSurvival::plotSurvival(dataset, colour = "strata_level", facet= "strata_name")
    })
  })
}

