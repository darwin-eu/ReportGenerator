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
    lockName <- "lockLSC"
  } else {
    lockName <- "lockSummary"
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
      column(4,
             actionButton(ns(lockName), "Add item to report")
             )
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

characteristicsServer <- function(id, dataset, dataReport) {
  moduleServer(id, function(input, output, session) {
    dataPP <- reactive({
      dataset %>% filter(cdm_name %in% input$cdm_name,
                         group_level %in% input$group_level,
                         strata_name %in% input$strata_name,
                         variable %in% input$variable,
                         estimate_type %in% input$estimate_type) %>%
      select(cdm_name, group_level, strata_name, variable, variable_level , estimate_type, estimate) %>%
      mutate(estimate = ifelse(estimate_type == "percentage", round(as.numeric(estimate), 2), estimate))
      })
    output$dt_summary <- DT::renderDataTable(server = FALSE, {
      createDataTable(dataPP())
    })

    observeEvent(input$lockSummary, {
      objectChoice <- "Summary Characteristics"
      chars <- c(0:9, letters, LETTERS)
      randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
      dataReport[[randomId]][[objectChoice]] <- dataPP()
    })
  })
}

# lockItemsUI <- function(id) {
#
#   actionButton(NS(id, "lockItem"), "Add item to report")
#
# }
#
# lockItemsServer <- function(id,
#                             dataReport,
#                             prevalenceAttrition,
#                             incidenceAttrition,
#                             incidenceEstimates,
#                             captionInput) {
#   moduleServer(id, function(input, output, session) {
#     if (id == "lockTableNumPar") {
#       observeEvent(input$lockItem, {
#         objectChoice <- "Table - Number of participants"
#         chars <- c(0:9, letters, LETTERS)
#         randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
#         dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttrition
#         dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttrition
#         dataReport[[randomId]][[objectChoice]][["caption"]] <- captionInput
#       })
#     } else if (id == "lockTableIncAtt") {
#       observeEvent(input$lockItem, {
#         objectChoice <- "Table - Incidence Attrition"
#         attritionDataType <- "incidence"
#         chars <- c(0:9, letters, LETTERS)
#         randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
#         dataReport[[randomId]][[objectChoice]][["incidence_attrition"]] <- incidenceAttrition
#         dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
#         dataReport[[randomId]][[objectChoice]][["caption"]] <- captionInput
#       })
#     } else if (id == "lockTablePrevAtt") {
#       observeEvent(input$lockItem, {
#         objectChoice <- "Table - Prevalence Attrition"
#         attritionDataType <- "prevalence"
#         chars <- c(0:9, letters, LETTERS)
#         randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
#         dataReport[[randomId]][[objectChoice]][["prevalence_attrition"]] <- prevalenceAttrition
#         dataReport[[randomId]][[objectChoice]][["attritionDataType"]] <- attritionDataType
#         dataReport[[randomId]][[objectChoice]][["caption"]] <- captionInput
#         })
#     } else if (id == "lockTableSex") {
#       observeEvent(input$lockItem, {
#         objectChoice <- "Table - Number of participants by sex and age group"
#         chars <- c(0:9, letters, LETTERS)
#         randomId <- stringr::str_c(sample(chars, 4, replace = TRUE) , collapse = "" )
#         dataReport[[randomId]][[objectChoice]][["incidence_estimates"]] <- incidenceEstimates
#         dataReport[[randomId]][[objectChoice]][["caption"]] <- captionInput
#       })
#     }
#     })
# }
