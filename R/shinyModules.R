datasetLoadUI <- function(id) {

  uiOutput(NS(id, "datasetLoad"))
}


datasetLoadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
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
