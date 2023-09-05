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
      accept <- c(".csv")
      placeholder <- "CSV"
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
