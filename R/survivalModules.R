cohortSurvivalUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  outResult <- NULL
  dlTable <- NULL
  dlPlot <- NULL
  dataset <- uploadedFiles %>% visOmopResults::splitAdditional()
  cdmOptions <- unique(dataset$cdm_name)
  resultIdOptions <- unique(dataset$result_id)
  groupLevelOptions <- unique(dataset$group_level)
  groupNameOptions <- unique(dataset$group_name)
  strataNameOptions <- unique(dataset$strata_name)
  strataLevelOptions <- unique(dataset$strata_level)
  variableNameOptions <- unique(dataset$variable_name)
  variableLevelOptions <- unique(dataset$variable_level)
  estimateTypeOptions <- unique(dataset$estimate_type)
  outcomeOptions <- unique(dataset$outcome)
  eventgapOptions <- unique(dataset$eventgap)
  pickerOptions <- list(`actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "count > 3")
  cdmOptions <- unique(dataset$cdm_name)

    tagList(
      fluidRow(
        column(4,
               pickerInput(
                 inputId = ns("cdm_name"),
                 label = "Database",
                 choices = cdmOptions,
                 selected = cdmOptions[1],
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("result_id"),
                 label = "Result Id",
                 choices = resultIdOptions,
                 selected = resultIdOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("group_name"),
                 label = "Group Name",
                 choices = groupNameOptions,
                 selected = groupNameOptions[1],
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("group_level"),
                 label = "Group Level",
                 choices = groupLevelOptions,
                 selected = groupLevelOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("strata_name"),
                 label = "Strata Name",
                 choices = strataNameOptions,
                 selected = strataNameOptions[1],
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("strata_level"),
                 label = "Strata Level",
                 choices = strataLevelOptions,
                 selected = strataLevelOptions[1],
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("variable_name"),
                 label = "Variable Name",
                 choices = variableNameOptions,
                 selected = variableNameOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("variable_level"),
                 label = "Variable Level",
                 choices = variableLevelOptions,
                 selected = variableLevelOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("estimate_type"),
                 label = "Estimate Level",
                 choices = estimateTypeOptions,
                 selected = estimateTypeOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("outcome"),
                 label = "Outcome",
                 choices = outcomeOptions,
                 selected = outcomeOptions,
                 options = pickerOptions,
                 multiple = TRUE
               )
        ),
        column(4,
               pickerInput(
                 inputId = ns("eventgap"),
                 label = "Event Gap",
                 choices = eventgapOptions,
                 selected = eventgapOptions[1],
                 options = pickerOptions,
                 multiple = TRUE
               )
        )
      ),
      tags$br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Table",
                           tags$br(),
                           fluidRow(
                             column(6,
                                    pickerInput(inputId = ns("pivotWide"),
                                                label = "Arrange by",
                                                choices = c("group", "strata"),
                                                selected = c("group", "strata"),
                                                multiple = TRUE)
                                    ),
                             column(6,
                                    pickerInput(inputId = ns("header"),
                                                label = "Header",
                                                choices = c("cdm_name", "group", "strata", "additional", "variable", "estimate", "settings"),
                                                selected = c("cdm_name", "estimate"),
                                                multiple = TRUE)),
                             ),
                           uiOutput(outputId = ns("caption_table")),
                           fluidRow(createAddItemToReportUI(ns("lock_table")), dlTable),
                           tags$br(),
                           fluidRow(column(12, shinycssloaders::withSpinner(gt::gt_output(ns("cs_data")))))),
                  tabPanel("Plot",
                           tags$br(),
                           uiOutput(outputId = ns("caption_plot")),
                           fluidRow(createDownloadPlotUI(ns)),
                           fluidRow(createAddItemToReportUI(ns("lock_plot")), dlPlot),
                           tags$br(),
                           fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(ns("cs_plot"))))))
                  )
      )
}

cohortSurvivalServer <- function(id, uploadedFiles) {

  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      if (id == "single_event" || id == "competing_risk") {
        uploadedFiles() %>%
          filter(cdm_name %in% input$cdm_name,
                 result_id %in% input$result_id,
                 group_name %in% input$group_name,
                 group_level %in% input$group_level,
                 strata_name %in% input$strata_name,
                 strata_level %in% input$strata_level,
                 variable_name %in% input$variable_name,
                 variable_level %in% input$variable_level,
                 estimate_type %in% input$estimate_type) %>%
          visOmopResults::filterAdditional(outcome %in% input$outcome,
                                           eventgap %in% input$eventgap)

      }

    })

      survival_gt_table <- reactive({
        CohortSurvival::tableSurvival(getData(),
                                      times = c(365,
                                                1096,
                                                1826,
                                                3653),
                                      timeScale = "days",
                                      splitStrata = TRUE,
                                      header = input$header,
                                      type = "gt",
                                      groupColumn = NULL,
                                      .options = list()
                                      )
      })

    output$cs_data <- gt::render_gt({
        survival_gt_table()
      })

    previewFigure <- reactive({
      plot <- NULL
      if (nrow(getData()) > 0 && id == "single_event") {
        cumulativeFailure <- TRUE
      } else if (nrow(getData()) > 0 && id == "competing_risk") {
        cumulativeFailure <- TRUE
      }
        CohortSurvival::plotSurvival(getData(),
                                     xscale = "years",
                                     cumulativeFailure = cumulativeFailure,
                                     facet = "cdm_name",
                                     colour = "strata_name")
      })

    output$cs_plot <- renderPlot({
        previewFigure()
      })

    output$downloadSurvivalTable <- downloadHandler(
      filename = function() {
        paste("survivalTable", ".docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(survival_gt_table(), file)
      }
    )

    output$downloadFigure <- downloadHandler(
      filename = function() {
        paste(id, ".png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = previewFigure(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$caption_table <- renderUI({
      captionId <- captionText <- NULL
      if (id == "single_event" || id == "competing_risk") {
        captionText <- "Table 1. Survival Probability"
        captionId <-  "captionSurvivalEstimateTable"
      }

      captionText <- glue::glue("{captionText} in {paste0(input$cdm_name, collapse = ',')} (group_level: {paste0(input$group_level, collapse = ',')}; strata: {paste0(input$strata_name, collapse = ',')})")
      createCaptionInput(inputId = ns(captionId),
                         value = captionText)
    })

    output$caption_plot <- renderUI({
      captionId <- captionText <- NULL
      captionText <- "Figure 1. Survival Probability"
      captionId <-  "captionSurvivalEstimatePlot"

      captionText <- glue::glue("{captionText} in {paste0(input$cdm_name, collapse = ',')} (group_level: {paste0(input$group_level, collapse = ',')}; strata: {paste0(input$strata_name, collapse = ',')})")
      createCaptionInput(inputId = ns(captionId),
                         value = captionText)
    })

    addObject <- reactiveVal()
    observeEvent(input$lock_table, {
      if (nrow(getData()) > 0) {
        survivalObjectType <- paste0(id, " - Table")
        tempList <- list()
        tempList[[survivalObjectType]] <- list(
          x = getData(),
          times = c(365,
                    1096,
                    1826,
                    3653),
          timeScale = "days",
          splitStrata = TRUE,
          header = input$header,
          type = "gt",
          groupColumn = NULL,
          .options = list()
        )
        addObject(tempList)
      }
    })

    observeEvent(input$lock_plot, {
      if (nrow(getData()) > 0) {
        survivalObjectType <- paste0(id, " - Plot")
        tempList <- list()
        tempList[[survivalObjectType]] <- list(
          survivalEstimate = getData(),
          caption = input$captionSurvivalEstimatePlot
          )
        addObject(tempList)
      }
    })

    return(addObject)
  })
}
