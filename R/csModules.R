# CohortSurivial
cohortSurvivalUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  outResult <- NULL
  dlTable <- NULL
  dlPlot <- NULL
  if (id == "survivalTable") {
    outResult <- gt::gt_output(ns("cs_data"))
    dataset <- uploadedFiles$CohortSurvival$single_event %>%
      visOmopResults::splitAdditional()
  } else if (id == "survivalPlot") {
    outResult <- plotOutput(ns("cs_plot"))
    dataset <- uploadedFiles$CohortSurvival$single_event %>%
      visOmopResults::splitAdditional()
  } else if (id == "failureTable") {
    outResult <- gt::gt_output(ns("cu_inc_data"))
    dataset <- uploadedFiles$CohortSurvival$competing_risk %>%
      visOmopResults::splitAdditional()
  } else if (id == "failurePlot") {
    outResult <- plotOutput(ns("cu_inc_plot"))
    dataset <- uploadedFiles$CohortSurvival$competing_risk %>%
      visOmopResults::splitAdditional()
  }
  if (grepl("Plot", id)) {
    dlPlot <- createDownloadPlotUI(ns)
  } else if (grepl("Table", id)) {
    dlTable <- createDownloadTableUI(ns)
  }
  captionUI <- fluidRow(
    column(12,
           uiOutput(outputId = ns("caption"))
    ),
  )
  cdmOptions <- unique(dataset$cdm_name)
  resultIdOptions <- unique(dataset$result_id)
  groupLevelOptions <- unique(dataset$group_level)
  groupNameOptions <- unique(dataset$group_name)
  strataNameOptions <- unique(dataset$strata_name)
  strataLevelOptions <- unique(dataset$strata_level)
  variableNameOptions <- unique(dataset$variable_name)
  variableLevelOptions <- unique(dataset$variable_level)
  estimateTypeOptions <- unique(dataset$estimate_type)
  timeOptions <- unique(dataset$estimate_type)
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
              selected = groupLevelOptions[1],
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
               selected = variableNameOptions[1],
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
               inputId = ns("time"),
               label = "Estimate Level",
               choices = timeOptions,
               selected = timeOptions,
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
                         choices = c("cdm_name", "group", "strata",
                                     "additional", "variable", "estimate", "settings"),
                         selected = c("cdm_name", "estimate"),
                         multiple = TRUE)),
    ),
    captionUI,
    fluidRow(createAddItemToReportUI(ns(paste0("lock", id))),
             dlTable,
             dlPlot),
    tags$br(),
    fluidRow(column(12, outResult))
  )
}

cohortSurvivalServer <- function(id, uploadedFiles) {

  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      uploadedFiles <- uploadedFiles()
      if (id == "survivalTable"  || id == "survivalPlot") {
        dataset <- uploadedFiles$CohortSurvival$single_event
      } else if (id == "failureTable"  || id == "failurePlot") {
        dataset <- uploadedFiles$CohortSurvival$competing_risk
      }
      dataset_split <- dataset %>% visOmopResults::splitAdditional()

      dataset_split %>%
        filter(cdm_name %in% input$cdm_name,
               group_level %in% input$group_level,
               strata_name %in% input$strata_name) %>%
        visOmopResults::uniteAdditional()

    })

    survival_gt_table <- reactive({
      CohortSurvival::tableSurvival(getData(),
                                    header = input$header)
    })

    if (id == "survivalTable") {
      output$cs_data <- gt::render_gt({
        survival_gt_table()
      })
    } else if (id == "survivalPlot") {
      output$cs_plot <- renderPlot({
        previewFigure()
      })
    } else if (id == "failureTable") {
      output$cu_inc_data <- gt::render_gt({
        survival_gt_table()
      })
    } else if (id == "failurePlot") {
      output$cu_inc_plot <- renderPlot({
        previewFigure()
      })
    }

    output$downloadSurvivalTable <- downloadHandler(
      filename = function() {
        paste("survivalTable", ".docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(survival_gt_table(), file)
      }
    )

    previewFigure <- reactive({
      plot <- NULL
      if (nrow(getData()) > 0) {
        if (id == "survivalPlot") {
          plot <- CohortSurvival::plotSurvival(getData(),
                                               facet = "cdm_name",
                                               colour = "strata_name")
        } else if (id == "failurePlot") {
            plot <- CohortSurvival::plotSurvival(getData(),
                                                 facet = "cdm_name",
                                                 colour = "strata_name")
        }
      }
      return(plot)
    })

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

    output$caption <- renderUI({
      captionId <- captionText <- NULL
      if (id == "survivalTable") {
        captionText <- "Table 1. Survival Probability"
        captionId <-  "captionSurvivalEstimateData"
      } else if (id == "survivalPlot") {
        captionText <- "Figure 1. Survival Probability"
        captionId <-  "captionSurvivalEstimate"
      } else if (id == "failureTable") {
        captionText <- "Table 1. Cumulative Survival Probability"
        captionId <-  "captionCumulativeIncidenceData"
      } else if (id == "failurePlot") {
        captionText <- "Figure 1. Cumulative Survival Probability"
        captionId <-  "captionCumulativeIncidence"
      }
      captionText <- glue::glue("{captionText} in {paste0(input$cdm_name, collapse = ',')} (group_level: {paste0(input$group_level, collapse = ',')}; strata: {paste0(input$strata_name, collapse = ',')})")
      createCaptionInput(inputId = ns(captionId),
                         value = captionText)
    })

    addObject <- reactiveVal()
    observeEvent(input$locksurvivalTable, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Single Event - Table` = list(survivalEstimate = getData(),
                                       caption = input$captionSurvivalEstimateData))
        )
      }
    })

    observeEvent(input$locksurvivalPlot, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Single Event - Plot` = list(survivalEstimate = getData(),
                                      plotOption = "Facet by database, colour by strata_name",
                                      caption = input$captionSurvivalEstimate))
        )
      }
    })

    observeEvent(input$lockfailureTable, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Competing Risk - Table` = list(cumulativeSurvivalEstimate = getData(),
                                                   caption = input$captionCumulativeIncidenceData))
        )
      }
    })

    observeEvent(input$lockfailurePlot, {
      if (nrow(getData()) > 0) {
        addObject(
          list(`Competing Risk - Plot` = list(cumulativeSurvivalEstimate = getData(),
                                                  plotOption = "Facet by database, colour by strata_name",
                                                  caption = input$captionCumulativeIncidence))
        )
      }
    })
    return(addObject)
  })
}
