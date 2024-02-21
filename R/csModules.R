# CohortSurivial
cohortSurvivalUI <- function(id, dataset) {
  ns <- NS(id)
  outResult <- NULL
  topN <- NULL
  if (id == "survivalTable") {
    outResult <- DT::dataTableOutput(ns("cs_data"))
    topN <- numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100)
    captionText <- "Table 1. Survival estimate data"
    captionId <-  "captionSurvivalEstimateData"
  } else if (id == "survivalPlot") {
    outResult <- plotOutput(ns("cs_plot"))
    captionText <- "Figure 1. Survival estimate plot"
    captionId <-  "captionSurvivalEstimate"
  } else if (id == "failureTable") {
    outResult <- DT::dataTableOutput(ns("cu_inc_data"))
    topN <- numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100)
    captionText <- "Table 1. Cumulative incidence data"
    captionId <-  "captionCumulativeIncidenceData"
  } else if (id == "failurePlot") {
    outResult <- plotOutput(ns("cu_inc_plot"))
    captionText <- "Figure 1. Cumulative incidence plot"
    captionId <-  "captionCumulativeIncidence"
  }
  captionUI <- fluidRow(
    column(8,
           textAreaInput(ns(captionId),
                         "Caption",
                         captionText,
                         width = '100%',
                         height = "130px")
    ),
  )
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
    captionUI,
    fluidRow(column(2, tagList(shiny::HTML("<label class = 'control-label'>&#8205;</label>"),
                               shiny::br(), actionButton(paste0("lock", id), "Add item to report"))),
             column(2, topN)),
    tags$br(),
    fluidRow(column(12, outResult))
  )
}

cohortSurvivalServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({ dataset %>%
        filter(cdm_name %in% input$cdm_name,
               group_level %in% input$group_level,
               strata_name %in% input$strata_name)
    })

    getTableData <- reactive({
      getData() %>%
        dplyr::slice_head(n = input$top_n) %>%
        select(c("cdm_name", "result_type", "group_level", "strata_name",
                 "strata_level", "variable_type", "time", "estimate"))
    })

    if (id == "survivalTable") {
      output$cs_data <- DT::renderDataTable(server = FALSE, {
        createDataTable(getTableData())
      })
    } else if (id == "survivalPlot") {
      output$cs_plot <- renderPlot({
        CohortSurvival::plotSurvival(getData(),
                                     facet = "cdm_name",
                                     colour = "strata_name")
      })
    } else if (id == "failureTable") {
      output$cu_inc_data <- DT::renderDataTable(server = FALSE, {
        createDataTable(getTableData())
      })
    } else if (id == "failurePlot") {
      output$cu_inc_plot <- renderPlot({
        if (nrow(getData()) > 0) {
          CohortSurvival::plotCumulativeIncidence(getData(),
                                                  facet = "cdm_name",
                                                  colour = "strata_name")
        }
      })
    }
  })
}
