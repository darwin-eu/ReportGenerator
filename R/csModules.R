# CohortSurivial
cohortSurvivalUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  outResult <- NULL
  topN <- NULL
  if (id == "survivalTable") {
    outResult <- DT::dataTableOutput(ns("cs_data"))
    topN <- numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100)
    captionText <- "Table 1. Survival estimate data"
    captionId <-  "captionSurvivalEstimateData"
    dataset <- uploadedFiles$dataCS$`Survival estimate`
  } else if (id == "survivalPlot") {
    outResult <- plotOutput(ns("cs_plot"))
    captionText <- "Figure 1. Survival estimate plot"
    captionId <-  "captionSurvivalEstimate"
    dataset <- uploadedFiles$dataCS$`Survival estimate`
  } else if (id == "failureTable") {
    outResult <- DT::dataTableOutput(ns("cu_inc_data"))
    topN <- numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100)
    captionText <- "Table 1. Cumulative incidence data"
    captionId <-  "captionCumulativeIncidenceData"
    dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
  } else if (id == "failurePlot") {
    outResult <- plotOutput(ns("cu_inc_plot"))
    captionText <- "Figure 1. Cumulative incidence plot"
    captionId <-  "captionCumulativeIncidence"
    dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
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
                               shiny::br(), actionButton(ns(paste0("lock", id)), "Add item to report"))),
             column(2, topN)),
    tags$br(),
    fluidRow(column(12, outResult))
  )
}

cohortSurvivalServer <- function(id, uploadedFiles) {
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      uploadedFiles <- uploadedFiles()
      if (id == "survivalTable"  || id == "survivalPlot") {
        dataset <- uploadedFiles$dataCS$`Survival estimate`
      } else if (id == "failureTable"  || id == "failurePlot") {
        dataset <- uploadedFiles$dataCS$`Survival cumulative incidence`
      }
      dataset %>%
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

    addObject <- reactiveVal()
    observeEvent(input$locksurvivalTable, {
      addObject(
        list(`Survival table` = list(survivalEstimate = getTableData(),
                                     caption = input$captionSurvivalEstimateData))
      )
    })

    observeEvent(input$locksurvivalPlot, {
      addObject(
        list(`Survival plot` = list(survivalEstimate = getData(),
                                    plotOption = "Facet by database, colour by strata_name",
                                    caption = input$captionSurvivalEstimate))
      )
    })

    observeEvent(input$lockfailureTable, {
      addObject(
        list(`Cumulative incidence table` = list(cumulativeSurvivalEstimate = getTableData(),
                                                 caption = input$captionCumulativeIncidenceData))
      )
    })

    observeEvent(input$lockfailurePlot, {
      addObject(
        list(`Cumulative incidence plot` = list(cumulativeSurvivalEstimate = getData(),
                                                plotOption = "Facet by database, colour by strata_name",
                                                caption = input$captionCumulativeIncidence))
      )
    })

    return(addObject)
  })
}
