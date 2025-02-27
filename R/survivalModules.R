cohortSurvivalUI <- function(id, uploaded_files) {
  ns <- NS(id)
  outResult <- NULL
  dlTable <- NULL
  dlPlot <- NULL
  dataset <- uploaded_files %>% visOmopResults::splitAdditional()
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
                 inputId = ns("group_name"),
                 label = "Group Name",
                 choices = groupNameOptions,
                 selected = groupNameOptions,
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
                 selected = strataNameOptions,
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
                 inputId = ns("time_scale"),
                 label = "Time Scale",
                 choices = c("years", "days")),
                 selected = c("years"),
                 options = pickerOptions,
                 multiple = FALSE
               ),
        column(4,
               textInput(ns("times"), label = "Times", value = "1, 3, 5, 10")
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
        ),
      tags$br(),
      tabsetPanel(type = "tabs",
                  tabPanel("Table",
                           tags$br(),
                           tableFiltersSurvivalUI(id, uploaded_files)),
                  tabPanel("Plot",
                           tags$br(),
                           plotFiltersSurvivalUI(id, uploaded_files)),
                  )
      )
}

cohortSurvivalServer <- function(id, uploaded_files) {

  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      if (id == "single_event" || id == "competing_risk") {
        uploaded_files() %>%
          filter(cdm_name %in% input$cdm_name,
                 group_name %in% group_name,
                 group_level %in% input$group_level,
                 strata_name %in% input$strata_name,
                 strata_level %in% input$strata_level,
                 variable_name %in% input$variable_name,
                 variable_level %in% input$variable_level,
                 estimate_type %in% input$estimate_type)
      }

    })

    observeEvent(input$group_name, {

      groupLevelOptions <- uploaded_files() %>%
        filter(group_name %in% input$group_name) %>%
        pull(group_level) %>%
        unique()

      updatePickerInput(session,
                        inputId = "group_level",
                        selected = groupLevelOptions,
                        choices = groupLevelOptions)

    })

    observeEvent(input$strata_name, {

      strataLevelOptions <- uploaded_files() %>%
        filter(strata_name %in% input$strata_name) %>%
        pull(strata_level) %>%
        unique()

      updatePickerInput(session,
                        inputId = "strata_level",
                        selected = strataLevelOptions,
                        choices = strataLevelOptions)

    })

    times <- reactive({
      values <- unlist(strsplit(input$times, "[, ]+"))
      as.numeric(values)
    })


    output$times_observe <- renderPrint({
      print(times())
    })

    survival_gt_table <- reactive({
        times <- as.numeric(input$times)

        CohortSurvival::tableSurvival(getData(),
                                      times = times(),
                                      timeScale = input$time_scale,
                                      splitStrata = input$split_strata,
                                      header = input$header,
                                      type = "gt",
                                      groupColumn = input$groupColumn,
                                      .options = list()
                                      )
      })

    output$summarisedTable <- gt::render_gt({
        survival_gt_table()
      })

    summarised_plot <- reactive({
      CohortSurvival::plotSurvival(result = getData(),
                                   x = input$x_axis,
                                   xscale = input$time_scale,
                                   ylim = c(0, NA),
                                   cumulativeFailure = input$cumulative_failure,
                                   ribbon = input$ribbon,
                                   facet = input$facet,
                                   colour = input$colour,
                                   riskTable = input$risk_table,
                                   riskInterval = as.numeric(input$risk_interval))
      })

    output$summarisedPlot <- renderPlot({
      summarised_plot()
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
                   plot = summarised_plot(),
                   height = as.numeric(input$download_plot_height),
                   width = as.numeric(input$download_plot_height_width),
                   dpi = as.numeric(input$download_plot_dpi))
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
    observeEvent(input$add_table, {
      if (nrow(getData()) > 0) {
        survivalObjectType <- paste0(id, " - Table")
        tempList <- list()
        tempList[[survivalObjectType]] <- list(
          x = getData(),
          times = times(),
          timeScale = input$time_scale,
          splitStrata = input$split_strata,
          header = input$header,
          type = "gt",
          groupColumn = input$groupColumn,
          .options = list()
        )
        addObject(tempList)
      }
    })

    observeEvent(input$add_plot, {
        survivalObjectType <- paste0(id, " - Plot")
        tempList <- list()
        tempList[[survivalObjectType]] <- list(result = getData(),
                                               x = input$x_axis,
                                               xscale = input$time_scale,
                                               ylim = c(0, NA),
                                               cumulativeFailure = input$cumulative_failure,
                                               ribbon = input$ribbon,
                                               facet = input$facet,
                                               colour = input$colour,
                                               riskTable = input$risk_table,
                                               riskInterval = as.numeric(input$risk_interval))
        addObject(tempList)
    })

    return(addObject)
  })
}
