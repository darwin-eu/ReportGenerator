characteristicsUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "characteristics") {
    lockName <- "lockSummary"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$cdm_name),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("result_id"),
                           label = "Result Id",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$result_id),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$result_id),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_name"),
                           label = "Group Name",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$group_name),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$group_name),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$group_level),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$group_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$strata_name),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_level"),
                           label = "Strata Level",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$strata_level),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$strata_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_characteristics$variable_name)),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = c("NA", sort(unique(uploadedFiles$dataPP$summarised_characteristics$variable_level))),
                           selected = c("NA", unique(uploadedFiles$dataPP$summarised_characteristics$variable_level)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_characteristics$estimate_type)),
                           selected = sort(unique(uploadedFiles$dataPP$summarised_characteristics$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(12,
               uiOutput(outputId = ns("captionInput"))
               # createCaptionInput(inputId = ns("captionCharacteristics"),
               #                    value = captionText,
               #                    height = "80px")
        ),
      )
      ,
      fluidRow(createAddItemToReportUI(ns(lockName)),
               column(2, numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100))),
      tags$br(),
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             pickerInput(inputId = ns("pivotWide"),
                                         label = "Arrange by",
                                         choices = c("group", "strata"),
                                         selected = c("group", "strata"),
                                         multiple = TRUE),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
        )
      )
    )
  } else {
    lockName <- "lockLSC"
    captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$cdm_name),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("result_id"),
                           label = "Result Id",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$result_id),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$result_id),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_name"),
                           label = "Group Name",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_name),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_name),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_level),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_name),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_level"),
                           label = "Strata Level",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_level),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_name)),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = c("NA", sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_level))),
                           selected = c("NA", unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_level)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("table_name"),
                           label = "Table Name",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$table_name)),
                           selected = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$table_name)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$estimate_type)),
                           selected = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(12,
               createCaptionInput(inputId = ns("captionCharacteristics"),
                                  value = captionText,
                                  height = "80px")
        ),
      ),
      fluidRow(createAddItemToReportUI(ns(lockName)),
               column(2, numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100))),
      tags$br(),
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             pickerInput(inputId = ns("pivotWide"),
                                         label = "Arrange by",
                                         choices = c("group", "strata"),
                                         selected = c("group", "strata"),
                                         multiple = TRUE),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
        )
      )
    )
  }
}

characteristicsServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (id == "characteristics") {
      summarised_result <- reactive({
        uploadedFiles <- uploadedFiles()
        summarised_result <- uploadedFiles$dataPP$summarised_characteristics
        summarised_result %>%
          mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
          filter(cdm_name %in% input$cdm_name,
                 result_id %in% input$result_id,
                 group_name %in% input$group_name,
                 group_level %in% input$group_level,
                 strata_name %in% input$strata_name,
                 strata_level %in% input$strata_level,
                 estimate_type %in% input$estimate_type,
                 variable_level %in% input$variable_level,
                 variable_name %in% input$variable_name)
        # %>%
        #   mutate(estimate_value = ifelse(estimate_type == "percentage", round(as.numeric(estimate_value), 2), estimate_value))
      })

      output$summarisedTableGt <- gt::render_gt({
        # summarised_result <- as_tibble(selectCols(summarised_result()))
        CohortCharacteristics::tableCharacteristics(result = summarised_result(), split = input$pivotWide)
      })

    } else if (id == "lsc") {
      summarised_result <- reactive({
        uploadedFiles <- uploadedFiles()
        summarised_result <- uploadedFiles$dataPP$summarised_large_scale_characteristics
        summarised_result %>%
          mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
          filter(cdm_name %in% input$cdm_name,
                 result_id %in% input$result_id,
                 group_name %in% input$group_name,
                 group_level %in% input$group_level,
                 strata_name %in% input$strata_name,
                 strata_level %in% input$strata_level,
                 variable_name %in% input$variable_name,
                 variable_level %in% input$variable_level,
                 # table_name %in% input$table_name,
                 estimate_type %in% input$estimate_type
                 )
      #   # %>%
      #   #   mutate(estimate_value = ifelse(estimate_type == "percentage", round(as.numeric(estimate_value), 2), estimate_value))
      })

      output$summarisedTableGt <- gt::render_gt({
        # summarised_result <- as_tibble(selectColsLSC(summarised_result())) %>%
        #   omopgenerics::newSummarisedResult()
        CohortCharacteristics::tableLargeScaleCharacteristics(result = summarised_result(),
                                                              splitStrata  = TRUE,
                                                              topConcepts = input$top_n)
      })

    }

    captionText <- reactive({autoCaptionCharac(summarised_result())})

    output$captionInput <- renderUI({
      createCaptionInput(inputId = ns("captionCharacteristics"),
                         value = captionText())
      # textAreaInput(inputId = ns("captionCharacteristics"),
      #               label = "Caption",
      #               value = captionText,
      #               width = '100%',
      #               height = "50px")
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result())
    })

    addObject <- reactiveVal()

    observeEvent(input$lockSummary, {
      # dataPP <- as_tibble(selectCols(dataPP()))
      addObject(
        list(`Summarised Characteristics - Table` = list(summarisedCharacteristics = summarised_result(),
                                               caption = input$captionCharacteristics))
      )
    })

    observeEvent(input$lockLSC, {
      addObject(
        list(`Summarised Large Scale Characteristics - Table` = list(summarisedCharacteristics = summarised_result(),
                                                             caption = input$captionCharacteristics))
      )
    })
    addObject
  })
}
