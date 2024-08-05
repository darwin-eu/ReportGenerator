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
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$group_level)[1],
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
        ),
      ),
      tags$br(),
      fluidRow(
        column(12,
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
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
                                                  choices = c( "cdm_name", "group", "strata", "additional", "variable", "estimate", "settings"),
                                                  selected = c("cdm_name", "group"),
                                                  multiple = TRUE)),
                             ),
                             fluidRow(column(6, downloadButton(ns("downloadCharacteristicsTable"), "Download Table"))),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
                    )
        )
      )
    )
  } else {
    lockName <- "lockLSC"
    captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
    settingsLSC <- settings(uploadedFiles$dataPP$summarised_large_scale_characteristics)
    result_id_table_name <- paste(settingsLSC$result_id, settingsLSC$table_name, sep = " - ")
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
                           choices = result_id_table_name,
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
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_level)[1],
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
        column(12,
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
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
                                             choices = c("cdm_name", "cohort_name", "strata", "window_name"),
                                             selected = c("cdm_name", "cohort_name"),
                                             multiple = TRUE)),
                             ),
                             fluidRow(column(6, downloadButton(ns("downloadLSCTable"), "Download Table"))),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
                    )
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
      })

      summarisedCharacteristics_gt_table <- reactive({
        CohortCharacteristics::tableCharacteristics(result = summarised_result(),
                                                    split = input$pivotWide,
                                                    header = input$header)
      })

      output$summarisedTableGt <- gt::render_gt({
        summarisedCharacteristics_gt_table()
      })

      output$downloadCharacteristicsTable <- downloadHandler(
        filename = function() {
          paste("summarisedCharacteristics", ".docx", sep = "")
        },
        content = function(file) {
          gt::gtsave(summarisedCharacteristics_gt_table(), file)
        }
      )

    } else if (id == "lsc") {

      summarised_result <- reactive({
        result_id_table_name <- input$result_id
        split_text <- strsplit(result_id_table_name, " - ")
        result_id_table <- do.call(rbind, split_text)
        result_id_table <- as.data.frame(result_id_table, stringsAsFactors = FALSE)
        uploadedFiles <- uploadedFiles()
        summarised_result <- uploadedFiles$dataPP$summarised_large_scale_characteristics
        summarised_result %>%
          mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
          filter(cdm_name %in% input$cdm_name,
                 result_id %in% result_id_table$V1,
                 group_name %in% input$group_name,
                 group_level %in% input$group_level,
                 strata_name %in% input$strata_name,
                 strata_level %in% input$strata_level,
                 variable_name %in% input$variable_name,
                 variable_level %in% input$variable_level,
                 estimate_type %in% input$estimate_type)
      })

      summarisedLSC_gt_table <- reactive({
          CohortCharacteristics::tableLargeScaleCharacteristics(result = summarised_result(),
                                                                splitStrata  = TRUE,
                                                                topConcepts = input$top_n,
                                                                header = input$header)
      })

      output$summarisedTableGt <- gt::render_gt({
        summarisedLSC_gt_table()
      })

      output$downloadLSCTable <- downloadHandler(
        filename = function() {
          paste("summarisedLSC", ".docx", sep = "")
        },
        content = function(file) {
          gt::gtsave(summarisedLSC_gt_table(), file)
        }
      )

    }

    captionText <- reactive({autoCaptionCharac(summarised_result())})

    output$captionInput <- renderUI({
      createCaptionInput(inputId = ns("captionCharacteristics"),
                         value = captionText())
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result())
    })

    addObject <- reactiveVal()

    observeEvent(input$lockSummary, {
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
