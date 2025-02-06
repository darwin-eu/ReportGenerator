characteristicsUI <- function(id, uploadedFiles) {
  ns <- NS(id)
    lockName <- "lockSummary"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploadedFiles$cdm_name),
                           selected = unique(uploadedFiles$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("result_id"),
                           label = "Result Id",
                           choices = unique(uploadedFiles$result_id),
                           selected = unique(uploadedFiles$result_id)[1],
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_name"),
                           label = "Group Name",
                           choices = unique(uploadedFiles$group_name),
                           selected = unique(uploadedFiles$group_name)[1],
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploadedFiles$group_level),
                           selected = unique(uploadedFiles$group_level)[1],
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploadedFiles$strata_name),
                           selected = unique(uploadedFiles$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_level"),
                           label = "Strata Level",
                           choices = unique(uploadedFiles$strata_level),
                           selected = unique(uploadedFiles$strata_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploadedFiles$variable_name)),
                           selected = unique(uploadedFiles$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = c("NA", sort(unique(uploadedFiles$variable_level))),
                           selected = c("NA", unique(uploadedFiles$variable_level)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploadedFiles$estimate_type)),
                           selected = sort(unique(uploadedFiles$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(12,
               uiOutput(outputId = ns("captionInput"))
        ),
      ),
      fluidRow(createAddItemToReportUI(ns(lockName))),
      tags$br(),
      fluidRow(
        column(12,
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             fluidRow(
                               column(6,
                                      pickerInput(inputId = ns("header"),
                                                  label = "Header",
                                                  choices = c("cdm_name", "cohort_name", "strata", "window_name"),
                                                  selected = c("cdm_name", "cohort_name"),
                                                  multiple = TRUE)
                                      ),
                               column(6,
                                      pickerInput(inputId = ns("groupColumn"),
                                                  label = "Group Column",
                                                  choices = names(uploadedFiles),
                                                  selected = c("variable_name"),
                                                  multiple = TRUE)
                                      )
                               ),
                             fluidRow(column(6, downloadButton(ns("downloadCharacteristicsTable"), "Download Table"))),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
                    )
        )
      )
    )

}

characteristicsServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

      summarised_result <- reactive({
        uploadedFiles() %>%
          # mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
          dplyr::filter(cdm_name %in% input$cdm_name,
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
                                                    type = "gt",
                                                    header = input$header,
                                                    groupColumn = input$groupColumn,
                                                    hide = character())
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
        list(`Summarised Characteristics - Table` = list(result = summarised_result(),
                                                         type = "gt",
                                                         header = input$header,
                                                         groupColumn = input$groupColumn,
                                                         hide = character())))
      })

    addObject
  })
}
