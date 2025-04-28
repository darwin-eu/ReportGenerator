characteristicsUI <- function(id, uploaded_files) {
  ns <- NS(id)
    lockName <- "lockSummary"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploaded_files$cdm_name),
                           selected = unique(uploaded_files$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("result_id"),
                           label = "Result Id",
                           choices = unique(uploaded_files$result_id),
                           selected = unique(uploaded_files$result_id)[1],
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_name"),
                           label = "Group Name",
                           choices = unique(uploaded_files$group_name),
                           selected = unique(uploaded_files$group_name)[1],
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploaded_files$group_level),
                           selected = unique(uploaded_files$group_level)[1],
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploaded_files$strata_name),
                           selected = unique(uploaded_files$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_level"),
                           label = "Strata Level",
                           choices = unique(uploaded_files$strata_level),
                           selected = unique(uploaded_files$strata_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploaded_files$variable_name)),
                           selected = unique(uploaded_files$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = c("NA", sort(unique(uploaded_files$variable_level))),
                           selected = c("NA", unique(uploaded_files$variable_level)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploaded_files$estimate_type)),
                           selected = sort(unique(uploaded_files$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      tags$br(),
      fluidRow(
        column(12,
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             fluidRow(
                               column(4,
                                      pickerInput(inputId = ns("header"),
                                                  label = "Header",
                                                  choices = c("cdm_name", "cohort_name", "strata", "window_name"),
                                                  selected = c("cdm_name", "cohort_name"),
                                                  multiple = TRUE)
                                      ),
                               column(4,
                                      pickerInput(inputId = ns("groupColumn"),
                                                  label = "Group Column",
                                                  choices = names(uploaded_files),
                                                  selected = c("variable_name"),
                                                  multiple = TRUE)
                                      ),
                               column(4,
                                      pickerInput(inputId = ns("hideColumn"),
                                                  label = "Hide Column",
                                                  choices = c(omopgenerics::additionalColumns(uploaded_files), omopgenerics::settingsColumns(uploaded_files), "Age Group", "Sex", "Quarter", "Year"),
                                                  selected = c(omopgenerics::additionalColumns(uploaded_files), omopgenerics::settingsColumns(uploaded_files)),
                                                  multiple = TRUE)
                               )


                               ),
                             fluidRow(
                               createAddItemToReportUI(ns(lockName)),
                               column(4, downloadButton(ns("downloadCharacteristicsTable"), "Download Table"))
                               ),
                             tags$br(),
                             fluidRow(
                               column(12,
                                      uiOutput(outputId = ns("captionInput"))
                               ),
                             ),
                             column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
                    )
        )
      )
    )

}

characteristicsServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

      summarised_result <- reactive({
        uploaded_files() %>%
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
                                                    hide = input$hideColumn)
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
                                                         hide = character(),
                                                         caption = input$captionCharacteristics)))
      })

    addObject
  })
}
