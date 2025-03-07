largeScaleUI <- function(id, uploaded_files) {
  ns <- NS(id)
    lockName <- "lockLSC"
    captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
    settingsLSC <- settings(uploaded_files)
    result_id_table_name <- paste(settingsLSC$result_id, settingsLSC$table_name, sep = " - ")
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
                           choices = result_id_table_name,
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_name"),
                           label = "Group Name",
                           choices = unique(uploaded_files$group_name),
                           selected = unique(uploaded_files$group_name),
                           multiple = FALSE,
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
      fluidRow(
        column(12,
               createCaptionInput(inputId = ns("captionCharacteristics"),
                                  value = captionText,
                                  height = "80px")
        ),
      ),
      fluidRow(createAddItemToReportUI(ns(lockName)),
               column(2, numericInput(ns("topConcepts"), "Top n", 10, min = 1, max = 100))),
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
                                                         multiple = TRUE)),
                                      column(6,
                                            pickerInput(inputId = ns("groupColumn"),
                                                        label = "Group Column",
                                                        choices = names(uploaded_files),
                                                        selected = c("variable_name"),
                                                        multiple = TRUE)
                                    )),
                                    fluidRow(column(6, downloadButton(ns("downloadLSCTable"), "Download Table"))),
                                    column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt"))))),
                           tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
               )
        )
      )
    )
}

largeScaleServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result <- reactive({
      result_id_table_name <- input$result_id
      split_text <- strsplit(result_id_table_name, " - ")
      result_id_table <- do.call(rbind, split_text)
      result_id_table <- as.data.frame(result_id_table, stringsAsFactors = FALSE)
      summarised_result <- uploaded_files()
      summarised_result %>%
        mutate(across(where(is.character), ~ ifelse(is.na(.), "NA", .))) %>%
        dplyr::filter(cdm_name %in% input$cdm_name,
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
                                                              topConcepts = input$topConcepts,
                                                              type = "gt",
                                                              header = input$header,
                                                              groupColumn = input$groupColumn,
                                                              hide = character())
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

    captionText <- reactive({autoCaptionCharac(summarised_result())})

    output$captionInput <- renderUI({
      createCaptionInput(inputId = ns("captionCharacteristics"),
                         value = captionText())
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result())
    })

    addObject <- reactiveVal()

    observeEvent(input$lockLSC, {
      addObject(
        list(`Summarised Large Scale Characteristics - Table` = list(result = summarised_result(),
                                                                     topConcepts = input$topConcepts,
                                                                     type = "gt",
                                                                     header = input$header,
                                                                     groupColumn = input$groupColumn,
                                                                     hide = character()))
      )
    })
    addObject
  })
}
