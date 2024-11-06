incidenceSumUI <- function(id, uploadedFiles) {

  # Filters defined by the settings(summarisedResult)
  settings_incidence <- settings(uploadedFiles)
  setttings_denominator_sex <- settings_incidence$denominator_sex
  setttings_denominator_age_group <- settings_incidence$denominator_age_group

  start_end_date <- uploadedFiles %>%
    visOmopResults::splitAdditional(keep = FALSE, fill = "overall") %>%
    select(incidence_start_date,
           incidence_end_date)

  ns <- NS(id)
    lockName <- "lockIncidence"
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
        ),
        column(4,
               pickerInput(inputId = ns("incidence_start_date"),
                           label = "Start Date",
                           choices = unique(start_end_date$incidence_start_date),
                           selected = min(unique(start_end_date$incidence_start_date)),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("incidence_end_date"),
                           label = "End Date",
                           choices = unique(start_end_date$incidence_end_date),
                           selected = max(unique(start_end_date$incidence_end_date)),
                           multiple = FALSE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("denominator_sex"),
                           label = "Denominator Sex",
                           choices = unique(setttings_denominator_sex),
                           selected = unique(setttings_denominator_sex),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("denominator_age_group"),
                           label = "Denominator Age Group",
                           choices = unique(setttings_denominator_age_group),
                           selected = unique(setttings_denominator_age_group),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
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
                                    fluidRow(column(6, downloadButton(ns("downloadIncidenceTable"), "Download Table"))),
                                    fluidRow(column(12, gt::gt_output(ns("summarisedTableGt"))))),
                           tabPanel("Plot",
                                    fluidRow(column(6, downloadButton(ns("downloadIncidencePlot"), "Download Plot"))),
                                    fluidRow(column(12, plotOutput(ns("summarisedIncidencePlot"))))),
                           tabPanel("Data",
                                    fluidRow(column(12, DT::dataTableOutput(ns("summarisedTable")))))
               )
        )
      )
    )
}

incidenceSumServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

      summarised_result <- reactive({
        req(input$result_id)
        uploadedFiles() %>%
          dplyr::filter(cdm_name %in% input$cdm_name,
                        result_id == input$result_id) %>%
          visOmopResults::filterSettings(result_id == input$result_id)
      })

      observe({
        req(summarised_result())

        summarised_result_end_date <- summarised_result() %>%
          visOmopResults::splitAdditional(keep = FALSE, fill = "overall") %>%
          select(incidence_start_date,
                 incidence_end_date)

        updatePickerInput(session,
                          "group_level",
                          choices = unique(summarised_result()$group_level),
                          selected = unique(summarised_result()$group_level)[1])
        updatePickerInput(session,
                          "group_name",
                          choices = unique(summarised_result()$group_name),
                          selected = unique(summarised_result()$group_name)[1])
        updatePickerInput(session,
                          "strata_name",
                          choices = unique(summarised_result()$strata_name),
                          selected = unique(summarised_result()$strata_name))
        updatePickerInput(session,
                          "strata_level",
                          choices = unique(summarised_result()$strata_level),
                          selected = unique(summarised_result()$strata_level))
        updatePickerInput(session,
                          "estimate_type",
                          choices = unique(summarised_result()$estimate_type),
                          selected = unique(summarised_result()$estimate_type))
        updatePickerInput(session,
                          "variable_level",
                          choices = unique(summarised_result()$variable_level),
                          selected = unique(summarised_result()$variable_level))
        updatePickerInput(session,
                          "variable_name",
                          choices = unique(summarised_result()$variable_name),
                          selected = unique(summarised_result()$variable_name))
        updatePickerInput(session,
                          "denominator_sex",
                          choices = unique(settings(summarised_result())$denominator_sex),
                          selected = unique(settings(summarised_result())$denominator_sex))
        updatePickerInput(session,
                          "denominator_age_group",
                          choices = unique(settings(summarised_result())$denominator_age_group),
                          selected = unique(settings(summarised_result())$denominator_age_group))
        updatePickerInput(session,
                          "incidence_start_date",
                          choices = unique(summarised_result_end_date$incidence_start_date),
                          selected = min(unique(summarised_result_end_date$incidence_start_date)))
        updatePickerInput(session,
                          "incidence_end_date",
                          choices = unique(summarised_result_end_date$incidence_end_date),
                          selected = max(unique(summarised_result_end_date$incidence_end_date)))

      })

      # Final Summarised Result

      final_summarised_result <- reactive({
        req(summarised_result())
        summarised_result <- summarised_result()

        attr(summarised_result, "settings") <- settings(summarised_result) %>%
          dplyr::filter(result_id %in% input$result_id)
          summarised_result %>% dplyr::filter(group_name %in% input$group_name,
                                              group_level == input$group_level,
                                              strata_name %in% input$strata_name,
                                              strata_level %in% input$strata_level,
                                              estimate_type %in% input$estimate_type,
                                              variable_level %in% input$variable_level,
                                              variable_name %in% input$variable_name) %>%
            visOmopResults::filterSettings(denominator_sex == input$denominator_sex,
                                           denominator_age_group == input$denominator_age_group) %>%
            visOmopResults::filterAdditional(incidence_start_date %in% input$incidence_start_date,
                                             incidence_end_date %in% input$incidence_end_date)
      })

      # Table

      summarisedIncidence_gt_table <- reactive({
        req(final_summarised_result())
        IncidencePrevalence::tableIncidence(result = final_summarised_result())
      })

      output$summarisedTableGt <- gt::render_gt({
        req(summarisedIncidence_gt_table())
        summarisedIncidence_gt_table()
      })

      # Plot

      summarisedIncidence_plot <- reactive({
        IncidencePrevalence::plotIncidence(result = summarised_result())
      })

      output$summarisedIncidencePlot <- renderPlot({
        summarisedIncidence_plot()
      })

      output$downloadIncidenceTable <- downloadHandler(
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
