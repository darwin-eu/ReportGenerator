prevalenceUI <- function(id, uploadedFiles, uploadedFilesAttrition) {
  ns <- NS(id)

  # Pull prevalence values from uploadedFiles estimate column
  # Round without decimal points

  prevalenceMax <- uploadedFiles %>%
    filter(estimate_name == "prevalence_95CI_upper") %>%
    pull(estimate_value) %>%
    ifelse(is.na(.), 0, .) %>%
    as.numeric() %>%
    max() %>%
    round()

  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Estimates",
                         tags$br(),
                         fluidRow(
                         # column(4,
                         #        pickerInput(inputId = ns("group_level"),
                         #                    label = "Group Level",
                         #                    choices = unique(uploadedFiles$group_level),
                         #                    selected = unique(uploadedFiles$group_level)[1],
                         #                    multiple = TRUE,
                         #                    list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
                         #        ),
                         column(4,
                                pickerInput(inputId = ns("analysis_type"),
                                            label = "Analysis Type",
                                            choices = unique(settings(uploadedFiles)$analysis_type),
                                            selected = unique(settings(uploadedFiles)$analysis_type)[1],
                                            multiple = FALSE,
                                            list(`actions-box` = TRUE,
                                                 size = 10,
                                                 `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("strata_level"),
                                            label = "Strata Level",
                                            choices = unique(uploadedFiles$strata_level),
                                            selected = unique(uploadedFiles$strata_level)[1],
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE,
                                                 size = 10,
                                                 `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("variable_level"),
                                            label = "Variable Level",
                                            choices = unique(uploadedFiles$variable_level),
                                            selected = unique(uploadedFiles$variable_level)[1],
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE,
                                                 size = 10,
                                                 `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("denominator_target_cohort_name"),
                                            label = "Denominator target cohort name",
                                            choices = unique(settings(uploadedFiles)$denominator_target_cohort_name),
                                            selected = unique(settings(uploadedFiles)$denominator_target_cohort_name)[1],
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("cdm_name"),
                                            label = "CDM Name",
                                            choices = unique(uploadedFiles$cdm_name),
                                            selected = unique(uploadedFiles$cdm_name)[1],
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("denominator_sex"),
                                            label = "Sex",
                                            choices = unique(settings(uploadedFiles)$denominator_sex),
                                            selected = unique(settings(uploadedFiles)$denominator_sex),
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
                         ),
                         column(4,
                                pickerInput(inputId = ns("denominator_age_group"),
                                            label = "Age Group",
                                            choices = unique(settings(uploadedFiles)$denominator_age_group),
                                            selected = unique(settings(uploadedFiles)$denominator_age_group),
                                            multiple = TRUE,
                                            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
                         )
                         ),
                         tags$br(),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Table",
                                              tags$br(),
                                              fluidRow(column(4,
                                                              pickerInput(inputId = ns("header"),
                                                                          label = "Header",
                                                                          choices = names(uploadedFiles),
                                                                          selected = c("estimate_name"),
                                                                          multiple = TRUE)),
                                                       column(4,
                                                              pickerInput(inputId = ns("groupColumn"),
                                                                          label = "Group Column",
                                                                          choices = names(uploadedFiles),
                                                                          selected = c("cdm_name"),
                                                                          multiple = TRUE)),
                                                       column(4,
                                                              pickerInput(inputId = ns("settingsColumns"),
                                                                          label = "Settings Columns",
                                                                          choices = colnames(settings(uploadedFiles)),
                                                                          selected = c("denominator_target_cohort_name"),
                                                                          multiple = TRUE))
                                              ),
                                              fluidRow(createAddItemToReportUI(ns("prevalence_table")),
                                                       column(4, downloadButton(ns("downloadPrevalenceTable"), "Download Table"))),
                                              fluidRow(column(12, gt::gt_output(ns("summarisedTableGt"))))
                                     ),
                                     tabPanel("Plot",
                                              tags$br(),
                                              fluidRow(column(3,
                                                              pickerInput(inputId = ns("facet"),
                                                                          label = "Facet",
                                                                          choices = c("cdm_name",
                                                                                      "denominator_sex",
                                                                                      "denominator_age_group",
                                                                                      "variable_level"),
                                                                          # choices = names(uploadedFiles),
                                                                          selected = c("cdm_name", "denominator_sex"),
                                                                          multiple = TRUE)),
                                                       column(3,
                                                              pickerInput(inputId = ns("colour"),
                                                                          label = "Colour",
                                                                          choices = c("cdm_name",
                                                                                      "denominator_sex",
                                                                                      "denominator_age_group",
                                                                                      "strata_level",
                                                                                      "variable_level"),
                                                                          # choices = colnames(settings(uploadedFiles)),
                                                                          selected = "denominator_age_group",
                                                                          multiple = TRUE)),
                                                       column(3,
                                                              sliderInput(inputId = ns("y_limit"),
                                                                          label = "Y limit",
                                                                          min = 0,
                                                                          max = prevalenceMax,
                                                                          value = prevalenceMax,
                                                                          step = 0.01)
                                                       ),
                                                       column(3,
                                                              checkboxInput(inputId = ns("ribbon"),
                                                                            label = "Ribbon",
                                                                            value = FALSE,
                                                                            width = NULL)
                                                       ),
                                              ),
                                              fluidRow(createAddItemToReportUI(ns("prevalence_plot"))),
                                              fluidRow(createDownloadPlotUI(ns)),
                                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(ns("summarisedPrevalencePlot"))))),
                                     tabPanel("Data",
                                              fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("summarisedTable"))))))
                                     # fluidRow(column(12, verbatimTextOutput(ns("summarised_text")))))

                         ))

                ),
                tabPanel("Attrition",
                         tags$br(),
                         attritionUI("Prevalence Attrition", uploadedFiles = uploadedFilesAttrition)

  )
)
)
}

prevalenceServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result <- reactive({
      req(input$analysis_type)
      uploadedFiles <- uploadedFiles()
      uploadedFiles %>%
        visOmopResults::filterSettings(analysis_type == input$analysis_type)
    })

    observe({
      req(summarised_result())
      # updatePickerInput(session,
      #                   "strata_level",
      #                   choices = unique(summarised_result()$strata_level),
      #                   selected = unique(summarised_result()$strata_level)[1])
      # updatePickerInput(session,
      #                   "variable_level",
      #                   choices = unique(summarised_result()$variable_level),
      #                   selected = unique(summarised_result()$variable_level)[1])
      # updatePickerInput(session,
      #                   "denominator_target_cohort_name",
      #                   choices = unique(summarised_result()$denominator_target_cohort_name),
      #                   selected = unique(summarised_result()$denominator_target_cohort_name)[1])
      # updatePickerInput(session,
      #                   "denominator_age_group",
      #                   choices = unique(summarised_result()$denominator_age_group),
      #                   selected = unique(summarised_result()$denominator_age_group)[1])
      # updatePickerInput(session,
      #                   "denominator_age_group",
      #                   choices = unique(summarised_result()$denominator_sex),
      #                   selected = unique(summarised_result()$denominator_sex)[1])


    })

    final_summarised_result <- reactive({
      req(summarised_result())
      summarised_result <- summarised_result()

      summarised_result %>% dplyr::filter(variable_level %in% input$variable_level,
                                          strata_level %in% input$strata_level,
                                          cdm_name %in% input$cdm_name) %>%
        visOmopResults::filterSettings(denominator_target_cohort_name %in% input$denominator_target_cohort_name,
                                       denominator_sex %in% input$denominator_sex,
                                       denominator_age_group %in% input$denominator_age_group)

    })

    summarisedPrevalence_gt_table <- reactive({
      req(final_summarised_result())
      IncidencePrevalence::tablePrevalence(result = final_summarised_result(),
                                          header = input$header,
                                          groupColumn = input$groupColumn,
                                          settingsColumns = input$settingsColumns)
    })

    output$summarisedTableGt <- gt::render_gt({
      req(summarisedPrevalence_gt_table())
      summarisedPrevalence_gt_table()
    })

    summarisedPrevalence_plot <- reactive({
      IncidencePrevalence::plotPrevalence(result = final_summarised_result(),
                                         x = "prevalence_start_date",
                                         ylim = c(0, input$y_limit),
                                         ribbon = input$ribbon,
                                         facet = input$facet,
                                         colour = input$colour,
                                         colour_name = NULL)
    })

    output$summarisedPrevalencePlot <- renderPlot({
      summarisedPrevalence_plot()
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result())
    })

    output$downloadFigure <- downloadHandler(
      filename = function() {
        paste(id, ".png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = summarisedPrevalence_plot(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$downloadPrevalenceTable <- downloadHandler(
      filename = function() {
        paste("prevalenceTable", ".docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarisedPrevalence_gt_table(), file)
      }
    )

    addObject <- reactiveVal()
    observeEvent(input$prevalence_table, {
      addObject(
        list(prevalence_table = list(result = final_summarised_result(),
                                     type = "gt",
                                     header = input$header,
                                     groupColumn = input$groupColumn,
                                     settingsColumns = input$settingsColumns)))
    })

    observeEvent(input$prevalence_plot, {
      addObject(
        list(prevalence_plot = list(result = final_summarised_result(),
                                    ylim = c(0, input$y_limit),
                                    facet = input$facet,
                                    colour = input$colour,
                                    ribbon = input$ribbon)))
    })

    return(addObject)

  })
}
