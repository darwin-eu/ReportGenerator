incidenceUI <- function(id, uploadedFiles) {
  ns <- NS(id)

  # Pull incidence values from uploadedFiles estimate column
  # Round without decimal points
  incidenceMax <- uploadedFiles %>%
    filter(estimate_name == "incidence_100000_pys_95CI_upper") %>%
    pull(estimate_value) %>%
    ifelse(is.na(.), 0, .) %>%
    ifelse(. == "-", 0, .) %>%
    as.numeric() %>%
    max() %>%
    round()

  tagList(
  fluidRow(
    # column(4,
    #        pickerInput(inputId = ns("group_level"),
    #                    label = "Group Level",
    #                    choices = unique(uploadedFiles$group_level),
    #                    selected = unique(uploadedFiles$group_level)[1],
    #                    multiple = TRUE,
    #                    list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
    #        ),
    # column(4,
    #        pickerInput(inputId = ns("group_level"),
    #                    label = "Group Level",
    #                    choices = unique(uploadedFiles$group_level),
    #                    selected = unique(uploadedFiles$group_level)[1],
    #                    multiple = TRUE,
    #                    list(`actions-box` = TRUE,
    #                         size = 10,
    #                         `selected-text-format` = "count > 3"))
    #        ),
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
                              fluidRow(createAddItemToReportUI(ns("incidence_table")),
                                       column(4, downloadButton(ns("downloadIncidenceTable"), "Download Table"))),
                              fluidRow(column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt")))))
                       ),
                       tabPanel("Plot",
                                fluidRow(column(4,
                                                pickerInput(inputId = ns("facet"),
                                                            label = "Facet",
                                                            choices = c("cdm_name",
                                                                        "denominator_sex",
                                                                        "denominator_age_group",
                                                                        "variable_level"),
                                                            # choices = names(uploadedFiles),
                                                            selected = c("cdm_name", "denominator_sex"),
                                                            multiple = TRUE)),
                                         column(4,
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
                                         column(4,
                                                sliderInput(inputId = ns("y_limit"),
                                                            label = "Y limit",
                                                            min = 0,
                                                            max = incidenceMax,
                                                            value = incidenceMax,
                                                            step = 1)
                                         )
                                ),
                                fluidRow(createAddItemToReportUI(ns("incidence_plot"))),
                                fluidRow(createDownloadPlotUI(ns)),
                                fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(ns("summarisedIncidencePlot")))))),
                       tabPanel("Data",
                                fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("summarisedTable"))))))
                              # fluidRow(column(12, verbatimTextOutput(ns("summarised_text")))))

  )
)
}

incidenceServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result <- reactive({
      # req(input$group_level)
      uploadedFiles()
      # %>%
      #   filter(group_level %in% input$group_level)
    })

    # observe({
    #   req(summarised_result())
    #   updatePickerInput(session,
    #                     "variable_level",
    #                     choices = unique(summarised_result()$variable_level),
    #                     selected = unique(summarised_result()$variable_level)[1])
    #
    # })

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

    summarisedIncidence_gt_table <- reactive({
      req(final_summarised_result())
      IncidencePrevalence::tableIncidence(result = final_summarised_result(),
                                          header = input$header,
                                          groupColumn = input$groupColumn,
                                          settingsColumns = input$settingsColumns)
    })

    output$summarisedTableGt <- gt::render_gt({
      req(summarisedIncidence_gt_table())
      summarisedIncidence_gt_table()
    })

    summarisedIncidence_plot <- reactive({
      IncidencePrevalence::plotIncidence(result = final_summarised_result(),
                                         x = "incidence_start_date",
                                         ylim = c(0, input$y_limit),
                                         ribbon = TRUE,
                                         facet = input$facet,
                                         colour = input$colour,
                                         colour_name = NULL #,
                                         # options = list('hideConfidenceInterval' = TRUE)
                                         )
    })

    output$summarisedIncidencePlot <- renderPlot({
      summarisedIncidence_plot()
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
                   plot = summarisedIncidence_plot(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$downloadIncidenceTable <- downloadHandler(
      filename = function() {
        paste("incidenceTable", ".docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarisedIncidence_gt_table(), file)
      }
    )

    addObject <- reactiveVal()
        observeEvent(input$incidence_table, {
          addObject(
            list(incidence_table = list(incidence_estimates = final_summarised_result()
                                                         # plotOption = input$facet
                                                         # caption = input$captionInc,
                                                         # ribbon = input$ribbonIncidence,
                                                         # options = c(input$showCIIncidence, input$stackPlotsIncidence)
                                            )
                 )
          )
        })

        observeEvent(input$incidence_plot, {
          addObject(
            list(incidence_plot = list(incidence_estimates = final_summarised_result()
                                                                # plotOption = input$facet
                                                                # caption = input$captionInc,
                                                                # ribbon = input$ribbonIncidence,
                                                                # options = c(input$showCIIncidence, input$stackPlotsIncidence)
                                           )
                 )
          )
        })

        return(addObject)

  })
}
