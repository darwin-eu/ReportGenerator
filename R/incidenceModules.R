incidenceUI <- function(id, uploadedFiles, uploadedFilesAttrition) {
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
    tabsetPanel(type = "tabs",
                tabPanel("Estimates",
                         tags$br(),
                         mainFiltersIncPrevUI(id = id, uploadedFiles),
                         tags$br(),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Table",
                                              tags$br(),
                                              fluidRow(column(4,
                                                              pickerInput(inputId = ns("header"),
                                                                          label = "Header",
                                                                          choices = c("cdm_name",
                                                                                      "denominator_cohort_name",
                                                                                      "outcome_cohort_name",
                                                                                      "incidence_start_date",
                                                                                      "incidence_end_date",
                                                                                      "estimate_name"),,
                                                                          selected = c("estimate_name"),
                                                                          multiple = TRUE)),
                                                       column(4,
                                                              pickerInput(inputId = ns("groupColumn"),
                                                                          label = "Group Column",
                                                                          choices = c("cdm_name", "outcome_cohort_name"),
                                                                          selected = c("cdm_name", "outcome_cohort_name"),
                                                                          multiple = TRUE)),
                                                       column(4,
                                                              pickerInput(inputId = ns("settingsColumn"),
                                                                          label = "Settings Columns",
                                                                          choices = c("denominator_age_group", "denominator_sex"),
                                                                          selected = c("denominator_age_group", "denominator_sex"),
                                                                          multiple = TRUE)),
                                                       column(4,
                                                              pickerInput(inputId = ns("hide"),
                                                                          label = "Hide Columns",
                                                                          choices = c("denominator_cohort_name", "analysis_interval"),
                                                                          selected = c("denominator_cohort_name", "analysis_interval"),
                                                                          multiple = TRUE))
                                              ),
                                              fluidRow(createAddItemToReportUI(ns("incidence_table")),
                                                       column(4, downloadButton(ns("downloadIncidenceTable"), "Download Table"))),
                                              fluidRow(column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTableGt")))))
                                     ),
                                     tabPanel("Plot",
                                              tags$br(),
                                              fluidRow(column(3,
                                                              pickerInput(inputId = ns("x_axis"),
                                                                          label = "X Axis",
                                                                          choices = c("incidence_start_date",
                                                                                      "denominator_time_at_risk",
                                                                                      "denominator_age_group",
                                                                                      "denominator_sex"),
                                                                          selected = c("incidence_start_date"),
                                                                          multiple = FALSE)),
                                                       column(3,
                                                              pickerInput(inputId = ns("facet"),
                                                                          label = "Facet",
                                                                          choices = c("cdm_name",
                                                                                      "denominator_sex",
                                                                                      "denominator_age_group",
                                                                                      "strata_level",
                                                                                      "group",
                                                                                      "outcome_cohort_name"),
                                                                          selected = c("cdm_name"),
                                                                          multiple = TRUE)),
                                                       column(3,
                                                              pickerInput(inputId = ns("colour"),
                                                                          label = "Colour",
                                                                          choices = c("cdm_name",
                                                                                      "denominator_sex",
                                                                                      "denominator_age_group",
                                                                                      "strata",
                                                                                      "outcome_cohort_name"),
                                                                          selected = "outcome_cohort_name",
                                                                          multiple = TRUE)
                                                       ),
                                                       column(3,
                                                              checkboxInput(inputId = ns("line"),
                                                                            label = "Line",
                                                                            value = TRUE,
                                                                            width = NULL)
                                                       ),
                                                       column(3,
                                                              checkboxInput(inputId = ns("point"),
                                                                            label = "Point",
                                                                            value = TRUE,
                                                                            width = NULL)
                                                       ),
                                                       column(3,
                                                              checkboxInput(inputId = ns("ribbon"),
                                                                            label = "Ribbon",
                                                                            value = FALSE,
                                                                            width = NULL)
                                                       )
                                              ),
                                              fluidRow(createAddItemToReportUI(ns("incidence_plot"))),
                                              fluidRow(createDownloadPlotUI(ns)),
                                              fluidRow(column(12, shinycssloaders::withSpinner(plotOutput(ns("summarisedIncidencePlot")))))),
                                     tabPanel("Data",
                                              fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("summarisedTable"))))))
                                     # fluidRow(column(12, verbatimTextOutput(ns("summarised_text")))))

                         )),
                tabPanel("Attrition",
                         tags$br(),
                         attritionUI("Incidence Attrition", uploadedFiles = uploadedFilesAttrition)
                ))
)
}

mainFiltersIncPrevUI <- function(id, uploadedFiles) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "CDM Name",
                         choices = unique(uploadedFiles$cdm_name),
                         selected = unique(uploadedFiles$cdm_name),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("group_level"),
                         label = "Group Level",
                         choices = unique(uploadedFiles$group_level),
                         selected = unique(uploadedFiles$group_level),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("strata_level"),
                         label = "Strata Level",
                         choices = unique(uploadedFiles$strata_level),
                         selected = unique(uploadedFiles$strata_level)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("variable_level"),
                         label = "Variable Level",
                         choices = unique(uploadedFiles$variable_level),
                         selected = unique(uploadedFiles$variable_level)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
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
             pickerInput(inputId = ns("denominator_sex"),
                         label = "Sex",
                         choices = unique(settings(uploadedFiles)$denominator_sex),
                         selected = unique(settings(uploadedFiles)$denominator_sex)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_age_group"),
                         label = "Age Group",
                         choices = unique(settings(uploadedFiles)$denominator_age_group),
                         selected = unique(settings(uploadedFiles)$denominator_age_group)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_time_at_risk"),
                         label = "Denominator Time at Risk",
                         choices = unique(settings(uploadedFiles)$denominator_time_at_risk),
                         selected = unique(settings(uploadedFiles)$denominator_time_at_risk),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("analysis_type"),
                         label = "Analysis Type",
                         choices = unique(settings(uploadedFiles)$analysis_type),
                         selected = unique(settings(uploadedFiles)$analysis_type[3]),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      )
    ),
  )
}

incidenceServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # summarised_result <- reactive({
    #   # req(input$group_level)
    #   uploadedFiles()
    #   # %>%
    #   #   filter(group_level %in% input$group_level)
    # })

    # observe({
    #   req(summarised_result())
    #   updatePickerInput(session,
    #                     "variable_level",
    #                     choices = unique(summarised_result()$variable_level),
    #                     selected = unique(summarised_result()$variable_level)[1])
    #
    # })

    final_summarised_result <- reactive({
      req(uploadedFiles())
      # summarised_result <- summarised_result()

      # uploadedFiles() %>% dplyr::filter(cdm_name %in% "CHUBX",
      #                                   group_level %in% "denominator_cohort_3 &&& cohort_1",
      #                                   strata_level %in% "overall",
      #                                   variable_level %in% NA
      # ) %>%
      #   visOmopResults::filterSettings(denominator_target_cohort_name %in% "None",
      #                                  denominator_sex %in% "Both",
      #                                  denominator_age_group %in% "0 to 64",
      #                                  denominator_time_at_risk %in% "0 to Inf"
      #                                  # ,
      #                                  # analysis_type %in% input$analysis_type
      #   )

      uploadedFiles() %>% dplyr::filter(cdm_name %in% input$cdm_name,
                                        group_level %in% input$group_level,
                                        strata_level %in% input$strata_level,
                                        variable_level %in% NA) %>%
        visOmopResults::filterSettings(denominator_target_cohort_name %in% input$denominator_target_cohort_name,
                                       denominator_sex %in% input$denominator_sex,
                                       denominator_age_group %in% input$denominator_age_group,
                                       denominator_time_at_risk %in% input$denominator_time_at_risk,
                                       analysis_type %in% NA)

    })

    summarisedIncidence_gt_table <- reactive({
      req(final_summarised_result())

      # tableIncidence(result = final_summarised_result(),
      #                type = "gt",
      #                header = c("estimate_name"),
      #                groupColumn = c("cdm_name", "outcome_cohort_name"),
      #                settingsColumn = c("denominator_age_group", "denominator_sex"),
      #                hide = c("denominator_cohort_name", "analysis_interval"),
      #                .options = list())


      IncidencePrevalence::tableIncidence(result = final_summarised_result(),
                                          header = input$header,
                                          groupColumn = input$groupColumn,
                                          settingsColumn = input$settingsColumn,
                                          hide = input$hide
                                          )
    })

    output$summarisedTableGt <- gt::render_gt({
      req(summarisedIncidence_gt_table())
      summarisedIncidence_gt_table()
    })

    summarisedIncidence_plot <- reactive({
      IncidencePrevalence::plotIncidence(
        result = final_summarised_result(),
        x = input$x_axis,
        y = "incidence_100000_pys",
        line = input$line,
        point = input$point,
        ribbon = input$ribbon,
        ymin = "incidence_100000_pys_95CI_lower",
        ymax = "incidence_100000_pys_95CI_upper",
        facet = input$facet,
        colour = input$colour
        )
    })

    output$summarisedIncidencePlot <- renderPlot({
      summarisedIncidence_plot()
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(final_summarised_result())
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
            list(incidence_table = list(result = final_summarised_result(),
                                        type = "gt",
                                        header = input$header,
                                        groupColumn = input$groupColumn,
                                        settingsColumn = input$settingsColumn)))
        })

        observeEvent(input$incidence_plot, {
          addObject(
            list(incidence_plot = list(result = final_summarised_result(),
                                       x = input$x_axis,
                                       y = "incidence_100000_pys",
                                       line = input$line,
                                       point = input$point,
                                       ribbon = input$ribbon,
                                       ymin = "incidence_100000_pys_95CI_lower",
                                       ymax = "incidence_100000_pys_95CI_upper",
                                       facet = input$facet,
                                       colour = input$colour)))
        })

        return(addObject)

  })
}
