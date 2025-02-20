incidencePrevalenceUI <- function(id, uploaded_files, uploaded_files_attrition) {
  ns <- NS(id)

  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("Estimates",
                         tags$br(),
                         # Filters for summarised result and settings
                         mainFiltersIncPrevUI(id = id, uploaded_files),
                         tags$br(),
                         tabsetPanel(type = "tabs",
                                     tabPanel("Table",
                                              tags$br(),
                                              # Filters exclusive for tableIncidence/Prevalence
                                              tableFiltersIncPrevUI(id, uploaded_files)),
                                     tabPanel("Plot",
                                              tags$br(),
                                              # Filters exclusive for plotIncidence/Prevalence
                                              plotFiltersIncPrevUI(id, uploaded_files)),
                                     tabPanel("Tidy Data",
                                              # Raw Tidy Data
                                              fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("dataTable"))))))
                                     # fluidRow(column(12, verbatimTextOutput(ns("summarised_text")))))

                         )

                ),
                tabPanel("Attrition",
                         tags$br(),
                         attritionUI(paste(id, "Attrition"), uploaded_files = uploaded_files_attrition)

  )
)
)
}

incidencePrevalenceServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result_data <- reactive({
      req(uploaded_files())
      uploaded_files() %>% dplyr::filter(
        # variable_level %in% NA,
        cdm_name %in% input$cdm_name,
        group_level %in% input$group_level,
        strata_level %in% input$strata_level) %>%
        visOmopResults::filterSettings(
          # analysis_type %in% input$analysis_type,
          denominator_target_cohort_name %in% input$denominator_target_cohort_name,
          denominator_sex %in% input$denominator_sex,
          denominator_age_group %in% input$denominator_age_group,
          denominator_time_at_risk %in% input$denominator_time_at_risk)
    })

    addObject <- reactiveVal()

    if (id == "Incidence") {

      summarised_gt_table <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::tableIncidence(result = summarised_result_data(),
                                            header = input$header,
                                            groupColumn = input$groupColumn,
                                            settingsColumn = input$settingsColumn,
                                            hide = input$hide)
      })

      summarised_plot <- reactive({
        IncidencePrevalence::plotIncidence(
          result = summarised_result_data(),
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

      tidy_data <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::asIncidenceResult(summarised_result_data())
      })

      observeEvent(input$add_table, {
        addObject(
          list(incidence_table = list(result = summarised_result_data(),
                                      type = "gt",
                                      header = input$header,
                                      groupColumn = input$groupColumn,
                                      settingsColumn = input$settingsColumn,
                                      hide = input$hide,
                                      .options = list())))
      })

      observeEvent(input$add_plot, {
        addObject(
          list(incidence_plot = list(result = summarised_result_data(),
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

    } else if (id == "Prevalence") {

      summarised_gt_table <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::tablePrevalence(result = summarised_result_data(),
                                             type = "gt",
                                             header = input$header,
                                             groupColumn = input$groupColumn,
                                             settingsColumn = input$settingsColumn,
                                             hide = input$hide,
                                             .options = list())
      })

      summarised_plot <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::plotPrevalence(result = summarised_result_data(),
                                            x = input$x_axis,
                                            y = "prevalence",
                                            line = input$line,
                                            point = input$point,
                                            ribbon = input$ribbon,
                                            ymin = "prevalence_95CI_lower",
                                            ymax = "prevalence_95CI_upper",
                                            facet = input$facet,
                                            colour = input$colour)
      })

      tidy_data <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::asPrevalenceResult(summarised_result_data())
      })

      observeEvent(input$add_table, {
        addObject(
          list(prevalence_table = list(result = summarised_result_data(),
                                       type = "gt",
                                       header = input$header,
                                       groupColumn = input$groupColumn,
                                       settingsColumn = input$settingsColumn,
                                       hide = input$hide)))
      })

      observeEvent(input$add_plot, {
        addObject(
          list(prevalence_plot = list(result = summarised_result_data(),
                                      x = input$x_axis,
                                      y = "prevalence",
                                      line = input$line,
                                      point = input$point,
                                      ribbon = input$ribbon,
                                      ymin = "prevalence_95CI_lower",
                                      ymax = "prevalence_95CI_upper",
                                      facet = input$facet,
                                      colour = input$colour)))
      })

    }

    output$summarisedTable <- gt::render_gt({
      req(summarised_gt_table())
      summarised_gt_table()
    })

    output$summarisedPlot <- renderPlot({
      req(summarised_plot())
      summarised_plot()
    })

    output$dataTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(tidy_data())
    })

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(id, "-Plot.png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = summarised_incidence_plot(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(id, "-Table.docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarised_incidence_gt_table(), file)
      }
    )

    return(addObject)
  })
}
