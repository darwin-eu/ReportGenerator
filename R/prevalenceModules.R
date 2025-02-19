prevalenceServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Common Data
    summarised_result_data <- reactive({
      req(uploaded_files())
      uploaded_files() %>% dplyr::filter(# variable_level %in% NA,
        cdm_name %in% input$cdm_name,
        group_level %in% input$group_level,
        strata_level %in% input$strata_level) %>%
        visOmopResults::filterSettings(# analysis_type %in% input$analysis_type,
          denominator_target_cohort_name %in% input$denominator_target_cohort_name,
          denominator_sex %in% input$denominator_sex,
          denominator_age_group %in% input$denominator_age_group,
          denominator_time_at_risk %in% input$denominator_time_at_risk)
    })

    summarised_gt_table <- reactive({
      req(summarised_result_data())
      IncidencePrevalence::tablePrevalence(result = summarised_result_data(),
                                           header = input$header,
                                           groupColumn = input$groupColumn,
                                           settingsColumn = input$settingsColumn,
                                           hide = input$hide,
                                           .options = list())
    })

    output$summarisedTable <- gt::render_gt({
      req(summarised_gt_table())
      summarised_gt_table()
    })

    summarised_plot <- reactive({
      req(summarised_result_data())
      IncidencePrevalence::plotPrevalence(result = summarised_result_data(),
                                          x = "prevalence_start_date",
                                          y = "prevalence",
                                          line = input$line,
                                          point = input$point,
                                          ribbon = input$ribbon,
                                          ymin = "prevalence_95CI_lower",
                                          ymax = "prevalence_95CI_upper",
                                          facet = input$facet,
                                          colour = input$colour)
    })

    output$summarisedPlot <- renderPlot({
      summarised_plot()
    })

    output$dataTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result_data())
    })

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(id, "-plot.png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = summarised_plot(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(id, "-table.docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarised_gt_table(), file)
      }
    )

    addObject <- reactiveVal()
    observeEvent(input$summarised_table, {
      addObject(
        list(prevalence_table = list(result = summarised_result_data(),
                                     type = "gt",
                                     header = input$header,
                                     groupColumn = input$groupColumn,
                                     settingsColumn = input$settingsColumn)))
    })

    observeEvent(input$summarised_plot, {
      addObject(
        list(prevalence_plot = list(result = summarised_result_data(),
                                    ylim = c(0, input$y_limit),
                                    facet = input$facet,
                                    colour = input$colour,
                                    ribbon = input$ribbon)))
    })

    return(addObject)

  })
}
