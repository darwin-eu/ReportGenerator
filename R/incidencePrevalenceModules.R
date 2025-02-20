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
                                     tabPanel("Population",
                                              tags$br(),
                                              # Filters exclusive for plotIncidence/Prevalence
                                              plotPopulationFiltersIncPrevUI(id, uploaded_files)),
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

    ### DATA

    # Filtered data
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

    # Add object
    addObject <- reactiveVal()

    ### OBJECTS C
    ## Creates specific tables and plots for IncPrev
    ## Observe functions to add them to addObject

    if (id == "Incidence") {

      # TABLE
      summarised_gt_table <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::tableIncidence(result = summarised_result_data(),
                                            header = input$header,
                                            groupColumn = input$groupColumn,
                                            settingsColumn = input$settingsColumn,
                                            hide = input$hide)
      })

      # PLOT
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

      # POPULATION PLOT
      summarised_population_plot <- reactive({
        IncidencePrevalence::plotIncidencePopulation(
          result = summarised_result_data(),
          x = input$x_axis,
          y = input$y_axis,
          facet = input$facet,
          colour = input$colour
        )
      })

      # TIDY DATA
      tidy_data <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::asIncidenceResult(summarised_result_data())
      })

      # Add table
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

      # Add plot
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

      # Add population plot
      observeEvent(input$add_population_plot, {
        addObject(
          list(incidence_population_plot = list(result = summarised_result_data(),
                                                x = input$x_axis,
                                                y = input$y_axis,
                                                facet = input$facet,
                                                colour = input$colour)))
      })

    } else if (id == "Prevalence") {

      # TABLE
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

      # PLOT
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

      # POPULATION PLOT
      summarised_population_plot <- reactive({
        IncidencePrevalence::plotPrevalencePopulation(
          result = summarised_result_data(),
          x = input$x_axis,
          y = input$y_axis,
          facet = input$facet,
          colour = input$colour
        )
      })

      # TIDY DATA
      tidy_data <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::asPrevalenceResult(summarised_result_data())
      })

      # Add table
      observeEvent(input$add_table, {
        addObject(
          list(prevalence_table = list(result = summarised_result_data(),
                                       type = "gt",
                                       header = input$header,
                                       groupColumn = input$groupColumn,
                                       settingsColumn = input$settingsColumn,
                                       hide = input$hide)))
      })

      # Add plot
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

      # Add population plot
      observeEvent(input$add_population_plot, {
        addObject(
          list(prevalence_population_plot = list(result = summarised_result_data(),
                                                 x = input$x_axis,
                                                 y = input$y_axis,
                                                 facet = input$facet,
                                                 colour = input$colour)))
      })

    }

    #--------------------------------

    ### OUTPUTS

    # TABLE
    output$summarisedTable <- gt::render_gt({
      req(summarised_gt_table())
      summarised_gt_table()
    })

    # PLOT
    output$summarisedPlot <- renderPlot({
      req(summarised_plot())
      summarised_plot()
    })

    # POPULATION PLOT
    output$summarisedPopulationPlot <- renderPlot({
      req(summarised_population_plot())
      summarised_population_plot()
    })

    # TIDY DATA
    output$dataTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(tidy_data())
    })

    ### DOWNLOAD HANDLERS

    # TABLE
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste(id, "-Table.docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarised_incidence_gt_table(), file)
      }
    )

    # PLOT
    output$download_plot <- downloadHandler(
      filename = function() {
        paste(id, "-Plot.png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = summarised_plot(),
                   height = as.numeric(input$download_plot_height),
                   width = as.numeric(input$download_plot_width),
                   dpi = as.numeric(input$download_plot_dpi))
      }
    )

    # POPULATION PLOT
    output$download_population_plot <- downloadHandler(
      filename = function() {
        paste(id, "-Population-Plot.png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = summarised_population_plot(),
                   height = as.numeric(input$download_population_plot_height),
                   width = as.numeric(input$download_population_plot_width),
                   dpi = as.numeric(input$download_population_plot_dpi))
      }
    )

    return(addObject)
  })
}
