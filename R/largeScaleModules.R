largeScaleUI <- function(id, uploaded_files) {
  ns <- NS(id)
    tagList(
      tabsetPanel(type = "tabs",
                  tabPanel("Estimates",
                           tags$br(),
                           # Filters for summarised result and settings
                           mainFiltersLscUI(id = id, uploaded_files),
                           tags$br(),
                           tabsetPanel(type = "tabs",
                                       tabPanel("Table",
                                                tags$br(),
                                                # Filters exclusive for tableIncidence/Prevalence
                                                tableFiltersLscUI(id, uploaded_files)),
                                       tabPanel("Plot",
                                                tags$br(),
                                                # Filters exclusive for plotIncidence/Prevalence
                                                plotFiltersLscUI(id, uploaded_files)),
                                       tabPanel("Data",
                                                # Raw Tidy Data
                                                fluidRow(column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("dataTable"))))))
                                       # fluidRow(column(12, verbatimTextOutput(ns("summarised_text")))))

                           )

                  )
                  # ,
                  # tabPanel("Attrition",
                  #          tags$br(),
                  #          attritionUI(paste(id, "Attrition"), uploaded_files = uploaded_files_attrition)
                  #
                  # )
      )
    )
}

largeScaleServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## DATA

    summarised_result_data <- reactive({
      # result_id_table_name <- input$result_id
      # split_text <- strsplit(result_id_table_name, " - ")
      # result_id_table <- do.call(rbind, split_text)
      # result_id_table <- as.data.frame(result_id_table, stringsAsFactors = FALSE)
      uploaded_files() %>%
        dplyr::filter(cdm_name %in% input$cdm_name,
                      # result_id %in% result_id_table$V1,
                      group_name %in% input$group_name,
                      group_level %in% input$group_level,
                      strata_name %in% input$strata_name,
                      strata_level %in% input$strata_level,
                      variable_name %in% input$variable_name,
                      variable_level %in% input$variable_level,
                      estimate_type %in% input$estimate_type)
      })

    ### OBJECTS CREATION

    # GT TABLE
    summarised_gt_table <- reactive({
      req(summarised_result_data())
      CohortCharacteristics::tableLargeScaleCharacteristics(result = summarised_result_data(),
                                                            topConcepts = input$topConcepts,
                                                            type = "gt",
                                                            header = input$header,
                                                            groupColumn = input$groupColumn,
                                                            hide = character())
      })


    # PLOT
    summarised_plot <- reactive({
      req(summarised_result_data())
      CohortCharacteristics::plotLargeScaleCharacteristics(
        result = summarised_result_data(),
        facet = c("cdm_name", "cohort_name"),
        colour = "variable_level"
      )

    })

    # ------------------------------------------------

    ### OUTPUTS


    # SUMMARISED TABLE
    output$summarisedTable <- gt::render_gt({
      req(summarised_gt_table())
      summarised_gt_table()
    })

    # PLOT
    output$summarisedPlot <- renderPlot({
      req(summarised_plot())
      summarised_plot()
    })

    # RAW DATA
    output$dataTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(summarised_result_data())
    })

    # CAPTION
    output$captionInput <- renderUI({
      createCaptionInput(inputId = ns("captionCharacteristics"),
                         value = captionText())
    })

    ## DOWNLOAD HANDLERS

    # TABLE
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("summarisedLSC", ".docx", sep = "")
      },
      content = function(file) {
        gt::gtsave(summarised_gt_table(), file)
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

    captionText <- reactive({autoCaptionCharac(summarised_result_data())})



    addObject <- reactiveVal()

    observeEvent(input$add_table, {
      addObject(
        list(`Summarised Large Scale Characteristics - Table` = list(result = summarised_result_data(),
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
