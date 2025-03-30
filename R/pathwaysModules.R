pathwaysUI <- function(id, uploaded_files) {
  ns <- NS(id)

  cdm_name <- unique(uploaded_files$cdm_name)
  sex <- unique(uploaded_files$sex)
  age <- unique(uploaded_files$age)
  indexYear <- unique(uploaded_files$index_year)

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "CDM",
                         choices = cdm_name,
                         selected = cdm_name[1],
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("sex"),
                         label = "Sex",
                         choices = sex,
                         selected = sex[1],
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("age"),
                         label = "Age",
                         choices = age,
                         selected = age[1],
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("indexYear"),
                         label = "Index year",
                         choices = indexYear,
                         selected = indexYear[1],
                         multiple = TRUE)),
    tags$br(),
    fluidRow(
      column(12,
      tabsetPanel(type = "tabs",
                  tabPanel("Sunburst",
                           tags$br(),
                           fluidRow(
                             column(6, actionButton(ns("add_sunburst"), "Add plot to the report")),
                             column(6, downloadButton(ns("downloadSunburst"), "Download Plot"))
                             ),
                           tags$br(),
                           fluidRow(
                             column(12, createCaptionInput(inputId = ns("captionSunburst"), value = "Figure. Sunburst plot"))
                             ),
                           tags$br(),
                           fluidRow(
                             column(12, plotOutput(ns("previewSunburst"))))
                           ),
                  tabPanel("Sankey",
                           tags$br(),
                           fluidRow(column(6, actionButton(ns("add_sankey"), "Add diagram to the report")),
                                    column(6, downloadButton(ns("downloadSankey"), "Download Plot"))),
                           tags$br(),
                           fluidRow(
                             column(12, createCaptionInput(inputId = ns("captionSankey"), value = "Figure. Sankey diagram"))
                           ),
                           fluidRow(
                             column(12, uiOutput(ns("previewSankey"))))
                  )
                  )
    )
    )
  )
  )
}

pathwaysServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {

    pathwaysData <- reactive({
      treatmentPathways <- uploaded_files()
      treatmentPathways$freq <- as.numeric(treatmentPathways$freq)
      treatmentPathways %>%
        dplyr::filter(cdm_name == input$cdm_name,
                      sex == input$sex,
                      age == input$age,
                      index_year == input$indexYear)
    })

    # Sunburst

    # Sunburst Plot
    sunburst_plot <- reactive({
      if (nrow(pathwaysData()) > 0) {

        ggSunburst(treatmentPathways = pathwaysData(),
                   groupCombinations = FALSE,
                   unit = "percent")
      }
    })

    output$previewSunburst <- renderPlot({
      req(sunburst_plot())
      sunburst_plot()
    })

    # Sankey

    output$previewSankey <- renderUI({
      if (nrow(pathwaysData()) > 0) {
        TreatmentPatterns::createSankeyDiagram(pathwaysData(),
                                               groupCombinations = TRUE)
      }
    })

    output$downloadSunburst <- downloadHandler(
      filename = function() {
        paste("SunburstDiagram", ".html", sep = "")
      },
      content = function(file) {
        outputFile <- tempfile(pattern = "sunburstPlot", fileext = ".html")
        treatmentPathways <- pathwaysData()
        if (nrow(treatmentPathways) > 0) {
          outputSunburst <- TreatmentPatterns::createSunburstPlot(
            treatmentPathways = treatmentPathways,
            groupCombinations = TRUE,
          )
          htmlwidgets::saveWidget(outputSunburst, outputFile)
          file.copy(outputFile, file)
        }
      }
    )

    output$downloadSankey <- downloadHandler(
      filename = function() {
        paste("SankeyDiagram", ".html", sep = "")
      },
      content = function(file) {
        outputFile <- tempfile(pattern = "sankeyDiagram", fileext = ".html")
        treatmentPathways <- pathwaysData()
        if (nrow(treatmentPathways) > 0) {
          outputSankey <- TreatmentPatterns::createSankeyDiagram(
            treatmentPathways = treatmentPathways,
            groupCombinations = TRUE)
          htmlwidgets::saveWidget(outputSankey, outputFile)
          file.copy(outputFile, file)
        }
      }
    )

    addObject <- reactiveVal()

    ## For the GGPLOT2 version of the sunburst plot

    # observeEvent(input$add_sunburst, {
    #     addObject(
    #       list(`Sunburst Plot - TreatmentPatterns` = list(pathwaysData(),
    #                                                       groupCombinations = FALSE))
    #     )
    #   })

    ## For the HTML version of the sunburst plot

    observeEvent(input$add_sunburst, {
      # outputFile <- here::here("sunburstDiagram.html")
      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        # outputSunburst <- TreatmentPatterns::createSunburstPlot(
        #   treatmentPathways = treatmentPathways,
        #   groupCombinations = TRUE,
        # )
        # htmlwidgets::saveWidget(outputSunburst, outputFile)
        # sunburstPNG <- tempfile(pattern = "sunburstDiagram", fileext = ".png")
        # webshot2::webshot(
        #   url = outputFile,
        #   file = sunburstPNG,
        #   vwidth = 1200,
        #   vheight = 1200)
        # sunburstPNG <- normalizePath(sunburstPNG)
        addObject(
          list(`Sunburst Plot - TreatmentPatterns` = list(treatmentPathways = pathwaysData(),
                                                          groupCombinations = TRUE,
                                                          unit = "percent",
                                                          caption = input$captionSunburst)))
      }
    })

    observeEvent(input$add_sankey, {
      outputFile <- here::here("sankeyDiagram.html")

      treatmentPathways <- pathwaysData()
      if (nrow(treatmentPathways) > 0) {
        outputSankey <- TreatmentPatterns::createSankeyDiagram(
          treatmentPathways = treatmentPathways,
          groupCombinations = TRUE,
        )
        htmlwidgets::saveWidget(outputSankey, outputFile)
        sankeyPNG <- tempfile(pattern = "sankeyDiagram", fileext = ".png")
        webshot2::webshot(
          url = outputFile,
          file = sankeyPNG,
          vwidth = 1200,
          vheight = 1200)
        sankeyPNG <- normalizePath(sankeyPNG)
        addObject(
          list(`Sankey Diagram - TreatmentPatterns` = list(treatmentPathways = pathwaysData(),
                                                           groupCombinations = TRUE,
                                                           fileImage = sankeyPNG,
                                                           caption = input$captionSankey)))
      }
    })
    return(addObject)
  })
}
