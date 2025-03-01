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
                         multiple = TRUE))
    ),
    tags$br(),
    fluidRow(
      tabsetPanel(type = "tabs",
                  tabPanel("Sunburst", br(),
                           fluidRow(column(6, actionButton(ns("lockSunburst"), "Add plot to the report")),
                                    column(6, downloadButton(ns("downloadSunburst"), "Download Plot"))),
                           plotOutput(ns("previewSunburst"))),
                  tabPanel("Sankey", br(),
                           fluidRow(column(6, actionButton(ns("lockSankey"), "Add diagram to the report")),
                                    column(6, downloadButton(ns("downloadSankey"), "Download Plot"))),
                           uiOutput(ns("previewSankey"))))
    )
  )
}

pathwaysServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {

    pathwaysData <- reactive({
      treatmentPathways <- uploaded_files()
      treatmentPathways %>%
        dplyr::filter(cdm_name == input$cdm_name,
                      sex == input$sex,
                      age == input$age,
                      index_year == input$indexYear)
    })

    # Sunburst

    output$previewSunburst <- renderPlot({
      if (nrow(pathwaysData()) > 0) {
        TreatmentPatterns::createSunburstPlot(pathwaysData(),
                                              groupCombinations = FALSE)
        # sunburstPathways(pathwaysData())
      }
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

    observeEvent(input$lockSunburst, {
        addObject(
          list(`Sunburst Plot - TreatmentPatterns` = list(pathwaysData = pathwaysData()))
        )
      })

    observeEvent(input$lockSankey, {
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
          list(`Sankey Diagram - TreatmentPatterns` = list(treatmentPathways = treatmentPathways,
                                                           groupCombinations = TRUE,
                                                           fileImage = sankeyPNG)))
      }
    })
    return(addObject)
  })
}
