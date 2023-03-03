#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
reportGenerator <- function() {

  ui <- fluidPage(
    # tags$head(
    #   tags$style(HTML(".bucket-list-container {min-height: 700px;}"))
    # ),
    fluidRow(
      column(
        width = 12,
        tags$b("Load data"),
        column(
          width = 12,
          fileInput("datasetLoad",
                    "Choose file, folder or ZIP",
                    accept = c(".csv",
                               ".rda",
                               ".zip",
                               ".RData"),
                    multiple = TRUE
          ),
          actionButton('resetData',
                       'Reset data')
        )
      )
    ),
    fluidRow(
      column(
        tags$b("Report Generator"),
        width = 12,
        bucket_list(
          header = "Select the figures you want in the report",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels = list(
              "Table – Number of participants",
              "Table – Incidence overall",
              "Table – Incidence by year",
              "Table – Incidence by age group",
              "Table – Incidence by sex"
              # htmltools::tags$div(
              #   htmltools::em("Complex"), " html tag without a name"
              # ),
              # "five" = htmltools::tags$div(
              #   htmltools::em("Complex"), " html tag with name: 'five'"
              # )
            ),
            input_id = "objectsList1"
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = "objectsList2"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$b("Result"),
        column(
          width = 12,

          # tags$p("input$objectsList1"),
          # verbatimTextOutput("results_1"),

          # tags$p("Tables and figures in the report"),

          verbatimTextOutput("fileList"),

          tableOutput("table"),

          actionButton("generateReport", "Generate Report")

          # tags$p("input$bucket_list_group"),
          # verbatimTextOutput("results_3")
        )
      )
    )
  )

  server <- function(input,output) {

    # Load data

    resetDatasetLoad <- reactiveValues(data = NULL)

    observeEvent(input$datasetLoad, {

      resetDatasetLoad$data <- NULL

      })

    observeEvent(input$resetData, {

      resetDatasetLoad$data <- "resetDatasetLoad"

      })

    studyData <- reactive({

      inFile <- input$datasetLoad

      applyReset <- resetDatasetLoad$data

      if (is.null(inFile)) {

        return(NULL)

      } else if (!is.null(applyReset)) {

        return(NULL)

      } else if (!is.null(inFile)) {

        studyData <- inFile$datapath

        if (grepl(".RData",
                  studyData,
                  fixed = TRUE)) {


          load(studyData)

          "studyData"

        } else if (grepl(".zip",
                        studyData,
                        fixed = TRUE)) {



        }

        # studyData <- bind_rows(
        #   lapply(
        #     inFile$datapath,
        #     read_csv
        #   )
        # )
        #
        # # studyData <- read.csv(inFile$datapath, header = TRUE)
        #
        # studyData %>%
        #   mutate(denominator_age_group = gsub(";", "-", denominator_age_group),
        #          incidence_start_date = as.Date(incidence_start_date),
        #          incidence_end_date = as.Date(incidence_end_date))

      }

    })

    output$fileList <- renderPrint(
        studyData()
      )


    # Objects list


    objectsList <- reactive({

      Index <- seq(1:length(input$objectsList2))

      Contents  <- input$objectsList2

      objectsDataFrame <- data.frame(Index, Contents)

      objectsDataFrame

    })

    output$table <- renderTable(objectsList())


    # Report generator

    observeEvent(input$generateReport, {

      incidencePrevalenceDocx <- read_docx(path = here("inst/templates/word/darwinTemplate.docx"))

      # styles_info(incidencePrevalenceDocx)

      reverseList <- rev(input$objectsList2)

      for (i in reverseList) {

        if (i == "Table – Number of participants") {

          object <- table1NumPar(incidence_attrition,
                                 prevalence_attrition)

        } else if (i == "Table – Incidence overall") {

          object <- table2IncOver(incidence_estimates)

        } else if (i == "Table – Incidence by year") {

          object <- table3IncYear(incidence_estimates)

        } else if (i == "Table – Incidence by age group") {

          object <- table4IncAge(incidence_estimates)

        } else if (i == "Table – Incidence by sex") {

          object <- table4IncAge(incidence_estimates)

        }

        if (class(object) == "flextable" && length(class(object)) == 1) {

        body_add_flextable(incidencePrevalenceDocx,
                           value = object)

        body_add(incidencePrevalenceDocx,
                 value = i,
                 style = "Heading 1 (Agency)")

        } else {

          body_add_table(incidencePrevalenceDocx,
                    value = object,
                    style = "TableOverall")

          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")

        }

      }

      body_add_toc(incidencePrevalenceDocx)

      print(incidencePrevalenceDocx,
            target = here("Reports/generatedReport.docx"))

    })
    # output$results_3 <-
    #   renderPrint(
    #     input$bucket_list_group # Matches the group_name of the bucket list
    #   )

  }


  shinyApp(ui, server)


  }
