# Copyright 2023 DARWIN EU®
#
# This file is part of ReportGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' ReportGenerator Shiny App
#'
#' `ReportGenerator()` launches the package's main app. The user can upload a zip folder, and the function detects what figures and tables are available to generate a Word report.
#'
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer flextable waldo
#' @importFrom sortable bucket_list add_rank_list
#' @export
reportGenerator <- function() {

  # set max file upload size
  options(shiny.maxRequestSize = 30*1024^2)

  ui <- dashboardPage(
    dashboardHeader(title = "Report Generator"),
    dashboardSidebar(
      sidebarMenu(
        tagList(tags$br(),
                tags$br(),
                tags$div(tags$h4("Load data"), class = "form-group shiny-input-container"),
                fileInput("datasetLoad",
                          "Upload zip folder or csv file",
                          accept = c(".zip", ".csv"),
                          multiple = TRUE),
                actionButton('resetData', 'Reset data')
        ))
    ),
    dashboardBody(
      tabsetPanel(
        tabPanel("Item selection",
        # Item selection menu
        fluidRow(
          box(uiOutput("itemSelectionMenu"),
              tags$br())
        )),
        tabPanel("Item preview",
          fluidRow(
          column(width = 12,
                 fluidRow(
                   box(tags$b("Item preview"),
                       DT::dataTableOutput("tableContents"),
                       actionButton("generateReport", "Generate Report"),
                       width = 4),
                   box(uiOutput("plotFilters"),
                       uiOutput("itemPreview"),
                       width = 8)
                   )
                 )
          )
        )
      )
    )
  )


  server <- function(input,output) {

    # Load data

    # 1. Load files

    # 1.1 Reset data variables

    resetDatasetLoad <- reactiveValues(data = NULL)

    observeEvent(input$datasetLoad, {
      resetDatasetLoad$data <- NULL
    })

    observeEvent(input$resetData, {
      if (!is.null(uploadedFiles())) {
        lapply(uploadedFiles(), unlink)
        resetDatasetLoad$data <- "resetDatasetLoad"
      } else {
        resetDatasetLoad$data <- "resetDatasetLoad"
      }
    })

    # 1.2 Upload files
    uploadedFiles <- reactive({
      inFile <- input$datasetLoad
      applyReset <- resetDatasetLoad$data
      if (is.null(inFile)) {
        return(NULL)
      } else if (!is.null(applyReset)) {
        return(NULL)
      } else if (!is.null(inFile)) {
        uploadedFiles <- inFile$datapath

        if (length(uploadedFiles) == 1) {

          if (grepl(".zip",
                    uploadedFiles,
                    fixed = TRUE)) {
            csvLocation <- tempdir()
            lapply(list.files(path = csvLocation, full.names = TRUE), unlink)
            unzip(uploadedFiles, exdir = csvLocation)
            csvFiles <- list.files(path = csvLocation,
                                   pattern = ".csv",
                                   full.names = TRUE)
          } else {
            csvFiles <- uploadedFiles
          }
      } else if (length(uploadedFiles) > 1) {

        if (grepl(".zip",
                  uploadedFiles[1],
                  fixed = TRUE)) {

          csvLocation <- tempdir()
          lapply(list.files(path = csvLocation, full.names = TRUE), unlink)
          folderNumber <- 0

          for (fileLocation in uploadedFiles) {
            folderNumber <- folderNumber + 1
            unzip(zipfile = fileLocation,
                  exdir = paste0(csvLocation, "/", "database", as.character(folderNumber)))
          }

          databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
          incidence_estimates <- data.frame()
          incidence_attrition <- data.frame()
          prevalence_estimates <- data.frame()
          prevalence_attrition <-  data.frame()

          for (folderLocation in databaseFolders) {
            incidence_estimate_file <- list.files(folderLocation,
                                                  pattern = "incidence_estimates",
                                                  full.names = TRUE)
            incidence_estimate_file <- read.csv(incidence_estimate_file)
            incidence_estimates <- bind_rows(incidence_estimates, incidence_estimate_file)

            incidence_attrition_file <- list.files(folderLocation,
                                                   pattern = "incidence_attrition",
                                                   full.names = TRUE)
            incidence_attrition_file <- read.csv(incidence_attrition_file)
            incidence_attrition <- bind_rows(incidence_attrition, incidence_attrition_file)

            prevalence_estimates_file <- list.files(folderLocation,
                                                    pattern = "prevalence_estimates",
                                                    full.names = TRUE)
            prevalence_estimates_file <- read.csv(prevalence_estimates_file)
            prevalence_estimates <- bind_rows(prevalence_estimates, prevalence_estimates_file)

            prevalence_attrition_file <- list.files(folderLocation,
                                                    pattern = "prevalence_attrition",
                                                    full.names = TRUE)
            prevalence_attrition_file <- read.csv(prevalence_attrition_file)
            prevalence_attrition <- bind_rows(prevalence_attrition, prevalence_attrition_file)
          }

          dir.create(path = paste0(csvLocation, "//", "csvFilesFolder"))
          csvFilesLocation <- paste0(csvLocation, "//", "csvFilesFolder")

          if (dir.exists(csvFilesLocation)) {
            write.csv(incidence_estimates, file = paste0(csvFilesLocation, "//", "incidence_estimates.csv"), row.names = FALSE)
            write.csv(incidence_attrition, file = paste0(csvFilesLocation, "//", "incidence_attrition.csv"), row.names = FALSE)
            write.csv(prevalence_estimates, file = paste0(csvFilesLocation, "//", "prevalence_estimates.csv"), row.names = FALSE)
            write.csv(prevalence_attrition, file = paste0(csvFilesLocation, "//", "prevalence_attrition.csv"), row.names = FALSE)
          }

          csvFiles <- list.files(csvFilesLocation, pattern = ".csv",
                                 full.names = TRUE)

        } else if (grepl(".csv",
                         uploadedFiles[1],
                         fixed = TRUE)) {
          csvFiles <- uploadedFiles
        }
      }
      }
      return(csvFiles)
    })


    # 2. Item selection menu
    # 2.1 Items list
    itemsList <- reactive({
      inFile <- input$datasetLoad
      applyReset <- resetDatasetLoad$data

      uploadedFiles <- uploadedFiles()

      if (is.null(inFile)) {
        return(NULL)
      } else if (!is.null(applyReset)) {
        return(NULL)
      } else if (!is.null(inFile)) {
        itemsList <- c()
        for (i in uploadedFiles) {
          resultsData <- read_csv(i)
          resultsColumns <- names(resultsData)
          configData <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator"))
          configDataTypes <- unique(configData$name)
          for (val in configDataTypes) {
            configColumns <- configData %>% filter(name == val)
            configColumns <- configColumns$variables
            if (length(configColumns) == length(resultsColumns)) {
              if (identical(configColumns, resultsColumns)) {
                itemsList <- append(itemsList, val)
              }
            }
          }
        }
        result <- getItemsList(itemsList)
      }
    })

    output$itemSelectionMenu <- renderUI({

      column(tags$b("Item selection"),
             width = 12,
             bucket_list(header = "Select the figures you want in the report",
                         group_name = "bucket_list_group",
                         orientation = "horizontal",
                         add_rank_list(text = "Drag from here",
                                       labels = itemsList()$title,
                                       input_id = "objectMenu"),
                         add_rank_list(text = "to here",
                                       labels = NULL,
                                       input_id = "objectSelection"
                         )
             )
      )
    })

    # 2.3 Load Data variables

    # Uploaded object: Incidence Attrition
    incidence_attrition <- reactive({
      result <- NULL
      configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%
        dplyr::filter(name == "incidence_attrition")
      configColumns <- configColumns$variables
      for (i in uploadedFiles()) {
        csvData <- read_csv(i)
        resultsColumns <- names(csvData)
        if (length(configColumns) == length(resultsColumns)) {
          if (identical(configColumns, resultsColumns)) {
            result <- csvData
          }
        }
      }
      return(result)
    })

    # Uploaded object: Prevalence Attrition
    prevalence_attrition <- reactive({
      result <- NULL
      configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%
        dplyr::filter(name == "prevalence_attrition")
      configColumns <- configColumns$variables
      for (i in uploadedFiles()) {
        csvData <- read_csv(i)
        resultsColumns <- names(csvData)
        if (length(configColumns) == length(resultsColumns)) {
          if (identical(configColumns, resultsColumns)) {
            result <- csvData
          }
        }
      }
      return(result)
    })

    # Uploaded object: Incidence Estimates
    incidence_estimates <- reactive({
      result <- NULL
      configColumns <- read.csv(system.file("config/variablesConfig.csv", package = "ReportGenerator")) %>%
        dplyr::filter(name == "incidence_estimates")
      configColumns <- configColumns$variables
      for (i in uploadedFiles()) {
        csvData <- read_csv(i)
        resultsColumns <- names(csvData)
        if (length(configColumns) == length(resultsColumns)) {
          if (identical(configColumns, resultsColumns)) {
            result <- csvData
          }
        }
      }
      return(result)
    })

    # IncidenceCommonData
    incidenceCommonData <- reactive({
      commonData <- incidence_estimates()
      commonData[is.na(commonData)] = 0

      # Database
      if (length(input$databaseIncidence) != 1 || input$databaseIncidence != "All") {
        commonData <- commonData %>%
          filter(database_name %in% c(input$databaseIncidence))
      }

      # Outcome
      commonData <- commonData %>%
        filter(outcome_cohort_id == input$outcomeIncidence)

      # Sex
      if (input$sexIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexIncidence)
      }

      # Age group
      if (input$ageIncidence != "All") {
        commonData <- commonData %>%
          filter(denominator_age_group == input$ageIncidence)
      }

      # Start Time
      commonData <- commonData %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidence),
                       as.Date(input$timeToIncidence)))

      # Analysis

      # Interval
      commonData <- commonData %>%
        filter(analysis_interval == input$intervalIncidence)

      # Repeated events
      commonData <- commonData %>%
        filter(analysis_repeated_events == input$repeatedIncidence)
    })

    # 3. Item preview

    # 3.1 Objects list. From the sortable menu.
    menu <- reactive({
      contents <- input$objectSelection
      objectsDataFrame <- data.frame(contents)
      objectsDataFrame
    })

    # Functions available from the configuration file
    menuFun  <- reactive({
      menuFun  <- read.csv(system.file("config/itemsConfig.csv", package = "ReportGenerator"), sep = ";")
      menuFun$arguments <- gsub("incidence_attrition",
                                "incidence_attrition()",
                                menuFun$arguments)

      menuFun$arguments <- gsub("prevalence_attrition",
                                "prevalence_attrition()",
                                menuFun$arguments)

      menuFun$arguments <- gsub("incidence_estimates",
                                "incidenceCommonData()",
                                menuFun$arguments)

      menuFun  <- menuFun  %>% dplyr::mutate(signature = paste0(name, "(", arguments, ")"))
      menuFun
    })

    # 3.2 Preview Table

    # Table of contents to the UI
    output$tableContents <- DT::renderDataTable(createPreviewMenuTable(menu()))

    # Item choice data
    objectChoice  <- reactive({
      req(uploadedFiles())
      req(input$tableContents_cells_selected)

      objectChoiceTitle <- menu()[input$tableContents_cells_selected,]
      objectChoiceTitle
    })

    observe({
      req(objectChoice())

      if (objectChoice() == "Plot - Incidence rate per year") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidence_estimates()$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidence_estimates()$denominator_age_group))

      } else if (objectChoice()== "Plot - Incidence rate per year group by sex") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidence_estimates()$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidence_estimates()$denominator_age_group))

      } else if (objectChoice() == "Plot - Incidence rate per year color by age") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidence_estimates()$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidence_estimates()$denominator_age_group)))

      } else if (objectChoice() == "Plot - Incidence rate per year facet by database, age group") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidence_estimates()$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidence_estimates()$denominator_age_group)))
      }
    })

    # Renders item preview depending on the object class
    output$itemPreview <- renderUI({
      req(objectChoice())

      if (grepl("Table", objectChoice())) {
        tableOutput("previewTable")
      } else {
        plotOptions <- menuFun() %>%
          dplyr::filter(title == objectChoice()) %>%
          getItemOptions()
        if (identical(plotOptions, character(0))) {
          plotlyOutput("previewPlot")
        } else {
          names(plotOptions) <- as.character(glue::glue("{toupper(letters)[1:length(plotOptions)]}: {plotOptions}"))
          tagList(selectizeInput("previewPlotOption", "Select plot type", choices = plotOptions, width = "32%"),
                  plotlyOutput("previewPlot"))
        }
      }
    })

    # Objects to be rendered in the UI

    output$previewTable <- renderTable({
      req(objectChoice())

      if (grepl("Table", objectChoice())) {
        object <- eval(parse(text = menuFun() %>%
                               dplyr::filter(title == objectChoice()) %>%
                               dplyr::pull(signature)))

        object
      }
    })

    output$plotFilters <- renderUI({
      req(objectChoice())

      selectionPlotFilter()

    })

    selectionPlotFilter <- reactive({

      if (req(objectChoice()) == "Table - Number of participants") {

      tagList(
        fluidRow(
          column(4,
                 pickerInput(inputId = "databaseIncidence",
                             label = "Database",
                             choices = c("All", unique(incidence_estimates()$database_name)),
                             selected = "All",
                             multiple = TRUE)
          ),
          column(4,
                 selectInput(inputId = "outcomeIncidence",
                             label = "Outcome",
                             choices = unique(incidence_estimates()$outcome_cohort_id))
          )
        )
      )

      } else {

        tagList(
          fluidRow(
            column(4,
                   pickerInput(inputId = "databaseIncidence",
                               label = "Database",
                               choices = c("All", unique(incidence_estimates()$database_name)),
                               selected = "All",
                               multiple = TRUE)
            ),
            column(4,
                   selectInput(inputId = "outcomeIncidence",
                               label = "Outcome",
                               choices = unique(incidence_estimates()$outcome_cohort_id))
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "sexIncidence",
                               label = "Sex",
                               choices = c("All", unique(incidence_estimates()$denominator_sex)))
            ),
            column(4,
                   selectInput(inputId = "ageIncidence",
                               label = "Age",
                               choices = c("All", unique(incidence_estimates()$denominator_age_group)))
            ),
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "intervalIncidence",
                               label = "Interval",
                               choices = unique(incidence_estimates()$analysis_interval)),
            ),
            column(4,
                   selectInput(inputId = "repeatedIncidence",
                               label = "Repeated Events",
                               choices = unique(incidence_estimates()$analysis_repeated_events)),
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "timeFromIncidence",
                               label = "From",
                               choices = unique(incidence_estimates()$incidence_start_date),
                               selected = min(unique(incidence_estimates()$incidence_start_date)))
            ),
            column(4,
                   selectInput(inputId = "timeToIncidence",
                               label = "To",
                               choices = unique(incidence_estimates()$incidence_start_date),
                               selected = max(unique(incidence_estimates()$incidence_start_date)))
            )
          )
        )

      }

    })

    output$previewPlot<- renderPlotly({
      req(objectChoice())

      menuFunction <- menuFun() %>%
        dplyr::filter(title == objectChoice())
      itemOptions <- menuFunction %>% getItemOptions()
      expression <- menuFunction %>%
        dplyr::pull(signature)
      if (!identical(itemOptions, character(0))) {
        expression <- expression %>%
          addPreviewItemType(input$previewPlotOption)
      }
      object <- eval(parse(text = expression))

      if (grepl("Plot", objectChoice())) {
        object %>%
          plotly::ggplotly() %>%
          increaseFacetStripSize()
      }
    })

    # 4. Word report generator
    observeEvent(input$generateReport, {
      incidencePrevalenceDocx <- read_docx(path = file.path(system.file("templates/word/darwinTemplate.docx", package = "ReportGenerator")))
      reverseList <- rev(menu()$contents)
      for (i in reverseList) {
        menuFunction <- menuFun() %>%
          dplyr::filter(title == i)
        itemOptions <- menuFunction %>% getItemOptions()
        expression <- menuFunction %>%
          dplyr::pull(signature)
        if (!identical(itemOptions, character(0))) {
          expression <- expression %>%
            addPreviewItemType(input$previewPlotOption)
        }
        object <- eval(parse(text = expression))
        if ("flextable" %in% class(object)) {
          body_add_flextable(incidencePrevalenceDocx, value = object)
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")

        } else if ("ggplot" %in% class(object)) {
          body_add_gg(x = incidencePrevalenceDocx,
                      value = object,
                      style = "Normal")
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")

        } else {
          body_add_table(incidencePrevalenceDocx,
                         value = object,
                         style = "TableOverall",
                         header = FALSE)
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")
        }
      }
      body_add_toc(incidencePrevalenceDocx)
      print(incidencePrevalenceDocx,
            target = here("generatedReport.docx"))
    })
  }
  shinyApp(ui, server)
}
