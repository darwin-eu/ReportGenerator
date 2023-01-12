#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets
#' @return Dashboard
resultsDashboard <- function() {

  # denominatorData <- denominatorExtraction(importFolderDenominator)

  # incidenceData <- readRDS(here("inst",
  #                               "data",
  #                               "incidence",
  #                               "mock_data.rds"))

  # incidenceData <- readRDS(here("inst",
  #                               "data",
  #                               "incidence",
  #                               "pso_data.rds"))

  # prevalenceData <- readRDS(here("inst",
  #                                "data",
  #                                "prevalence",
  #                                "mock_data.rds"))

  # prevalenceData <- readRDS(here("inst",
  #                                "data",
  #                                "prevalence",
  #                                "blood_cancer_IPCI.rds"))

  # prevalenceData <- readRDS(here("inst",
  #                                "data",
  #                                "prevalence",
  #                                "blood_cancer_direct.rds"))

  # prevalenceData <- readRDS(here("inst",
  #                                "data",
  #                                "prevalence",
  #                                "blood_cancer.rds"))

  ui <- dashboardPage(
    dashboardHeader(title = "Study Results"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Population level DUS",
                 tabName = "populationlevel",
                 icon = icon("th"),
                 menuSubItem("Incidence",
                             tabName = "incidenceTab"),
                 menuSubItem("Prevalence",
                             tabName = "prevalenceTab")
        ),
        menuItem("Patient Level DUS",
                 tabName = "patientLevel",
                 icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # Incidence tab content
        tabItem(tabName = "incidenceTab",
                fluidRow(
                  uiOutput("incidenceInputSettings")
                  ),
                fluidRow(
                  conditionalPanel(
                    condition = "output.fileUploadIncidence",

                    tabBox(
                      title = "",
                      id = "tabsetincidence",
                      tabPanel("Table 1",
                               downloadButton("downloadIncidence", "Download CSV"),
                               br(),
                               # textOutput("incidenceTable1Paragraph"),
                               dataTableOutput("table1Incidence")),
                      tabPanel("Figure 1",
                               "Figure 1",
                               plotOutput("plot1Incidence")),
                      tabPanel("Figure 2",
                               "Figure 2",
                               plotOutput("plot2Incidence")),
                      tabPanel("Figure 3",
                               "Figure 3",
                               plotOutput("plot3Incidence")),
                      tabPanel("Figure 4",
                               "Figure 4",
                               plotOutput("plot4Incidence"))
                    )
                  )
                )
        ),

        # Prevalence tab content
        tabItem(tabName = "prevalenceTab",
                fluidRow(
                  uiOutput("prevalenceInputSettings")
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "output.fileUploadPrevalence",
                  tabBox(
                    title = "",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabsetprevalence",
                    tabPanel("Table 1",
                             downloadButton("downloadPrevalence", "Download CSV"),
                             # "Table 1",
                             dataTableOutput("table1Prevalence")),
                    tabPanel("Figure 1",
                             "Figure 1",
                             plotOutput("plot1Prevalence")),
                    tabPanel("Figure 2",
                             "Figure 2",
                             plotOutput("plot2Prevalence")),
                    tabPanel("Figure 3",
                             "Figure 3",
                             plotOutput("plot3Prevalence")),
                    tabPanel("Figure 4",
                             "Figure 4",
                             plotOutput("plot4Prevalence")),
                    tabPanel("Attrition table",
                             "Attrition table",
                             dataTableOutput("attritionPrevalence"))
                  )
                )
                )
        )
      )
    )
  )


  server <- function(input, output) {

    # Incidence data

    # Select data (file input method)

    incidenceData <- reactive({

      inFile <- input$datasetIncidence

      if (is.null(inFile)) {

        return(NULL)

      } else {

        incidenceData <- read.csv(inFile$datapath, header = TRUE)

        incidenceData %>%
          mutate(denominator_age_group = gsub(";", "-", denominator_age_group),
                 incidence_start_date = as.Date(incidence_start_date),
                 incidence_end_date = as.Date(incidence_end_date))

      }

    })

    # Select data (dropdown menu)

    # incidenceData <- reactive({
    #
    #   inFile <- input$datasetIncidence
    #
    #     if (is.null(inFile)) {
    #
    #       return(NULL)
    #
    #     } else {
    #
    #       readRDS(here("inst",
    #                    "data",
    #                    "incidence",
    #                    input$datasetIncidence))
    #
    #     }
    #
    # })

    # Check if incidence data is null

    output$fileUploadIncidence <- reactive({

      return(!is.null(incidenceData()))

    })

    outputOptions(output,
                  'fileUploadIncidence',
                  suspendWhenHidden = FALSE)


    # Render UI

    output$incidenceInputSettings <- renderUI({

      box(
        h3("Incidence Results"),

        fluidRow(
          column(4,

                 # FileInput selection

                 fileInput("datasetIncidence",
                           "Choose CSV File",
                           accept = c(".csv")
                           )


        # Dropdown selection

                 # selectInput("datasetIncidence",
                 #             label = "Dataset Incidence",
                 #             choices = list.files(
                 #               here("inst",
                 #                     "data",
                 #                     "incidence"),
                 #                pattern = ".rds",
                 #                full.names = FALSE
                 #                )
                 #              ),



        # --------------------------------------
          )
        ),

        # conditionalPanel shows settings when a file is loaded

        conditionalPanel(
          condition = "output.fileUploadIncidence",

        fluidRow(
          column(4,
                 pickerInput(inputId = "databaseIncidence",
                             label = "Database",
                             choices = c("All", unique(incidenceData()$database_name)),
                             selected = "All",
                             multiple = TRUE)
          ),
          column(4,
                 selectInput(inputId = "outcomeIncidence",
                             label = "Outcome",
                             choices = unique(incidenceData()$outcome_cohort_id))
          )
        ),
        h4("Population settings"),
        fluidRow(
          column(4,
                 selectInput(inputId = "sexIncidence",
                             label = "Sex",
                             choices = c("All", unique(incidenceData()$denominator_sex)))
          ),
          column(4,
                 selectInput(inputId = "ageIncidence",
                             label = "Age",
                             choices = c("All", unique(incidenceData()$denominator_age_group)))
          ),

        ),

        h4("Analysis settings"),
        fluidRow(
          column(4,
                 selectInput(inputId = "intervalIncidence",
                             label = "Interval",
                             choices = unique(incidenceData()$analysis_interval)),
          ),
          column(4,
                 selectInput(inputId = "repeatedIncidence",
                             label = "Repeated Events",
                             choices = unique(incidenceData()$analysis_repeated_events)),
          )
        ),
        h4("Start Time"),
        fluidRow(
          column(4,
                 selectInput(inputId = "timeFromIncidence",
                             label = "From",
                             choices = unique(incidenceData()$incidence_start_date),
                             selected = min(unique(incidenceData()$incidence_start_date)))
          ),
          column(4,
                 selectInput(inputId = "timeToIncidence",
                             label = "To",
                             choices = unique(incidenceData()$incidence_start_date),
                             selected = max(unique(incidenceData()$incidence_start_date)))
                 )
          )
        )
      )


    })


    # Data filter

    incidenceCommonData <- reactive({

      commonData <- incidenceData()

      commonData[is.na(commonData)] = 0

      # Database

      if (length(input$databaseIncidence) == 1 && input$databaseIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(database_name %in% c(input$databaseIncidence))
      }

      # Outcome

      commonData <- commonData %>%
        filter(outcome_cohort_id == input$outcomeIncidence)

      # Sex

      if (input$sexIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexIncidence)
      }

      # Age group

      if (input$ageIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_age_group == input$ageIncidence)
      }

      # Start Time

      commonData <- commonData %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidence),
                       as.Date(input$timeToIncidence)))

      # if (input$timeIncidence == "All") {
      #   commonData
      # } else {
      #   commonData <- commonData %>%
      #     filter(incidence_start_date == input$timeIncidence)
      # }

      # Analysis

      # Interval

      commonData <- commonData %>%
        filter(analysis_interval == input$intervalIncidence)

      # Repeated events

      commonData <- commonData %>%
        filter(analysis_repeated_events == input$repeatedIncidence)

    })

    # Input update

    observe({

      if (input$tabsetincidence == "Table 1") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All", unique(incidenceData()$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData()$denominator_age_group)))

      } else if (input$tabsetincidence == "Figure 1") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidenceData()$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidenceData()$denominator_age_group))

      } else if (input$tabsetincidence == "Figure 2") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidenceData()$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidenceData()$denominator_age_group))

      } else if (input$tabsetincidence == "Figure 3") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidenceData()$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData()$denominator_age_group)))

      } else if (input$tabsetincidence == "Figure 4") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidenceData()$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData()$denominator_age_group)))

      }

    })

    # Downloadable csv of Incidence Data

    output$downloadIncidence <- downloadHandler(
      filename = function() {
        paste("incidence_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(incidenceCommonData(), file, row.names = FALSE)
      }
    )

    # Table 1

    dataIncidenceTable1 <- reactive({

      table2Incidence(incidenceCommonData())

    })

    output$table1Incidence <- renderDataTable(dataIncidenceTable1(),
                                              options = list(
                                                searching = FALSE,
                                                scrollX = TRUE,
                                                autoWidth = TRUE
                                              ))

    IncidenceTable1Text <- reactive({

      textData <- table2Incidence(incidenceCommonData())

      # textAgeGroup <- as.character(input$ageIncidence)

      incidenceText <- paste("Table 1 Describes incidence data of 'outcome 1' for ",
                             "for ",
                             input$ageIncidence,
                             " age groups. ",
                             "Data shown from ",
                             min(textData$Time),
                             " to ",
                             max(textData$Time),
                             ".")

      incidenceText

    })

    output$incidenceTable1Paragraph <- renderText(IncidenceTable1Text())

    # Figure 1

    dataIncidenceFigure1 <- reactive({

      incidenceCommonData() %>%
        ggplot(aes(x = incidence_start_date,
                   y = incidence_100000_pys,
                   col = database_name)) +
        # scale_y_continuous(labels = scales::percent,
        #                    limits = c(0,NA)) +
        geom_line(aes(group = 1)) +
        geom_point() +
        geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower,
                          ymax = incidence_100000_pys_95CI_upper)) +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Incidence rate per 100000 person-years",
             col = "Database name")

    })

    output$plot1Incidence <- renderPlot(dataIncidenceFigure1())

    # Figure 2

    dataIncidenceFigure2 <- reactive({

      incidenceCommonData() %>%
        ggplot(aes(x = incidence_start_date,
                   y = incidence_100000_pys,
                   group = denominator_sex,
                   col = database_name)) +
        facet_grid(cols = vars(denominator_sex)) +
        # scale_y_continuous(labels = scales::percent,
        #                    limits = c(0,NA)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Incidence rate per 100000 person-years",
             col = "Database name")


    })

    output$plot2Incidence <- renderPlot(dataIncidenceFigure2())

    # Figure 3

    dataIncidenceFigure3 <- reactive({

      incidenceCommonData() %>%
        ggplot(aes(x = incidence_start_date,
                   y = incidence_100000_pys)) +
        facet_grid(rows = vars(database_name)) +
        # scale_y_continuous(labels = scales::percent,
        #                    limits = c(0,NA)) +
        geom_line(aes(colour = denominator_age_group)) +
        geom_point() +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Incidence rate per 100000 person-years",
             colour = "Age group")

    })

    output$plot3Incidence <- renderPlot(dataIncidenceFigure3())

    # Figure 4

    dataIncidenceFigure4 <- reactive({

      incidenceCommonData() %>%
        ggplot(aes(x = incidence_start_date,
                   y = incidence_100000_pys,
                   col = database_name)) +
        facet_grid(rows = vars(database_name),
                   cols = vars(denominator_age_group)) +
        # scale_y_continuous(labels = scales::percent,
        #                    limits = c(0,NA)) +
        geom_line(aes(linetype = denominator_sex)) +
        geom_point() +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Incidence rate per 100000 person-years",
             col = "Database name",
             linetype = "Sex") +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 1))

    })

    output$plot4Incidence <- renderPlot(dataIncidenceFigure4())

    # Prevalence Data

    # Select data (file input method)

    prevalenceData <- reactive({

        inFile <- input$datasetPrevalence

        if (is.null(inFile)) {

          return(NULL)

        } else {

          prevalenceData <- read.csv(inFile$datapath,
                                     header = TRUE)

          prevalenceData %>%
            mutate(denominator_age_group = gsub(";", "-", denominator_age_group),
                   prevalence_start_date = as.Date(prevalence_start_date),
                   prevalence_end_date = as.Date(prevalence_end_date))

        }

    })



    # Select database (dropdown menu method)

    # prevalenceData <- reactive({
    #
    #   inFile <- input$datasetPrevalence
    #
    #   if (is.null(inFile)) {
    #
    #     return(NULL)
    #
    #   } else {
    #
    #     readRDS(here("inst",
    #                  "data",
    #                  "prevalence",
    #                  input$datasetPrevalence))
    #
    #   }
    #
    # })

    # Check if prevalence data is null

    output$fileUploadPrevalence <- reactive({

      return(!is.null(prevalenceData()))

    })

    outputOptions(output,
                  'fileUploadPrevalence',
                  suspendWhenHidden = FALSE)

    # Render UI

    output$prevalenceInputSettings <- renderUI({

      box(
          h3("Prevalence Results"),
          fluidRow(
          column(4,

                 # FileInput method

                 fileInput("datasetPrevalence",
                           "Choose CSV File",
                           accept = c(".csv")
                           )

                 # Dropdown selection

                 # selectInput("datasetPrevalence",
                 #             label = "Dataset Prevalence",
                 #             choices = list.files(
                 #               here("inst",
                 #                    "data",
                 #                    "mockPrevalence"),
                 #               pattern = ".rds",
                 #               full.names = FALSE
                 #             ))



                 # -----------------
                 )
          ),

          # conditionalPanel shows settings when a file is loaded

          conditionalPanel(
            condition = "output.fileUploadPrevalence",

            fluidRow(
              column(4,
                     pickerInput(inputId = "databasePrevalence",
                                 label = "Database",
                                 choices = c("All", unique(prevalenceData()$database_name)),
                                 selected = "All",
                                 multiple = TRUE)
                     ),
              column(4,
                     selectInput(inputId = "outcomePrevalence",
                                 label = "Outcome",
                                 choices = unique(prevalenceData()$outcome_cohort_id))
                     )
              ),
          h4("Population settings"),
          fluidRow(
            column(4,
                   selectInput(inputId = "sexPrevalence",
                               label = "Sex",
                               choices = c("All", unique(prevalenceData()$denominator_sex)))
            ),
            column(4,
                   selectInput(inputId = "agePrevalence",
                               label = "Age",
                               choices = c("All", unique(prevalenceData()$denominator_age_group)))
            )
          ),
          fluidRow(
            column(4,
                   selectInput(inputId = "daysPriorPrevalence",
                               label = "Days prior history",
                               choices = unique(prevalenceData()$denominator_days_prior_history))
            )
          ),
          h4("Analysis settings"),
          fluidRow(
            column(4,
                   selectInput(inputId = "typePrevalence",
                               label = "Type",
                               choices = unique(prevalenceData()$analysis_type))
            ),
            column(4,
                   selectInput(inputId = "contributionPrevalence",
                               label = "Full contribution",
                               choices = unique(prevalenceData()$analysis_full_contribution))
            ),
          ),
          h4("Start Time"),
          fluidRow(
            column(4,
                   selectInput(inputId = "timeFromPrevalence",
                               label = "From",
                               choices = unique(prevalenceData()$prevalence_start_date),
                               selected = min(unique(prevalenceData()$prevalence_start_date)))
            ),
            column(4,
                   selectInput(inputId = "timeToPrevalence",
                               label = "To",
                               choices = unique(prevalenceData()$prevalence_start_date),
                               selected = max(unique(prevalenceData()$prevalence_start_date)))
            )
          )
      )
      )

    })

    # Data filter

    prevalenceCommonData <- reactive({

      commonData <- prevalenceData()

      commonData[is.na(commonData)] = 0

      # database

      if (length(input$databasePrevalence) == 1 && input$databasePrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(database_name %in% c(input$databasePrevalence))
      }

      # Outcome

      commonData <- commonData %>%
        filter(outcome_cohort_id == input$outcomePrevalence)

      # Settings

      # Sex

      if (input$sexPrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexPrevalence)
      }

      # Age

      if (input$agePrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_age_group == input$agePrevalence)
      }

      # Time

      commonData <- commonData %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalence),
                       as.Date(input$timeToIncidence)))

      # Days prior history

      commonData <- commonData %>%
        filter(denominator_days_prior_history == input$daysPriorPrevalence)

      # Analysis

      # Type

      commonData <- commonData %>%
        filter(analysis_type == input$typePrevalence)

      # Full contribution

      commonData <- commonData %>%
        filter(analysis_full_contribution == input$contributionPrevalence)

      # Start Time

      commonData <- commonData %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalence),
                       as.Date(input$timeToPrevalence)))

    })

    observe({

      if (input$tabsetprevalence == "Table 1") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_age_group)))

      } else if (input$tabsetprevalence == "Figure 1") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(prevalenceData()$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(prevalenceData()$denominator_age_group))

      } else  if (input$tabsetprevalence == "Figure 2") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(prevalenceData()$denominator_age_group))

      } else if (input$tabsetprevalence == "Figure 3") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(prevalenceData()$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_age_group)))

      } else if (input$tabsetprevalence == "Figure 4") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData()$denominator_age_group)))

      }

    })

    # Downloadable csv of Prevalence Data

    output$downloadPrevalence <- downloadHandler(
      filename = function() {
        paste("prevalence_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(prevalenceCommonData(), file, row.names = FALSE)
      }
    )

    # Table 1

    dataPrevalenceTable1 <- reactive({

      table4Prevalence(prevalenceCommonData())

    })

    output$table1Prevalence <- renderDataTable(dataPrevalenceTable1(),
                                               options = list(
                                                 searching = FALSE,
                                                 scrollX = TRUE,
                                                 autoWidth = TRUE
                                               ))

    # Figure 1

    dataprevalenceFigure1 <- reactive({

      prevalenceCommonData() %>%
        ggplot(aes(x = prevalence_start_date,
                   y = prevalence,
                   col = database_name)) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0,NA)) +
        # facet_grid(rows = vars(outcome_cohort_id)) +
        geom_line(aes(group = 1)) +
        geom_point() +
        geom_errorbar(aes(ymin = prevalence_95CI_lower,
                          ymax = prevalence_95CI_upper)) +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Prevalence",
             col = "Database name")

    })

    output$plot1Prevalence <- renderPlot(dataprevalenceFigure1())

    # Figure 2

    dataprevalenceFigure2 <- reactive({

      prevalenceCommonData() %>%
        ggplot(aes(x = prevalence_start_date,
                   y = prevalence,
                   group = denominator_sex,
                   col = database_name)) +
        facet_grid(cols = vars(denominator_sex)) +
        facet_grid(rows = vars(outcome_cohort_id)) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, NA)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Prevalence ",
             col = "Database name")

    })

    output$plot2Prevalence <- renderPlot(dataprevalenceFigure2())

    # Figure 3

    dataprevalenceFigure3 <- reactive({

      prevalenceCommonData() %>%
        ggplot(aes(x = prevalence_start_date,
                   y = prevalence)) +
        facet_grid(rows = vars(database_name)) +
        facet_grid(rows = vars(outcome_cohort_id)) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, NA)) +
        geom_line(aes(colour = denominator_age_group)) +
        geom_point() +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Prevalence",
             colour = "Age group")

    })

    output$plot3Prevalence <- renderPlot(dataprevalenceFigure3())

    # Figure 4

    dataprevalenceFigure4 <- reactive({

      prevalenceCommonData() %>%
        ggplot(aes(x = prevalence_start_date,
                   y = prevalence,
                   col = database_name)) +
        facet_grid(rows = vars(database_name),
                   cols = vars(denominator_age_group)) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, NA)) +
        geom_line(aes(linetype = denominator_sex)) +
        geom_point() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme_bw() +
        labs(x = "Calendar year",
             y = "Prevalence",
             col = "Database name",
             linetype = "Sex") +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 1))


    })

    output$plot4Prevalence <- renderPlot(dataprevalenceFigure4())

    # Table 1

    attritionPrevalence <- reactive({

      attritionPrevalence <- readRDS(here("inst/data/bloodCancerPrevalence/prevalence_attrition.rds"))

      attritionPrevalence

    })

    output$attritionPrevalence <- renderDataTable(attritionPrevalence(),
                                                  options = list(
                                                    searching = FALSE,
                                                    scrollX = TRUE,
                                                    autoWidth = TRUE
                                                  ))


  }

  shinyApp(ui, server)

}
