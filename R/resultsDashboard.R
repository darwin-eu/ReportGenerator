#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard
#' @return Dashboard
resultsDashboard <- function(importFolderDenominator = here("inst/csv/denominatorMockData"),
                             importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                             importFolderPrevalence = here("inst/csv/prevalenceMockResults")) {

  denominatorData <- denominatorExtraction(importFolderDenominator)

  incidenceData <- incidenceExtraction(importFolderIndcidence)

  prevalenceData <- prevalenceExtraction(importFolderPrevalence)

  ui <- dashboardPage(
    dashboardHeader(title = "Study Results"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",

        menuItem("Population level DUS",
                 tabName = "populationlevel",
                 icon = icon("th"),
                 menuSubItem("Attrition", tabName = "attritionPopTab"),
                 menuSubItem("Incidence", tabName = "incidenceTab"),
                 menuSubItem("Prevalence", tabName = "prevalenceTab")

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
                      h3("Incidence Results"),
                      fluidRow(
                        box(
                          selectInput(inputId = "outcomeIncidence",
                                      label = "Outcome",
                                      choices = unique(incidenceData$outcome_cohort_id)),
                          selectInput(inputId = "sexIncidence",
                                      label = "Sex",
                                      choices = c("All", unique(incidenceData$denominator_sex))),
                          selectInput(inputId = "ageIncidence",
                                      label = "Age",
                                      choices = c("All", unique(incidenceData$denominator_age_group))),
                          selectInput(inputId = "calendarperiodIncidence",
                                      label = "Calendar period",
                                      choices = c("All", unique(as.character(incidenceData$incidence_start_date)))),
                          selectInput(inputId = "databaseNameIncidence",
                                      label = "Database",
                                      choices = c("All", unique(incidenceData$database_name))),
                        )),
                      fluidRow(
                           tabBox(
                             title = "",
                             id = "tabsetincidence",
                            tabPanel("Table 1",
                                     textOutput("incidenceTable1Paragraph"),
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
                      ),

          # Prevalence tab content
          tabItem(tabName = "prevalenceTab",
                  h3("Prevalence Results"),
                  fluidRow(
                    box(
                      selectInput(inputId = "outcomePrevalence",
                                  label = "Outcome",
                                  choices = unique(prevalenceData$outcome_cohort_id)),
                      selectInput(inputId = "sexPrevalence",
                                  label = "Sex",
                                  choices = c("All", unique(prevalenceData$denominator_sex))),
                      selectInput(inputId = "agePrevalence",
                                  label = "Age",
                                  choices = c("All", unique(prevalenceData$denominator_age_group))),
                      selectInput(inputId = "calendarperiodPrevalence",
                                  label = "Calendar period",
                                  choices = c("All", unique(as.character(prevalenceData$prevalence_start_date)))),
                      selectInput(inputId = "ndatabasePrevalence",
                                  label = "Database",
                                  choices = c("All", unique(prevalenceData$database_name))),
                    )
                  ),
                  fluidRow(
                    tabBox(
                      title = "",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabsetprevalence",
                      tabPanel("Table 1",
                               "Table 1",
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
                               plotOutput("plot4Prevalence"))
                      )
                    )
                  )
          )
          )
    )


  server <- function(input, output) {

    # Incidence data

      # Data filter

    incidenceCommonData <- reactive({

      commonData <- incidenceData

      commonData[is.na(commonData)] = 0

      if (input$outcomeIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(outcome_cohort_id == input$outcomeIncidence)
      }

      if (input$sexIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexIncidence)
      }

      if (input$ageIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_age_group == input$ageIncidence)
      }

      if (input$calendarperiodIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(incidence_start_date == input$calendarperiodIncidence)
      }

      if (input$databaseNameIncidence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(database_name == input$databaseNameIncidence)
      }

    })

    # Input update

    observe({

      if (input$tabsetincidence == "Table 1") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All", unique(incidenceData$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData$denominator_age_group)))

      } else if (input$tabsetincidence == "Figure 1") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidenceData$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidenceData$denominator_age_group))

      } else if (input$tabsetincidence == "Figure 2") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidenceData$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = unique(incidenceData$denominator_age_group))

      } else if (input$tabsetincidence == "Figure 3") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = unique(incidenceData$denominator_sex))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData$denominator_age_group)))

      } else if (input$tabsetincidence == "Figure 4") {

        updateSelectInput(inputId = "sexIncidence",
                          choices = c("All",
                                      unique(incidenceData$denominator_sex)))

        updateSelectInput(inputId = "ageIncidence",
                          choices = c("All",
                                      unique(incidenceData$denominator_age_group)))

      }

    })

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

    # Data filter

    prevalenceCommonData <- reactive({

      commonData <- prevalenceData

      commonData[is.na(commonData)] = 0

      commonData <- commonData %>%
        filter(outcome_cohort_id == input$outcomePrevalence)

      if (input$sexPrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_sex == input$sexPrevalence)
      }
      #
      if (input$agePrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(denominator_age_group == input$agePrevalence)
      }
      #
      if (input$calendarperiodPrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(prevalence_start_date == input$calendarperiodPrevalence)
      }
      #
      if (input$ndatabasePrevalence == "All") {
        commonData
      } else {
        commonData <- commonData %>%
          filter(database_name == input$ndatabasePrevalence)
      }

    })

    observe({

      if (input$tabsetprevalence == "Table 1") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_age_group)))

      } else if (input$tabsetprevalence == "Figure 1") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(prevalenceData$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(prevalenceData$denominator_age_group))

      } else  if (input$tabsetprevalence == "Figure 2") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = unique(prevalenceData$denominator_age_group))

      } else if (input$tabsetprevalence == "Figure 3") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = unique(prevalenceData$denominator_sex))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_age_group)))

      } else if (input$tabsetprevalence == "Figure 4") {

        updateSelectInput(inputId = "sexPrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_sex)))

        updateSelectInput(inputId = "agePrevalence",
                          choices = c("All",
                                      unique(prevalenceData$denominator_age_group)))

      }

    })

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

  }

  shinyApp(ui, server)

}
