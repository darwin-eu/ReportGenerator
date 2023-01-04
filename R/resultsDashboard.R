#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard
#' @return Dashboard
resultDashboard <- function(importFolderDenominator = here("inst/csv/denominatorMockData"),
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
                 icon = icon("dashboard"),
                 menuSubItem("Attrition", tabName = "attritionPopTab"),
                 menuSubItem("Incidence", tabName = "incidenceTab"),
                 menuSubItem("Prevalence", tabName = "prevalenceTab")

                 ),
        menuItem("Widgets",
                 tabName = "widgets",
                 icon = icon("th"))
        # selectInput(inputId = "ndatabasePrevalence",
        #             label = "Database",
        #             choices = unique(prevalenceData$database_name)),
        # selectInput(inputId = "ndrugPrevalence",
        #             label = "Drug",
        #             choices = unique(prevalenceData$outcome_cohort_id))
        )
      # textOutput("res")
      ),

    dashboardBody(

            tabItems(

              # First tab content
              tabItem(tabName = "incidenceTab",
                      h3("Incidence Results"),
                      fluidRow(
                           tabBox(
                             title = "",
                             # The id lets us use input$tabset1 on the server to find the current tab
                             id = "tabsetincidence",
                            tabPanel("Table 1",
                                     selectInput(inputId = "sexIncidence",
                                                 label = "Sex",
                                                 choices = c("All", unique(incidenceData$denominator_sex))),
                                     selectInput(inputId = "ageIncidence",
                                                 label = "Age",
                                                 choices = c("All", unique(incidenceData$denominator_age_group))),
                                     selectInput(inputId = "calendarperiodIncidence",
                                                 label = "Calendar period",
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date)))),
                                     selectInput(inputId = "ndatabaseIncidence",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidence",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Table 1",
                                     dataTableOutput("table1Incidence")),
                            tabPanel("Figure 1",
                                     selectInput(inputId = "sexIncidenceFigure1",
                                                 label = "Sex",
                                                 choices = unique(incidenceData$denominator_sex)),
                                     selectInput(inputId = "ageIncidenceFigure1",
                                                 label = "Age",
                                                 choices = unique(incidenceData$denominator_age_group)),
                                     selectInput(inputId = "calendarperiodIncidenceFigure1",
                                                 label = "Calendar period",
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                                                 selected = "All"),
                                     selectInput(inputId = "ndatabaseIncidenceFigure1",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidenceFigure1",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Figure 1",
                                     plotOutput("plot1Incidence")),
                            tabPanel("Figure 2",
                                     selectInput(inputId = "sexIncidenceFigure2",
                                                 label = "Sex",
                                                 choices = c("All", unique(incidenceData$denominator_sex))),
                                     selectInput(inputId = "ageIncidenceFigure2",
                                                 label = "Age",
                                                 choices = unique(incidenceData$denominator_age_group)),
                                     selectInput(inputId = "calendarperiodIncidenceFigure2",
                                                 label = "Calendar period",
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                                                 selected = "All"),
                                     selectInput(inputId = "ndatabaseIncidenceFigure2",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidenceFigure2",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Figure 2",
                                     plotOutput("plot2Incidence")),
                            tabPanel("Figure 3",
                                     selectInput(inputId = "sexIncidenceFigure3",
                                                 label = "Sex",
                                                 choices = unique(incidenceData$denominator_sex)),
                                     selectInput(inputId = "ageIncidenceFigure3",
                                                 label = "Age",
                                                 choices = c("All", unique(incidenceData$denominator_age_group))),
                                     selectInput(inputId = "calendarperiodIncidenceFigure3",
                                                 label = "Calendar period",
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                                                 selected = "All"),
                                     selectInput(inputId = "ndatabaseIncidenceFigure3",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidenceFigure3",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Figure 3",
                                     plotOutput("plot3Incidence")),
                            tabPanel("Figure 4",
                                     selectInput(inputId = "sexIncidenceFigure4",
                                                 label = "Sex",
                                                 choices = c("All", unique(incidenceData$denominator_sex))),
                                     selectInput(inputId = "ageIncidenceFigure4",
                                                 label = "Age",
                                                 choices = c("All", unique(incidenceData$denominator_age_group))),
                                     selectInput(inputId = "calendarperiodIncidenceFigure4",
                                                 label = "Calendar period",
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                                                 selected = "All"),
                                     selectInput(inputId = "ndatabaseIncidenceFigure4",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidenceFigure4",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Figure 4",
                                     plotOutput("plot4Incidence"))

                            )
                        )
                      ),

          # Second tab content
          tabItem(tabName = "prevalenceTab",
                  h3("Prevalence Results"),
                  fluidRow(
                    tabBox(
                      title = "",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tabsetprevalence",
                      tabPanel("Table 1",
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
                               selectInput(inputId = "ndrugPrevalence",
                                           label = "Drug",
                                           choices = unique(prevalenceData$outcome_cohort_id)),
                               "Table 1",
                               dataTableOutput("table1Prevalence")),
                      tabPanel("Figure 1",
                               selectInput(inputId = "sexPrevalenceFigure1",
                                           label = "Sex",
                                           choices = unique(prevalenceData$denominator_sex)),
                               selectInput(inputId = "agePrevalenceFigure1",
                                           label = "Age",
                                           choices = unique(prevalenceData$denominator_age_group)),
                               selectInput(inputId = "calendarperiodPrevalenceFigure1",
                                           label = "Calendar period",
                                           choices = c("All", unique(as.character(prevalenceData$prevalence_start_date))),
                                           selected = "All"),
                               selectInput(inputId = "ndatabasePrevalenceFigure1",
                                           label = "Database",
                                           choices = c("All", unique(prevalenceData$database_name))),
                               selectInput(inputId = "ndrugPrevalenceFigure1",
                                           label = "Drug",
                                           choices = unique(prevalenceData$outcome_cohort_id)),
                               "Figure 1",
                               plotOutput("plot1Prevalence")),
                      tabPanel("Figure 2",
                               selectInput(inputId = "sexPrevalenceFigure2",
                                           label = "Sex",
                                           choices = c("All", unique(prevalenceData$denominator_sex))),
                               selectInput(inputId = "agePrevalenceFigure2",
                                           label = "Age",
                                           choices = unique(prevalenceData$denominator_age_group)),
                               selectInput(inputId = "calendarperiodPrevalenceFigure2",
                                           label = "Calendar period",
                                           choices = c("All", unique(as.character(prevalenceData$prevalence_start_date))),
                                           selected = "All"),
                               selectInput(inputId = "ndatabasePrevalenceFigure2",
                                           label = "Database",
                                           choices = c("All", unique(prevalenceData$database_name))),
                               selectInput(inputId = "ndrugPrevalenceFigure2",
                                           label = "Drug",
                                           choices = unique(prevalenceData$outcome_cohort_id)),
                               "Figure 2",
                               plotOutput("plot2Prevalence")),
                      tabPanel("Figure 3",
                               selectInput(inputId = "sexPrevalenceFigure3",
                                           label = "Sex",
                                           choices = unique(prevalenceData$denominator_sex)),
                               selectInput(inputId = "agePrevalenceFigure3",
                                           label = "Age",
                                           choices = c("All", unique(prevalenceData$denominator_age_group))),
                               selectInput(inputId = "calendarperiodPrevalenceFigure3",
                                           label = "Calendar period",
                                           choices = c("All", unique(as.character(prevalenceData$prevalence_start_date))),
                                           selected = "All"),
                               selectInput(inputId = "ndatabasePrevalenceFigure3",
                                           label = "Database",
                                           choices = c("All", unique(prevalenceData$database_name))),
                               selectInput(inputId = "ndrugPrevalenceFigure3",
                                           label = "Drug",
                                           choices = unique(prevalenceData$outcome_cohort_id)),
                               "Figure 3",
                               plotOutput("plot3Prevalence")),
                      tabPanel("Figure 4",
                               selectInput(inputId = "sexPrevalenceFigure4",
                                           label = "Sex",
                                           choices = c("All", unique(prevalenceData$denominator_sex))),
                               selectInput(inputId = "agePrevalenceFigure4",
                                           label = "Age",
                                           choices = c("All", unique(prevalenceData$denominator_age_group))),
                               selectInput(inputId = "calendarperiodPrevalenceFigure4",
                                           label = "Calendar period",
                                           choices = c("All", unique(as.character(prevalenceData$prevalence_start_date))),
                                           selected = "All"),
                               selectInput(inputId = "ndatabasePrevalenceFigure4",
                                           label = "Database",
                                           choices = c("All", unique(prevalenceData$database_name))),
                               selectInput(inputId = "ndrugPrevalenceFigure4",
                                           label = "Drug",
                                           choices = unique(prevalenceData$outcome_cohort_id)),
                               "Figure 4",
                               plotOutput("plot4Prevalence"))

                    )
                  ))
        )

      )
    )

  server <- function(input, output) {

    # Incidence data

      # Table 1

    dataIncidenceTable1 <- reactive({

      incidenceTableData <- table2Incidence(incidenceData)

      if (input$sexIncidence == "All") {
        incidenceTableData
      } else {
        incidenceTableData <- incidenceTableData %>%
          filter(Sex == input$sexIncidence)
      }

      if (input$ageIncidence == "All") {
        incidenceTableData
      } else {
        incidenceTableData <- incidenceTableData %>%
          filter(`Age group` == input$ageIncidence)
      }

      if (input$calendarperiodIncidence == "All") {
        incidenceTableData
      } else {
        incidenceTableData <- incidenceTableData %>% filter(Time == input$calendarperiodIncidence)
      }

      if (input$ndatabaseIncidence == "All") {
        incidenceTableData
      } else {
        incidenceTableData <- incidenceTableData %>% filter(Database == input$ndatabaseIncidence)
      }

      incidenceTableData

    })

    output$table1Incidence <- renderDataTable(dataIncidenceTable1(),
                                              options = list(
                                                searching = FALSE,
                                                scrollX = TRUE,
                                                autoWidth = TRUE
                                              ))

      # Figure 1

    dataIncidenceFigure1 <- reactive({

      incidenceFigureData <- incidenceData

      if (input$sexIncidenceFigure1 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(denominator_sex == input$sexIncidenceFigure1)
      }

      if (input$ageIncidenceFigure1 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(denominator_age_group == input$ageIncidenceFigure1)
      }

      if (input$calendarperiodIncidenceFigure1 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(incidence_start_date == input$calendarperiodIncidenceFigure1)
      }

      if (input$ndatabaseIncidenceFigure1 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(database_name == input$ndatabaseIncidenceFigure1)
      }

      incidenceFigureData %>%
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

      incidenceFigureData <- incidenceData

      if (input$sexIncidenceFigure2 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(denominator_sex == input$sexIncidenceFigure2)
      }

    if (input$ageIncidenceFigure2 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(denominator_age_group == input$ageIncidenceFigure2)
    }

    if (input$calendarperiodIncidenceFigure2 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(incidence_start_date == input$calendarperiodIncidenceFigure2)
    }

    if (input$ndatabaseIncidenceFigure2 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(database_name == input$ndatabaseIncidenceFigure2)
    }

    incidenceFigureData %>%
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

    incidenceFigureData <- incidenceData

    if (input$sexIncidenceFigure3 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(denominator_sex == input$sexIncidenceFigure3)
    }

    if (input$ageIncidenceFigure3 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(denominator_age_group == input$ageIncidenceFigure3)
    }

    if (input$calendarperiodIncidenceFigure3 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(incidence_start_date == input$calendarperiodIncidenceFigure3)
    }

    if (input$ndatabaseIncidenceFigure3 == "All") {
      incidenceFigureData
    } else {
      incidenceFigureData <- incidenceFigureData %>%
        filter(database_name == input$ndatabaseIncidenceFigure3)
    }

    incidenceFigureData %>%
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

      incidenceFigureData <- incidenceData

      if (input$sexIncidenceFigure4 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(denominator_sex == input$sexIncidenceFigure4)
      }

      if (input$ageIncidenceFigure4 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(denominator_age_group == input$ageIncidenceFigure4)
      }

      if (input$calendarperiodIncidenceFigure4 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(incidence_start_date == input$calendarperiodIncidenceFigure4)
      }

      if (input$ndatabaseIncidenceFigure4 == "All") {
        incidenceFigureData
      } else {
        incidenceFigureData <- incidenceFigureData %>%
          filter(database_name == input$ndatabaseIncidenceFigure4)
      }

      incidenceFigureData %>%
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

    # Table 1

    dataPrevalenceTable1 <- reactive({

      prevalenceTableData <- table4Prevalence(prevalenceData)

      if (input$sexPrevalence == "All") {
        prevalenceTableData
        } else {
          prevalenceTableData <- prevalenceTableData %>%
            filter(Sex == input$sexPrevalence)
        }
      #
      if (input$agePrevalence == "All") {
        prevalenceTableData
        } else {
          prevalenceTableData <- prevalenceTableData %>%
            filter(`Age group` == input$agePrevalence)
      }
      #
      if (input$calendarperiodPrevalence == "All") {
        prevalenceTableData
        } else {
          prevalenceTableData <- prevalenceTableData %>%
            filter(Time == input$calendarperiodPrevalence)
          }
      #
      if (input$ndatabasePrevalence == "All") {
        prevalenceTableData
        } else {
          prevalenceTableData <- prevalenceTableData %>% filter(Database == input$ndatabasePrevalence)
          }

    prevalenceTableData

    })

    output$table1Prevalence <- renderDataTable(dataPrevalenceTable1())

    # Figure 1

    dataprevalenceFigure1 <- reactive({

      prevalenceFigureData <- prevalenceData

      if (input$sexPrevalenceFigure1 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_sex == input$sexPrevalenceFigure1)
      }
      #
      if (input$agePrevalenceFigure1 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_age_group == input$agePrevalenceFigure1)
      }
      #
      if (input$calendarperiodPrevalenceFigure1 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(prevalence_start_date == input$calendarperiodPrevalenceFigure1)
      }
      #
      if (input$ndatabasePrevalenceFigure1 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(database_name == input$ndatabasePrevalenceFigure1)
      }

      prevalenceFigureData %>%
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

    output$plot1Prevalence <- renderPlot(dataprevalenceFigure1(),
                                         options = list(
                                           searching = FALSE,
                                           scrollX = TRUE,
                                           autoWidth = TRUE
                                         ))

    # Figure 2

    dataprevalenceFigure2 <- reactive({

      prevalenceFigureData <- prevalenceData

      if (input$sexPrevalenceFigure2 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_sex == input$sexPrevalenceFigure2)
      }
      #
      if (input$agePrevalenceFigure2 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_age_group == input$agePrevalenceFigure2)
      }
      #
      if (input$calendarperiodPrevalenceFigure2 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(prevalence_start_date == input$calendarperiodPrevalenceFigure2)
      }
      #
      if (input$ndatabasePrevalenceFigure2 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(database_name == input$ndatabasePrevalenceFigure2)
      }

      prevalenceFigureData %>%
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

    dataprevalenceFigure4 <- reactive({

      prevalenceFigureData <- prevalenceData

      if (input$sexPrevalenceFigure4 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_sex == input$sexPrevalenceFigure4)
      }
      #
      if (input$agePrevalenceFigure4 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(denominator_age_group == input$agePrevalenceFigure4)
      }
      #
      if (input$calendarperiodPrevalenceFigure4 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(prevalence_start_date == input$calendarperiodPrevalenceFigure4)
      }
      #
      if (input$ndatabasePrevalenceFigure4 == "All") {
        prevalenceFigureData
      } else {
        prevalenceFigureData <- prevalenceFigureData %>%
          filter(database_name == input$ndatabasePrevalenceFigure4)
      }

      prevalenceFigureData %>%
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
