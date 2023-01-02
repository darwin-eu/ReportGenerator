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
                      # fluidRow(
                      #   column(width = 7,
                      #   box(title = "Incidence Results",
                      #       status = "primary",
                      #       solidHeader = TRUE,
                      #       collapsible = TRUE,
                      #       selectInput(inputId = "sexIncidence",
                      #                   label = "Sex",
                      #                   choices = c("All", unique(incidenceData$denominator_sex)),
                      #                   selected = "All"),
                      #       selectInput(inputId = "ageIncidence",
                      #                   label = "Age",
                      #                   choices = c("All", unique(incidenceData$denominator_age_group)),
                      #                   selected = "All"),
                      #       selectInput(inputId = "calendarperiodIncidence",
                      #                   label = "Calendar period",
                      #                   choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                      #                   selected = "All"),
                      #       selectInput(inputId = "ndatabaseIncidence",
                      #                   label = "Database",
                      #                   choices = c("All", unique(incidenceData$database_name))),
                      #       selectInput(inputId = "ndrugIncidence",
                      #                   label = "Drug",
                      #                   choices = unique(incidenceData$outcome_cohort_id))
                      #       )
                      #   )
                      #   ),
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
                                                 choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                                                 selected = "All"),
                                     selectInput(inputId = "ndatabaseIncidence",
                                                 label = "Database",
                                                 choices = c("All", unique(incidenceData$database_name))),
                                     selectInput(inputId = "ndrugIncidence",
                                                 label = "Drug",
                                                 choices = unique(incidenceData$outcome_cohort_id)),
                                     "Table 1",
                                     tableOutput("table1Incidence")),
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
                  h2("prevalenceTab content"))
        )

      )
    )

  server <- function(input, output) {

    # Incidence data

      # Table 1

    dataIncidenceTable1 <- reactive({

      incidenceData <- incidenceData %>% select(database_name,
                                                incidence_start_date,
                                                denominator_sex,
                                                denominator_age_group,
                                                n_events,
                                                person_days,
                                                incidence_100000_pys) %>%
        mutate(incidence_start_date = as.character(incidence_start_date))


      if (input$sexIncidence == "All") {
        incidenceData
      } else {
        incidenceData <- incidenceData %>%
          filter(denominator_sex == input$sexIncidence)
      }

      if (input$ageIncidence == "All") {
        incidenceData
      } else {
        incidenceData <- incidenceData %>%
          filter(denominator_age_group == input$ageIncidence)
      }

      if (input$calendarperiodIncidence == "All") {
        incidenceData
      } else {
        incidenceData <- incidenceData %>% filter(incidence_start_date == input$calendarperiodIncidence)
      }

      if (input$ndatabaseIncidence == "All") {
        incidenceData
      } else {
        incidenceData <- incidenceData %>% filter(database_name == input$ndatabaseIncidence)
      }

      incidenceData

    })

    output$table1Incidence <- renderTable(dataIncidenceTable1())

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


  }

  shinyApp(ui, server)



}
