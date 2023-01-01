#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny
#' @return An HTML document
studyReportApp <- function(importFolderDenominator = here("inst/csv/denominatorMockData"),
                           importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                           importFolderPrevalence = here("inst/csv/prevalenceMockResults")) {

  denominatorData <- denominatorExtraction(importFolderDenominator)

  incidenceData <- incidenceExtraction(importFolderIndcidence)

  prevalenceData <- prevalenceExtraction(importFolderPrevalence)

  ui <- fluidPage(

    sidebarLayout(

      sidebarPanel(

        h6("Population level DUS", align = "center"),

        h5(actionLink(inputId = "attritionpop",
                    label = "Attrition")),
        h5(actionLink(inputId = "incidencerate",
                    label = "Incidence rate")),
        h5(actionLink(inputId = "prevalence",
                      label = "Prevalence")),

        h6("Patient level DUS",
           align = "center"),

        h5(actionLink(inputId = "attritionpat",
                      label = "Attrition")),
        h5(actionLink(inputId = "initialdose",
                      label = "Initial dose")),
        h5(actionLink(inputId = "cumulativedose",
                      label = "Cumulative dose")),
        h5(actionLink(inputId = "treatmentduration",
                      label = "Treatment duration")),
        h5(actionLink(inputId = "nprescription",
                      label = "Number of prescription")),
        h5(actionLink(inputId = "indication",
                      label = "Indication")),
        h5(actionLink(inputId = "characterization",
                      label = "Characterisation")),
        h5(actionLink(inputId = "summarytable",
                      label = "Summary (Table 1)")),
        h5(actionLink(inputId = "largescaleappendix",
                      label = "Large Scale (Appendix)")),
        selectInput(inputId = "ndatabase",
                    label = "Database",
                    choices = unique(incidenceData$database_name)),
                    # choices = unique(incidenceData$database_name)),
        selectInput(inputId = "ndrug",
                    label = "Drug",
                    choices = unique(incidenceData$outcome_cohort_id))),
        # id = "tabset",
        # "Population Level DUS",
        #  tabPanel("Attrition"),
        #  tabPanel("Incidence rate"),
        #  tabPanel("Prevalence"),
        # "Patient Level DUS",
        #  tabPanel("Attrition"),
        #  tabPanel("Initial dose"),
        #  tabPanel("Cumulative dose"),
        #  tabPanel("Treatment duration"),
        #  tabPanel("Number of prescription"),
        #  tabPanel("Indication"),
        #  tabPanel("Characterisation"),
        #  tabPanel("Summary (Table 1)"),
        #  tabPanel("Large Scale (Appendix)"),
        # navbarMenu("Database",
        #            tabPanel("IPCI", "four-a"),
        #            tabPanel("CPRD", "four-b"),
        #            tabPanel("Synthea", "four-c"),
        #            tabPanel("All", "four-c"),
        # ),
        # navbarMenu("Drug",
        #            tabPanel("Ranitidine", "four-a"),
        #            tabPanel("Cimetidine", "four-b"),
        #            tabPanel("Famotidine", "four-c"),
        #            tabPanel("All", "four-c")
        # ),
         # tabPanel(selectInput(inputId = "download",
         #                      label = "Database",
         #                      choices = c("IPCI",
         #                                  "CPRD",
         #                                  "Synthea",
         #                                  "All"))),
         # tabPanel(selectInput(inputId = "download",
         #                      label = "Drug",
         #                      choices = c("Ranitidine",
         #                                  "Cimetidine",
         #                                  "Famotidine",
         #                                  "All")))
      mainPanel(
        fluidRow(
        tabsetPanel(

            selectInput(inputId = "sex",
                        label = "Sex",
                        choices = unique(incidenceData$denominator_sex),
                        selected = "Both"),
            selectInput(inputId = "age",
                        label = "Age",
                        choices = unique(incidenceData$denominator_age_group),
                        selected = "0-99"),
            selectInput(inputId = "calendarperiod",
                        label = "Calendar period",
                        choices = c("All", unique(as.character(incidenceData$incidence_start_date))),
                        selected = "All"),
                    # tabPanel("Table", "one"),
                    tabPanel("Table", tableOutput("table")),
                    # tabPanel("Figure", "two"),
                    tabPanel("Figure", plotOutput("plot")),

        ),
        # selectInput(inputId = "download",
        #             label = "Sex",
        #             choices = unique(incidenceData$denominator_sex)),
        # selectInput(inputId = "download",
        #             label = "Age",
        #             choices = unique(incidenceData$denominator_age_group)),
        # selectInput(inputId = "download",
        #             label = "Calendar period",
        #             choices = unique(incidenceData$incidence_start_date)),
        )
        )
      )
    )

server <- function(input, output, session) {
  # output$distPlot <- renderPlot({
  #   hist(rnorm(input$obs))
  # })
  #
  #
  # output$summary <- renderPrint({
  #   dataset <- get(input$dataset, "package:datasets")
  #   summary(dataset)
  # })
  #
  dataIncidenceTable <- reactive({

    incidenceData <- incidenceData %>% select(database_name,
                                               incidence_start_date,
                                               denominator_sex,
                                               denominator_age_group,
                                               n_events,
                                               person_days,
                                               incidence_100000_pys) %>%
      mutate(incidence_start_date = as.character(incidence_start_date)) %>%
      filter(database_name == input$ndatabase,
             denominator_sex == input$sex,
             denominator_age_group == input$age)

    if (input$calendarperiod == "All") {
      incidenceData
    } else {
      incidenceData <- incidenceData %>% filter(incidence_start_date == input$calendarperiod)
    }

    incidenceData

    })

  dataIncidenceFigure1 <- reactive({

    incidenceFigureData <- incidenceData %>%
      filter(denominator_sex == input$sex,
             denominator_age_group == input$age) %>%
      ggplot(aes(x = incidence_start_date,
                 y = incidence_100000_pys,
                 col = input$ndatabase)) +
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

    incidenceFigureData

  })

  output$table <- renderTable(dataIncidenceTable())

  output$plot <- renderPlot(dataIncidenceFigure1())




}
shiny::shinyApp(ui, server)

}
