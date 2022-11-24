#' This application displays an automated and interactive report of the results from the IncidencePrevalence package.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny
#' @return An HTML document
studyReportApp <- function() {

  denominatorData <- denominatorData()
  incidenceData <- incidenceData()
  prevalenceData <- prevalenceData()

  ui <- shiny::fluidPage(

    sidebarLayout(

      sidebarPanel(

        h6("Population level DUS", align = "center"),

        h5(actionLink(inputId = "link",
                    label = "Attrition")),
        h5(actionLink(inputId = "link",
                    label = "Incidence rate")),
        h5(actionLink(inputId = "link",
                      label = "Prevalence")),

        h6("Patient level DUS",
           align = "center"),

        h5(actionLink(inputId = "link",
                      label = "Attrition")),
        h5(actionLink(inputId = "link",
                      label = "Initial dose")),
        h5(actionLink(inputId = "link",
                      label = "Cumulative dose")),
        h5(actionLink(inputId = "link",
                      label = "Treatment duration")),
        h5(actionLink(inputId = "link",
                      label = "Number of prescription")),
        h5(actionLink(inputId = "link",
                      label = "Indication")),
        h5(actionLink(inputId = "link",
                      label = "Characterisation")),
        h5(actionLink(inputId = "link",
                      label = "Summary (Table 1)")),
        h5(actionLink(inputId = "link",
                      label = "Large Scale (Appendix)")),
        selectInput(inputId = "download",
                    label = "Database",
                    choices = unique(incidenceData$database_name)),
        selectInput(inputId = "download",
                    label = "Drug",
                    choices = c("Ranitidine",
                                "Cimetidine",
                                "Famotidine",
                                "All"))),
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
        tabsetPanel(
                    tabPanel("Table", "one"),
                    # tabPanel("Table", tableOutput("table")),
                    tabPanel("Figure", "two")
                    # tabPanel("Figure", plotOutput("plot"))
        ),
        radioButtons("strata", "Stratification",
                     c("Sex" = "sex",
                       "Age" = "age",
                       "Calendar period" = "calendar")),
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
  # output$table <- renderTable({
  #   dataset <- get(input$dataset, "package:datasets")
  #   dataset
  # })
}
shiny::shinyApp(ui, server)

}
