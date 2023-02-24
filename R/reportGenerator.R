#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
reportGenerator <- function() {

  ui <- fluidPage(
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
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
              "Table 1 Number of participants",
              "Table 2 Incidence overall",
              "Table 3 Incidence by year",
              "Table 4 Incidence by age group",
              "Table 5 Incidence by sex"
              # htmltools::tags$div(
              #   htmltools::em("Complex"), " html tag without a name"
              # ),
              # "five" = htmltools::tags$div(
              #   htmltools::em("Complex"), " html tag with name: 'five'"
              # )
            ),
            input_id = "rank_list_1"
          ),
          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = "rank_list_2"
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

          # tags$p("input$rank_list_1"),
          # verbatimTextOutput("results_1"),

          tags$p("Tables and figures in the report"),
          verbatimTextOutput("results_2"),

          actionButton("generateReport", "Generate Report")

          # tags$p("input$bucket_list_group"),
          # verbatimTextOutput("results_3")
        )
      )
    )
  )

  server <- function(input,output) {
    # output$results_1 <-
    #   renderPrint(
    #     input$rank_list_1 # This matches the input_id of the first rank list
    #   )
    output$results_2 <-
      renderPrint(
        input$rank_list_2 # This matches the input_id of the second rank list
      )

    observeEvent(input$generateReport, {

      incidencePrevalenceDocx <- read_docx(path = paste0(system.file(package = "ReportGenerator"),
                                                         "/templates/word/IncidencePrevalenceReport.docx"))

      for (i in input$rank_list_2) {

        if (i == "Table 1 Number of participants") {

          table1 <- table1NumPar(incidence_attrition, prevalence_attrition)

          body_add_flextable(
            incidencePrevalenceDocx,
            value =  table1)

        } else if (i == "Table 2 Incidence overall") {

          table2 <- table2IncOver(incidence_estimates)

          body_add(incidencePrevalenceDocx,
                   value = table2,
                   style = "Normal")

        } else if (i == "Table 3 Incidence by year") {

          table3 <- table3IncYear(incidence_estimates)

          body_add(incidencePrevalenceDocx,
                   value = table3,
                   style = "Normal")

        } else if (i == "Table 4 Incidence by age group") {

          table4 <- table4IncAge(incidence_estimates)

          body_add(incidencePrevalenceDocx,
                   value = table4,
                   style = "Normal")

        } else if (i == "Table 5 Incidence by sex") {

          table4 <- table4IncAge(incidence_estimates)

          body_add(incidencePrevalenceDocx,
                   value = table4,
                   style = "Normal")
        }

      }

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
