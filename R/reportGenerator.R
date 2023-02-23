#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
#' @return Dashboard
reportGenerator <- function() {

  ui <- fluidPage(
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    fluidRow(
      column(
        tags$b("Exercise"),
        width = 12,
        bucket_list(
          header = "Drag the items in any desired bucket",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Drag from here",
            labels = list(
              "Table 1 Number of participants",
              "Table 2 Incidence overall",
              "Table 3 Incidence by year",
              "Table 4 Incidence by age group",
              htmltools::tags$div(
                htmltools::em("Complex"), " html tag without a name"
              ),
              "five" = htmltools::tags$div(
                htmltools::em("Complex"), " html tag with name: 'five'"
              )
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

          tags$p("input$rank_list_1"),
          verbatimTextOutput("results_1"),

          tags$p("input$rank_list_2"),
          verbatimTextOutput("results_2"),

          tags$p("input$bucket_list_group"),
          verbatimTextOutput("results_3")
        )
      )
    )
  )

  server <- function(input,output) {
    output$results_1 <-
      renderPrint(
        input$rank_list_1 # This matches the input_id of the first rank list
      )
    output$results_2 <-
      renderPrint(
        input$rank_list_2 # This matches the input_id of the second rank list
      )
    output$results_3 <-
      renderPrint(
        input$bucket_list_group # Matches the group_name of the bucket list
      )
  }


  shinyApp(ui, server)


  }
