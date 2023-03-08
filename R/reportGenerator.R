#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr CDMConnector rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
reportGenerator <- function() {

  itemsList <- read.csv("inst/config/itemsConfig.csv", sep = ";") %>%
    dplyr::mutate(signature = paste0(name, "(", arguments, ")")) %>%
    dplyr::select(title, signature)

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
            labels = itemsList$title,
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
        tableOutput("table"),
          actionButton("generateReport", "Generate Report")
        )
      )
    )
  )

  server <- function(input, output) {

    objectsListData <- reactive({
      Index    <- seq(1:length(input$objectsList2))
      Contents <- input$objectsList2
      objectsDataFrame <- NULL
      if (!is.null(Contents) && !identical(Contents, character(0))) {
        objectsDataFrame <- data.frame(Index, Contents)
      }
      objectsDataFrame
    })

    output$table <- renderTable(objectsListData())

    observeEvent(input$generateReport, {

      incidencePrevalenceDocx <- read_docx(path = here("inst/templates/word/darwinTemplate.docx"))

      # styles_info(incidencePrevalenceDocx)

      reverseList <- rev(input$objectsList2)
      for (i in reverseList) {
        object <- eval(parse(text = itemsList %>%
                               dplyr::filter(title == i) %>%
                               dplyr::pull(signature)))
        if ("flextable" %in% class(object)) {
          body_add_flextable(incidencePrevalenceDocx,
                             value = object)
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")
        } else if ("ggplot" %in% class(object)) {
          body_add(incidencePrevalenceDocx,
                   value = i,
                   style = "Heading 1 (Agency)")
          body_add_gg(x = incidencePrevalenceDocx,
                      value = object,
                      style = "Normal")
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
  }
  shinyApp(ui, server)
}

if(getRversion() >= "2.15.1")    utils::globalVariables(c("incidence_attrition",
                                                          "prevalence_attrition",
                                                          "incidence_estimates"))
