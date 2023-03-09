#' This application offers a visualization of the figures available to fill in a study report in Word format.
#'
#' @export
#' @import dplyr rmarkdown here ggplot2 quarto shiny shinydashboard shinyWidgets officer sortable
reportGenerator <- function() {

  itemsList <- list(
    "Table - Number of participants" = "table1NumPar(incidence_attrition,
                                                     prevalence_attrition)",
    "Table - Incidence overall" = "table2IncOver(incidence_estimates)",
    "Table - Incidence by year" = "table3IncYear(incidence_estimates)",
    "Table - Incidence by age group" = "table4IncAge(incidence_estimates)",
    "Table - Incidence by sex" = "table5IncSex(incidence_estimates)",
    "Plot - Incidence rate per year" = "incidenceRatePerYearPlot(incidence_estimates)",
    "Plot - Incidence rate per year group by sex" = "incidenceRatePerYearGroupBySexPlot(incidence_estimates)",
    "Plot - Incidence rate per year color by age" = " incidenceRatePerYearColorByAgePlot(incidence_estimates)",
    "Plot - Incidence rate per year facet by database, age group" = "incidenceRatePerYearFacetByDBAgeGroupPlot(incidence_estimates)")

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
            labels = names(itemsList),
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
        object <- eval(parse(text = itemsList[[i]]))
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
