tableUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionTable"),
                                value = "Table 1. Displays the total number of drug users, for each of the databases, during the study period. Total number of drug A users ranged from XXX to XXX across databases. Total number of drug B users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by sex, number of male drug A users ranged from XXX to XXX  across databases,whereas number of female drug A users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by age, number of drug A users in age group 1 ranged from XXX to XXX across databases, whereas number of drug A users in age group 2 ranged from XXX to XXX across databases. [continue for each age group and each drug ]. In summary, there were more users of [insert drug with highest count]; and least users of [insert drug with lowest count]. [Describe other observed patterns in the data].",
                                height = "130px")
      )
    ),
    fluidRow(createAddItemToReportUI(ns("lockTable")),
            column(2, numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100))),
    tags$br(),
    tabsetPanel(type = "tabs",
                tabPanel("Table",
                         fluidRow(column(12, gt_output(ns("previewTable"))))),
                tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("inputDataTable"))))
    )
  )
}

tableServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {

    getData <- reactive({
      uploadedFiles <- uploadedFiles()
      uploadedFiles$dataIP$incidence_estimates
    })

    getTopData <- reactive({
      result <- getData()
      if (!is.na(input$top_n)) {
        result <- result %>% dplyr::slice_head(n = input$top_n)
      }
      result
    })

    output$inputDataTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(getTopData())
    })

    output$previewTable <- render_gt({
      incidence_estimates <- getData()
      eval(parse(text = getItemConfig(input = "title",
                                      output = "function",
                                      inputValue = id)))
    })

    addObject <- reactiveVal()
    observeEvent(input$lockTable, {
      addObject(
        list(`Table - Number of participants by sex and age group` = list(incidence_estimates = getTopData(),
                                                                          caption = input$captionTable))
      )
    })
    return(addObject)
  })
}
