attritionUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "Database",
                         choices = unique(uploadedFiles$cohortAttrition$cohortAttrition$cdm_name),
                         multiple = FALSE)
      )
    ),
    fluidRow(createAddItemToReportUI(ns("lockAttrition"))),
    fluidRow(
      tabPanel("Data", br(), column(12, dataTableOutput(ns("attritionTable")))))
  )
}


attritionServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    attritionResult <- reactive({
      uploadedFiles <- uploadedFiles()
      summarised_result <- uploadedFiles$cohortAttrition$cohortAttrition
      summarised_result %>%
        dplyr::filter(cdm_name %in% input$cdm_name) %>%
        select(cohort,
               number_records,
               number_subjects,
               reason_id,
               reason,
               excluded_records,
               excluded_subjects)
    })

    output$attritionTable <- renderDataTable(attritionResult())

    addObject <- reactiveVal()

    observeEvent(input$lockAttrition, {
      addObject(
        list(`Cohort Attrition - Table` = list(cohortAttrition = attritionResult()
                                               # ,
                                               # caption = input$captionCharacteristics
                                               ))
      )
    })

    addObject
  })
}
