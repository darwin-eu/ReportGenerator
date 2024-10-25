attritionIncPrevUI <- function(id, uploadedFiles) {
  ns <- NS(id)

  lockName <- "lockTableNumPar"
  cdm_name <- unique(uploadedFiles$attrition$cdm_name)
  group_name <- unique(uploadedFiles$attrition$group_name)
  outputTableName <- "previewTableAtt"

  tagList(
    fluidRow(
      column(4,
             selectInput(inputId = ns("cdm_name"),
                         label = "Analysis ID",
                         choices = cdm_name)
      ),
      column(4,
             selectInput(inputId = ns("group_name"),
                         label = "Analysis ID",
                         choices = group_name)
      ),
    ),
    fluidRow(createAddItemToReportUI(ns(lockName)),
             column(2, numericInput(ns("top_n"), "Top n", 10, min = 1, max = 100))),
    tags$br(),
    fluidRow(
      column(12,
             tableOutput(ns(outputTableName))
      )
    )
  )
}


attritionIncPrevServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {

      attritionCommon <- reactive({
        uploadedFiles <- uploadedFiles()
        uploadedFiles <- uploadedFiles$attrition
        uploadedFiles %>%
          filter(cdm_name == input$cdm_name,
                 group_name == input$group_name)
      })



      output$previewTableAtt <- renderTable({
        attritionCommon()
      })

      # observeEvent(input$lockTablePrevAtt, {
      #   addObject(
      #     list(`Incidence Attrition - Table` = list(prevalence_attrition = prevalenceAttritionCommon(),
      #                                                attritionDataType = attritionDataType,
      #                                                caption = input$captionTableAtt))
      #     )
      #   })

  })
}


