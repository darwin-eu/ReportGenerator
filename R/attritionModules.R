attritionUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "Database",
                         choices = unique(uploadedFiles$cdm_name),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("denominator_sex"),
                         label = "Sex",
                         choices = unique(settings(uploadedFiles)$denominator_sex),
                         selected = unique(settings(uploadedFiles)$denominator_sex),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_age_group"),
                         label = "Age Group",
                         choices = unique(settings(uploadedFiles)$denominator_age_group),
                         selected = unique(settings(uploadedFiles)$denominator_age_group),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("header"),
                         label = "Header",
                         choices = names(uploadedFiles),
                         selected = c("variable_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("groupColumn"),
                         label = "Group Column",
                         choices = names(uploadedFiles),
                         selected = c("cdm_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("settingsColumns"),
                         label = "Settings Columns",
                         choices = colnames(settings(uploadedFiles)),
                         selected = c("analysis_type", "denominator_sex", "denominator_age_group"),
                         multiple = TRUE))
    ),
    fluidRow(createAddItemToReportUI(ns("lockAttrition"))),
    fluidRow(
      tabPanel("Data", br(), column(12, gt::gt_output(ns("attritionTable")))))
  )
}


attritionServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result <- reactive({
      uploadedFiles() %>%
        dplyr::filter(cdm_name %in% input$cdm_name) %>%
        visOmopResults::filterSettings(denominator_sex %in% input$denominator_sex,
                                       denominator_age_group %in% input$denominator_age_group)
    })

    if (id == "Incidence Attrition") {
      output$attritionTable <- gt::render_gt({
        req(summarised_result())
        tableIncidenceAttrition(
          result = summarised_result(),
          type = "gt",
          header = input$header,
          groupColumn = input$groupColumn,
          settingsColumns = input$settingsColumns,
          hide = "estimate_name"
        )
      })
    } else if (id == "Prevalence Attrition") {
      output$attritionTable <- gt::render_gt({
        req(summarised_result())
        tablePrevalenceAttrition(
          result = summarised_result(),
          type = "gt",
          header = input$header,
          groupColumn = input$groupColumn,
          settingsColumns = input$settingsColumns,
          # settingsColumns = colnames(settings(summarised_result())),
          hide = "estimate_name"
        )
      })
    }
    # addObject <- reactiveVal()
    #
    # observeEvent(input$lockAttrition, {
    #   addObject(
    #     list(`Cohort Attrition - Table` = list(cohortAttrition = attritionResult()
    #                                            # ,
    #                                            # caption = input$captionCharacteristics
    #                                            ))
    #   )
    # })
    #
    # addObject
  })
}
