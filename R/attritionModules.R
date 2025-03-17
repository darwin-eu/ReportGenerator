attritionUI <- function(id, uploaded_files) {
  ns <- NS(id)
  captionText <- "Table. New user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "Database",
                         choices = unique(uploaded_files$cdm_name),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("denominator_sex"),
                         label = "Sex",
                         choices = unique(settings(uploaded_files)$denominator_sex),
                         selected = unique(settings(uploaded_files)$denominator_sex),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_age_group"),
                         label = "Age Group",
                         choices = unique(settings(uploaded_files)$denominator_age_group),
                         selected = unique(settings(uploaded_files)$denominator_age_group),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("header"),
                         label = "Header",
                         choices = names(uploaded_files),
                         selected = c("cdm_name", "variable_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("groupColumn"),
                         label = "Group Column",
                         choices = names(uploaded_files),
                         selected = c("group_level"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("settingsColumn"),
                         label = "Settings Columns",
                         choices = colnames(settings(uploaded_files)),
                         selected = c("result_type"),
                         multiple = TRUE))
    ),
    fluidRow(createAddItemToReportUI(ns("lockAttrition"))),
    tags$br(),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionAttrition"),
                                value = captionText,
                                height = "80px")
      ),
    ),
    fluidRow(
      tabPanel("Data", br(), column(12, gt::gt_output(ns("attritionTable")))))
  )
}


attritionServer <- function(id, uploaded_files) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    summarised_result <- reactive({
      uploaded_files() %>%
        dplyr::filter(cdm_name %in% input$cdm_name) %>%
        visOmopResults::filterSettings(denominator_sex %in% input$denominator_sex,
                                       denominator_age_group %in% input$denominator_age_group)
    })

    addObject <- reactiveVal()

    if (id == "Incidence Attrition") {
      output$attritionTable <- gt::render_gt({
        req(summarised_result())
        tableIncidenceAttrition(
          result = summarised_result(),
          type = "gt",
          header = input$header,
          groupColumn = input$groupColumn,
          settingsColumn = input$settingsColumn,
          hide = "estimate_name"
        )
      })

      observeEvent(input$lockAttrition, {
        addObject(
          list("Incidence Attrition - Table" = list(result = summarised_result(),
                                                    type = "gt",
                                                    header = input$header,
                                                    groupColumn = input$groupColumn,
                                                    settingsColumn = input$settingsColumn,
                                                    hide = "estimate_name",
                                                    caption = input$captionAttrition))
        )
      })

    } else if (id == "Prevalence Attrition") {
      output$attritionTable <- gt::render_gt({
        req(summarised_result())
        tablePrevalenceAttrition(
          result = summarised_result(),
          type = "gt",
          header = NULL, # input$header,
          groupColumn = NULL, # input$groupColumn,
          settingsColumn = NULL, # input$settingsColumn,
          hide = "estimate_name"
        )
      })

      observeEvent(input$lockAttrition, {
        addObject(
          list("Prevalence Attrition - Table" = list(result = summarised_result(),
                                                     type = "gt",
                                                     header = NULL, # input$header,
                                                     groupColumn = NULL, # input$groupColumn,
                                                     settingsColumn = NULL, # input$settingsColumn,
                                                     hide = "estimate_name",
                                                     caption = input$captionAttrition))
        )
      })

    }
    return(addObject)
  })
}
