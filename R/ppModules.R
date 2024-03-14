characteristicsUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "characteristics") {
    lockName <- "lockSummary"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$cdm_name),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$group_level),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$group_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploadedFiles$dataPP$summarised_characteristics$strata_name),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_characteristics$variable_name)),
                           selected = unique(uploadedFiles$dataPP$summarised_characteristics$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = c("NA", sort(unique(uploadedFiles$dataPP$summarised_characteristics$variable_level))),
                           selected = c("NA", unique(uploadedFiles$dataPP$summarised_characteristics$variable_level)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_characteristics$estimate_type)),
                           selected = sort(unique(uploadedFiles$dataPP$summarised_characteristics$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(12,
               uiOutput(outputId = ns("captionInput"))
               # createCaptionInput(inputId = ns("captionCharacteristics"),
               #                    value = captionText,
               #                    height = "80px")
        ),
      ),
      fluidRow(column(4, actionButton(ns(lockName), "Add item to report"))),
      tags$br(),
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             pickerInput(inputId = ns("pivotWide"),
                                         label = "Arrange by",
                                         choices = c("Group", "Strata"),
                                         selected = c("Group", "Strata"),
                                         multiple = TRUE),
                             column(12, gt::gt_output(ns("summarisedTableGt")))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
        )
      )
    )
  } else {
    lockName <- "lockLSC"
    captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("cdm_name"),
                           label = "Database",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$cdm_name),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$cdm_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("group_level"),
                           label = "Group Level",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_level),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$group_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("strata_name"),
                           label = "Strata Name",
                           choices = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_name),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$strata_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("variable_name"),
                           label = "Variable",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_name)),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("variable_level"),
                           label = "Variable Level",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_level)),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$variable_level),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        ),
        column(4,
               pickerInput(inputId = ns("estimate_type"),
                           label = "Estimate Type",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$estimate_type)),
                           selected = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$estimate_type)),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("table_name"),
                           label = "Table Name",
                           choices = sort(unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$table_name)),
                           selected = unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$table_name),
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      ),
      fluidRow(
        column(12,
               createCaptionInput(inputId = ns("captionCharacteristics"),
                                  value = captionText,
                                  height = "80px")
        ),
      ),
      fluidRow(column(4, actionButton(ns(lockName), "Add item to report"))),
      tags$br(),
      fluidRow(
        tabsetPanel(type = "tabs",
                    tabPanel("Table",
                             br(),
                             pickerInput(inputId = ns("pivotWide"),
                                         label = "Arrange by",
                                         choices = c("Group", "Strata"),
                                         selected = c("Group", "Strata"),
                                         multiple = TRUE),
                             column(12, gt::gt_output(ns("summarisedTableGt")))),
                    tabPanel("Data", br(), column(12, DT::dataTableOutput(ns("summarisedTable"))))
        )
      )
    )
  }
}

createDataTable <- function(data, tableName = "result") {
  DT::datatable(data,
                extensions = 'Buttons',
                options = list(pageLength = 10,
                               paging = TRUE,
                               searching = TRUE,
                               fixedColumns = TRUE,
                               autoWidth = TRUE,
                               ordering = TRUE,
                               scrollX = TRUE,
                               dom = 'Bfrtip',
                               buttons =
                                 list(list(
                                   extend = "collection",
                                   buttons = list(
                                     list(extend = "csv", title = tableName),
                                     list(extend = "excel", title = tableName)),
                                   text = "Download"
                                 ))),
                class = "display")
}

characteristicsServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (id == "characteristics") {
      dataPP <- reactive({
        dataset() %>% filter(cdm_name %in% input$cdm_name,
                             group_level %in% input$group_level,
                             strata_name %in% input$strata_name,
                             variable_name %in% input$variable_name,
                             variable_level %in% input$variable_level,
                             estimate_type %in% input$estimate_type) %>%
          mutate(estimate_value = ifelse(estimate_type == "percentage", round(as.numeric(estimate_value), 2), estimate_value))
      })
    } else {
      dataPP <- reactive({
        dataset() %>% filter(cdm_name %in% input$cdm_name,
                             group_level %in% input$group_level,
                             strata_name %in% input$strata_name,
                             variable_name %in% input$variable_name,
                             variable_level %in% input$variable_level,
                             table_name %in% input$table_name,
                             estimate_type %in% input$estimate_type) %>%
          mutate(estimate_value = ifelse(estimate_type == "percentage", round(as.numeric(estimate_value), 2), estimate_value))
      })
    }

    captionText <- reactive({autoCaptionCharac(dataPP())})

    output$captionInput <- renderUI({
      createCaptionInput(inputId = ns("captionCharacteristics"),
                         value = captionText())
      # textAreaInput(inputId = ns("captionCharacteristics"),
      #               label = "Caption",
      #               value = captionText,
      #               width = '100%',
      #               height = "50px")
    })

    output$summarisedTable <- DT::renderDataTable(server = FALSE, {
      createDataTable(dataPP())
    })

    output$summarisedTableGt <- gt::render_gt({
      gtCharacteristics(summarisedCharacteristics = dataPP(),
                        pivotWide = c("CDM Name", input$pivotWide))
    })

    addObject <- reactiveVal()

    observeEvent(input$lockSummary, {
      addObject(
        list(summarised_characteristics = list(summarisedCharacteristics = dataPP(),
                                                 caption = input$captionCharacteristics))
      )
    })

    observeEvent(input$lockLSC, {
      addObject(
        list(summarised_large_scale_characteristics = list(summarisedCharacteristics = dataPP(),
                                                             caption = input$captionCharacteristics))
      )
    })
    addObject
  })
}
