mainFiltersLscUI <- function(id, uploaded_files) {
  ns <- NS(id)
  settingsLSC <- settings(uploaded_files)
  # result_id_table_name <- paste(settingsLSC$result_id, settingsLSC$table_name, sep = " - ")
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "Database",
                         choices = unique(uploaded_files$cdm_name),
                         selected = unique(uploaded_files$cdm_name),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      # column(4,
      #        pickerInput(inputId = ns("result_id"),
      #                    label = "Result Id",
      #                    choices = result_id_table_name,
      #                    multiple = FALSE,
      #                    list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      # ),
      column(4,
             pickerInput(inputId = ns("group_name"),
                         label = "Group Name",
                         choices = unique(uploaded_files$group_name),
                         selected = unique(uploaded_files$group_name),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("group_level"),
                         label = "Group Level",
                         choices = unique(uploaded_files$group_level),
                         selected = unique(uploaded_files$group_level)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("strata_name"),
                         label = "Strata Name",
                         choices = unique(uploaded_files$strata_name),
                         selected = unique(uploaded_files$strata_name),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("strata_level"),
                         label = "Strata Level",
                         choices = unique(uploaded_files$strata_level),
                         selected = unique(uploaded_files$strata_level),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("variable_name"),
                         label = "Variable",
                         choices = sort(unique(uploaded_files$variable_name)),
                         selected = unique(uploaded_files$variable_name),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("variable_level"),
                         label = "Variable Level",
                         choices = c("NA", sort(unique(uploaded_files$variable_level))),
                         selected = c("NA", unique(uploaded_files$variable_level)),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("estimate_type"),
                         label = "Estimate Type",
                         choices = sort(unique(uploaded_files$estimate_type)),
                         selected = sort(unique(uploaded_files$estimate_type)),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
     )
    )
}

tableFiltersLscUI <- function(id, uploaded_files) {
  ns <- NS(id)
  captionText <- "Table 2. Baseline characteristics of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
  tagList(
    fluidRow(
      column(6,
             pickerInput(inputId = ns("header"),
                         label = "Header",
                         choices = c("cdm_name", "cohort_name", "strata", "window_name"),
                         selected = c("cdm_name", "cohort_name"),
                         multiple = TRUE)),
      column(6,
             pickerInput(inputId = ns("groupColumn"),
                         label = "Group Column",
                         choices = names(uploaded_files),
                         selected = c("variable_name"),
                         multiple = TRUE)
      )
      ),
    fluidRow(
      createAddItemToReportUI(ns("add_table")),
      column(4, downloadButton(ns("downloadTable"), "Download Table"))
    ),
    fluidRow(
      column(4, numericInput(ns("topConcepts"), "Top n", 10, min = 1, max = 100))
    ),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionCharacteristics"),
                                value = captionText,
                                height = "80px")
      ),
    ),
    fluidRow(
      column(12,
             shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTable"))))
    )
  )
}

plotFiltersLscUI <- function(id, uploaded_files) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("facet"),
                         label = "Facet",
                         choices = c("cdm_name",
                                     "cohort_name"),
                         selected = c("cdm_name", "cohort_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("colour"),
                         label = "Colour",
                         choices = c("variable_level"),
                         selected = "variable_level",
                         multiple = TRUE)),
    ),
    fluidRow(
      column(3,
             actionButton(ns("add_plot"), "Add plot to report"))
    ),
    fluidRow(
      createDownloadPlotUI(ns)
    ),
    fluidRow(column(12,
                    shinycssloaders::withSpinner(plotOutput(ns("summarisedPlot")))))
  )
}
