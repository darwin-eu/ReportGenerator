mainFiltersIncPrevUI <- function(id, uploaded_files) {
  ns <- NS(id)

  uploaded_files_additional <- omopgenerics::splitAdditional(uploaded_files)

  colnames_additional <- uploaded_files_additional %>% colnames()

  if ("analysis_interval" %in% colnames_additional) {
    analysis_interval_switch <- TRUE
    analysis_interval_option <- uploaded_files_additional %>%
      pull(analysis_interval) %>%
      unique()
  } else {
    analysis_interval_switch <- FALSE
  }

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("cdm_name"),
                         label = "CDM Name",
                         choices = unique(uploaded_files$cdm_name),
                         selected = unique(uploaded_files$cdm_name),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("group_level"),
                         label = "Group Level",
                         choices = unique(uploaded_files$group_level),
                         selected = unique(uploaded_files$group_level),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("strata_level"),
                         label = "Strata Level",
                         choices = unique(uploaded_files$strata_level),
                         selected = unique(uploaded_files$strata_level)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("variable_level"),
                         label = "Variable Level",
                         choices = unique(uploaded_files$variable_level),
                         selected = unique(uploaded_files$variable_level)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_target_cohort_name"),
                         label = "Denominator target cohort name",
                         choices = unique(settings(uploaded_files)$denominator_target_cohort_name),
                         selected = unique(settings(uploaded_files)$denominator_target_cohort_name)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_sex"),
                         label = "Sex",
                         choices = unique(settings(uploaded_files)$denominator_sex),
                         selected = unique(settings(uploaded_files)$denominator_sex)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_age_group"),
                         label = "Age Group",
                         choices = unique(settings(uploaded_files)$denominator_age_group),
                         selected = unique(settings(uploaded_files)$denominator_age_group)[1],
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("denominator_time_at_risk"),
                         label = "Denominator Time at Risk",
                         choices = unique(settings(uploaded_files)$denominator_time_at_risk),
                         selected = unique(settings(uploaded_files)$denominator_time_at_risk),
                         multiple = TRUE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      column(4,
             pickerInput(inputId = ns("analysis_type"),
                         label = "Analysis Type",
                         choices = unique(settings(uploaded_files)$analysis_type),
                         selected = unique(settings(uploaded_files)$analysis_type[3]),
                         multiple = FALSE,
                         list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
      ),
      if (analysis_interval_switch) {
        column(4,
               pickerInput(inputId = ns("analysis_interval"),
                           label = "Analysis Interval",
                           choices = analysis_interval_option,
                           selected = analysis_interval_option[1],
                           multiple = TRUE,
                           list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))
        )
      }
    )
  )
}

tableFiltersIncPrevUI <- function(id, uploaded_files) {
  ns <- NS(id)
  captionText <- "Table. New user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
  tagList(
    fluidRow(
      column(3,
             pickerInput(inputId = ns("header"),
                         label = "Header",
                         choices = c("cdm_name",
                                     "denominator_cohort_name",
                                     "outcome_cohort_name",
                                     "incidence_start_date",
                                     "incidence_end_date",
                                     "estimate_name"),,
                         selected = c("cdm_name",
                                      "estimate_name"),
                         multiple = TRUE)),
      column(3,
             pickerInput(inputId = ns("groupColumn"),
                         label = "Group Column",
                         choices = c("cdm_name", "outcome_cohort_name"),
                         selected = c("outcome_cohort_name"),
                         multiple = TRUE)),
      column(3,
             pickerInput(inputId = ns("settingsColumn"),
                         label = "Settings Columns",
                         choices = c("denominator_age_group", "denominator_sex"),
                         selected = c("denominator_age_group", "denominator_sex"),
                         multiple = TRUE)),
      column(3,
             pickerInput(inputId = ns("hide"),
                         label = "Hide Columns",
                         choices = c("denominator_cohort_name", "analysis_interval"),
                         selected = c(),
                         multiple = TRUE))
    ),
    fluidRow(
      column(3,
             actionButton(ns("add_table"), "Add table to report")),
      column(3,
             downloadButton(ns("downloadTable"), "Download Table"))
    ),
    tags$br(),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionTable"),
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

plotFiltersIncPrevUI <- function(id, uploaded_files) {
  ns <- NS(id)
  captionText <- "Figure. Plot of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"
  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("x_axis"),
                         label = "X Axis",
                         choices = c("incidence_start_date",
                                     "denominator_time_at_risk",
                                     "denominator_age_group",
                                     "denominator_sex"),
                         selected = c("incidence_start_date"),
                         multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("facet"),
                         label = "Facet",
                         choices = c("cdm_name",
                                     "denominator_sex",
                                     "denominator_age_group",
                                     "strata_level",
                                     "group",
                                     "outcome_cohort_name"),
                         selected = c("cdm_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("colour"),
                         label = "Colour",
                         choices = c("cdm_name",
                                     "denominator_sex",
                                     "denominator_age_group",
                                     "strata",
                                     "outcome_cohort_name"),
                         selected = "outcome_cohort_name",
                         multiple = TRUE)),
    ),
    fluidRow(
      column(3,
             checkboxInput(inputId = ns("line"),
                           label = "Line",
                           value = TRUE,
                           width = NULL)),
      column(3,
             checkboxInput(inputId = ns("point"),
                           label = "Point",
                           value = TRUE,
                           width = NULL)),
      column(3,
             checkboxInput(inputId = ns("ribbon"),
                           label = "Ribbon",
                           value = FALSE,
                           width = NULL))
    ),
    fluidRow(
      column(3,
             actionButton(ns("add_plot"), "Add plot to report"))
    ),
    fluidRow(
      createDownloadPlotUI(ns)
    ),
    tags$br(),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionPlot"),
                                value = captionText,
                                height = "80px")
      ),
    ),
    fluidRow(column(12,
                    shinycssloaders::withSpinner(plotOutput(ns("summarisedPlot")))))
  )
}

plotPopulationFiltersIncPrevUI <- function(id, uploaded_files) {
  ns <- NS(id)

  inc_prev_label <- tolower(id)
  # Describe in caption text what kind of object is being plotted
  captionText <- "Figure. Plot of new user/s of different medicines at the time of treatment initiation, including pre-specified indication/s"

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("x_axis"),
                         label = "X Axis",
                         choices = c(paste0(inc_prev_label, "_start_date"),
                                     "denominator_count"),
                         selected = paste0(inc_prev_label, "_start_date"),
                         multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("y_axis"),
                         label = "Y Axis",
                         choices = c(paste0(inc_prev_label, "_start_date"),
                                     "denominator_count"),
                         selected = c("denominator_count"),
                         multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("facet"),
                         label = "Facet",
                         choices = c("cdm_name",
                                     "denominator_sex",
                                     "denominator_age_group",
                                     "strata_level",
                                     "group",
                                     "outcome_cohort_name"),
                         selected = c("cdm_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("colour"),
                         label = "Colour",
                         choices = c("cdm_name",
                                     "denominator_sex",
                                     "denominator_age_group",
                                     "strata",
                                     "outcome_cohort_name"),
                         selected = "outcome_cohort_name",
                         multiple = TRUE))
    ),
    fluidRow(
      column(3,
             actionButton(ns("add_population_plot"), "Add plot to report"))
    ),
    fluidRow(
      createDownloadPlotUI(ns, type = "download_population_plot")
    ),
    tags$br(),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionPopulationPlot"),
                                value = captionText,
                                height = "80px")
      ),
    ),
    fluidRow(column(12,
                    shinycssloaders::withSpinner(plotOutput(ns("summarisedPopulationPlot")))))
  )
}
