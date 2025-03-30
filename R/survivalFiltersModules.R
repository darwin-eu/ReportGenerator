plotFiltersSurvivalUI <- function(id, uploaded_files) {
  ns <- NS(id)
  if (id == "competing_risk") {
    cumulativeFailureOption <- TRUE
  } else {
    cumulativeFailureOption <- FALSE
  }

  # We have to filter "reason"
  strata_column <- testData$CohortSurvival$single_event %>% omopgenerics::strataColumns()
  strata_column <- strata_column[!grepl("reason", strata_column)]

  tagList(
    fluidRow(
      # column(4,
      #        pickerInput(inputId = ns("x_axis"),
      #                    label = "X Axis",
      #                    choices = c("time"),
      #                    selected = c("time"),
      #                    multiple = FALSE)),
      column(4,
             pickerInput(inputId = ns("facet"),
                         label = "Facet",
                         choices = c("cdm_name"),
                         selected = c("cdm_name"),
                         multiple = TRUE)),
      column(4,
             pickerInput(inputId = ns("colour"),
                         label = "Colour",
                         choices = strata_column,
                         selected = strata_column[1],
                         multiple = TRUE)),
    ),
    fluidRow(
      column(2,
             checkboxInput(inputId = ns("cumulative_failure"),
                           label = "Cumulative Failure",
                           value = cumulativeFailureOption,
                           width = NULL)),
      column(2,
             checkboxInput(inputId = ns("ribbon"),
                           label = "Ribbon",
                           value = FALSE,
                           width = NULL)),
      # column(2,
      #        checkboxInput(inputId = ns("risk_table"),
      #                      label = "Risk Table",
      #                      value = FALSE,
      #                      width = NULL)),
      column(3,
             textInput(ns("risk_interval"),
                       label = "Risk Interval",
                       value = "30"))
    ),
    fluidRow(
      column(3,
             actionButton(ns("add_plot"), "Add plot to report"))
    ),
    fluidRow(
      createDownloadPlotUI(ns)
    ),
    tags$br(),
    uiOutput(outputId = ns("caption_plot")),
    tags$br(),
    fluidRow(column(12,
                    shinycssloaders::withSpinner(plotOutput(ns("summarisedPlot")))))
  )
}

tableFiltersSurvivalUI <- function(id, uploaded_files) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             pickerInput(inputId = ns("header"),
                         label = "Header",
                         choices = c("cdm_name",
                                     "group",
                                     "strata",
                                     "additional",
                                     "variable",
                                     "estimate",
                                     "settings"),
                        selected = c("cdm_name", "estimate"),
                        multiple = TRUE)),
      column(6,
             pickerInput(inputId = ns("groupColumn"),
                         label = "Group",
                         choices = c("group", "strata"),
                         selected = c("group"),
                         multiple = TRUE)),
    ),
    # fluidRow(
    #   column(2,
    #          checkboxInput(inputId = ns("split_strata"),
    #                        label = "Split Strata",
    #                        value = TRUE,
    #                        width = NULL)),
    # ),
    fluidRow(createAddItemToReportUI(ns("add_table"))),
    tags$br(),
    uiOutput(outputId = ns("caption_table")),
    tags$br(),
    fluidRow(column(12, shinycssloaders::withSpinner(gt::gt_output(ns("summarisedTable")))))
    )
}
