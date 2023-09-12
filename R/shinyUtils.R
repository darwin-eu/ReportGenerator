tabPanelSelection <- function(selection, uploadedFiles, menuFun) {
  if (selection == "Table - Number of participants") {
    tabPanel(selection, tableNumParFilters(uploadedFiles), tableOutput("previewTable1"))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableSexFilters(uploadedFiles), tableOutput("previewTableSex"))
  } else if (selection == "Plot - Incidence rate per year") {
    tabPanel(selection, incPlotByYearFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure1"))
  } else if (selection == "Plot - Incidence rate per year by sex") {
    tabPanel(selection, incPlotSexFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure2"))
  } else if (selection == "Plot - Incidence rate per year by age") {
    tabPanel(selection, incPlotAgeFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure3"))
  } else if (selection == "Plot - Prevalence rate per year") {
    tabPanel(selection, prevPlotByYearFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure4"))
  } else if (selection == "Plot - Prevalence rate per year by sex") {
    tabPanel(selection, prevPlotSexFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure5"))
  } else if (selection == "Plot - Prevalence rate per year by age") {
    tabPanel(selection, prevPlotAgeFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure6"))
  } else if (selection == "Sankey Diagram - TreatmentPatterns") {
    tabPanel(selection, sankeyDiagramFilters(uploadedFiles), htmlOutput("previewSankeyDiagram"))
  } else if (selection == "Sunburst Plot - TreatmentPatterns") {
    tabPanel(selection, outburstDiagramFilters(uploadedFiles), htmlOutput("previewOutburstPlot"))
  }
}

incPlotByYearFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectInput("facetIncidenceYear",
                         "Select plot type",
                         choices = c("Facet by outcome",
                                     "Facet by database"),
                         selected = "Facet by outcome")
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidenceYear",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceYear",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidenceYear",
                         label = "Sex",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "ageIncidenceYear",
                         label = "Age",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceYear",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceYear",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceYear",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceYear",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataIncidenceYear",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}
incPlotSexFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectizeInput("facetIncidenceSex", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidenceSex",
                         label = "Database",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$cdm_name),
                         selected = uploadedFiles$dataIP$incidence_estimates$cdm_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceSex",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "sexIncidenceSex",
                         label = "Sex",
                         choices = c("Male", "Female"),
                         selected = c("Male", "Female"),
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "ageIncidenceSex",
                         label = "Age",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceSex",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceSex",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceSex",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceSex",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      ),
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataIncidenceSex",
                           label = "Add data to report",
                           value = FALSE)
      )
    )
  )
}

incPlotAgeFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectizeInput("facetIncidenceAge", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidenceAge",
                         label = "Database",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$cdm_name),
                         selected = uploadedFiles$dataIP$incidence_estimates$cdm_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceAge",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidenceAge",
                         label = "Sex",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex))
      ),
      column(4,
             pickerInput(inputId = "ageIncidenceAge",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)),
                         selected = "All",
                         multiple = TRUE)
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceAge",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceAge",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceAge",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceAge",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date)))
      ),
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataIncidenceAge",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}
