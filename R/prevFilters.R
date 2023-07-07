prevPlotByYearFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectInput("facetPrevalenceYear",
                         "Select plot type",
                         choices = c("Facet by outcome",
                                     "Facet by database"),
                         selected = "Facet by outcome")
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalenceYear",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             pickerInput(inputId = "outcomePrevalenceYear",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalenceYear",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "agePrevalenceYear",
                         label = "Age",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceYear",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceYear",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceYear",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceYear",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataPrevalenceYear",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}

prevPlotSexFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalenceSex",
                         label = "Database",
                         choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                         selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                         multiple = FALSE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             selectInput(inputId = "outcomePrevalenceSex",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "sexPrevalenceSex",
                         label = "Sex",
                         choices = c("Male", "Female"),
                         selected = c("Male", "Female"),
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "agePrevalenceSex",
                         label = "Age",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceSex",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceSex",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceSex",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceSex",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataPrevalenceSex",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}

prevPlotAgeFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalenceAge",
                         label = "Database",
                         choices = unique(uploadedFiles$data$prevalence_estimates$database_name),
                         selected = uploadedFiles$data$prevalence_estimates$database_name[1],
                         multiple = FALSE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             selectInput(inputId = "outcomePrevalenceAge",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalenceAge",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))
      ),
      column(4,
             pickerInput(inputId = "agePrevalenceAge",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$denominator_age_group)),
                         selected = "All",
                         multiple = TRUE)
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceAge",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceAge",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceAge",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceAge",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockDataPrevalenceAge",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}
