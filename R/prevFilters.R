prevPlotByYearFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalence",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomePrevalence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalence",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "agePrevalence",
                         label = "Age",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalence",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalence",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalence",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataPrevalence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
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
             pickerInput(inputId = "databasePrevalence",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomePrevalence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalence",
                         label = "Sex",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$denominator_sex)))
      ),
      column(4,
             selectInput(inputId = "agePrevalence",
                         label = "Age",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalence",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalence",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalence",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataPrevalence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
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
             pickerInput(inputId = "databasePrevalence",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomePrevalence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$prevalence_estimates$outcome_cohort_name))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalence",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$prevalence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "agePrevalence",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$data$prevalence_estimates$denominator_age_group)))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalence",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$data$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalence",
                         label = "From",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalence",
                         label = "To",
                         choices = unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$data$prevalence_estimates$prevalence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataPrevalence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
    )
  )
}

