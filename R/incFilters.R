incPlotByYearFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectInput("previewPlotOptionYear",
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
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceYear",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidenceYear",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "ageIncidenceYear",
                         label = "Age",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceYear",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceYear",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceYear",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceYear",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
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
             selectizeInput("previewPlotOptionSex", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
             # facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidenceSex",
                         label = "Database",
                         choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                         selected = uploadedFiles$data$incidence_estimates$database_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceSex",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id)),
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
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceSex",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceSex",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceSex",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceSex",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
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
             selectizeInput("previewPlotOptionAge", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
             # facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidenceAge",
                         label = "Database",
                         choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                         selected = uploadedFiles$data$incidence_estimates$database_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomeIncidenceAge",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id)),
                         selected = "All",
                         multiple = TRUE)
             # selectInput(inputId = "outcomeIncidenceAge",
             #             label = "Outcome",
             #             choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidenceAge",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
      ),
      column(4,
             pickerInput(inputId = "ageIncidenceAge",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$denominator_age_group)),
                         selected = "All",
                         multiple = TRUE)
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidenceAge",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidenceAge",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidenceAge",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidenceAge",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
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
