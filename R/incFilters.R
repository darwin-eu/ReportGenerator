incPlotByYearFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidence",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomeIncidence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidence",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "ageIncidence",
                         label = "Age",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidence",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidence",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidence",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataIncidence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
    )
  )
}

incPlotSexFilters <- function(uploadedFiles, menuFun, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidence",
                         label = "Database",
                         choices = unique(uploadedFiles$data$incidence_estimates$database_name),
                         selected = uploadedFiles$data$incidence_estimates$database_name[1],
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomeIncidence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidence",
                         label = "Sex",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$denominator_sex)))
      ),
      column(4,
             selectInput(inputId = "ageIncidence",
                         label = "Age",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidence",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidence",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidence",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataIncidence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
    )
  )


}

incPlotAgeFilters <- function(uploadedFiles, menuFun, objectChoice) {

  tagList(
    fluidRow(
      column(4,
             facetReturn(menuFun = menuFun, objectChoice = objectChoice)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databaseIncidence",
                         label = "Database",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$database_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             selectInput(inputId = "outcomeIncidence",
                         label = "Outcome",
                         choices = unique(uploadedFiles$data$incidence_estimates$outcome_cohort_id))
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexIncidence",
                         label = "Sex",
                         choices = unique(uploadedFiles$data$incidence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "ageIncidence",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$data$incidence_estimates$denominator_age_group)))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalIncidence",
                         label = "Interval",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "repeatedIncidence",
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$data$incidence_estimates$analysis_repeated_events)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromIncidence",
                         label = "From",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = min(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToIncidence",
                         label = "To",
                         choices = unique(uploadedFiles$data$incidence_estimates$incidence_start_date),
                         selected = max(unique(uploadedFiles$data$incidence_estimates$incidence_start_date)))
      ),
      fluidRow(
        column(4,
               checkboxInput(inputId = "lockDataIncidence",
                             label = "Add data to report",
                             value = FALSE)
        ),
      )
    )
  )
}
