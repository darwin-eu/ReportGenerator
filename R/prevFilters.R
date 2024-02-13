prevPlotByYearFilters <- function(uploadedFiles, objectChoice) {
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
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)),
                         selected = "All",
                         multiple = TRUE)
      ),
      column(4,
             pickerInput(inputId = "outcomePrevalenceYear",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalenceYear",
                         label = "Sex",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex))
      ),
      column(4,
             selectInput(inputId = "agePrevalenceYear",
                         label = "Age",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceYear",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceYear",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceYear",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceYear",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(8,
             textAreaInput("captionPrevYear",
                           "Caption",
                           "Figure 4. Prevalence of drug/s use over calendar time (month/year) overall.",
                           width = '100%',
                           height = "130px")
      ),
    ),
    fluidRow(
      column(4,
             actionButton("lockDataPrevalenceYear", "Add item to report")
             # checkboxInput(inputId = "lockDataPrevalenceYear",
             #               label = "Add data to report",
             #               value = FALSE)
      ),
      column(4,
             downloadButton("downloadFigure4Prev", "Download Plot")
      ),
    ),
    tags$br()
  )
}

prevPlotSexFilters <- function(uploadedFiles, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectInput("facetPrevalenceSex", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalenceSex",
                         label = "Database",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name),
                         selected = uploadedFiles$dataIP$prevalence_estimates$cdm_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomePrevalenceSex",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
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
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group))
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceSex",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceSex",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceSex",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceSex",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(8,
             textAreaInput("captionPrevSex",
                           "Caption",
                           "Figure 5. Prevalence of drug/s use over calendar time (per month/year) stratified by sex and age.",
                           width = '100%',
                           height = "130px")
      ),
    ),
    fluidRow(
      column(4,
             actionButton("lockDataPrevalenceSex", "Add item to report")
             # checkboxInput(inputId = "lockDataPrevalenceSex",
             #               label = "Add data to report",
             #               value = FALSE)
      ),
      column(4,
             downloadButton("downloadFigure5Prev", "Download Plot")
      ),
    ),
    tags$br()
  )
}

prevPlotAgeFilters <- function(uploadedFiles, objectChoice) {
  tagList(
    fluidRow(
      column(4,
             selectInput("facetPrevalenceAge", "Select plot type", choices = c("Facet by outcome", "Facet by database"))
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = "databasePrevalenceAge",
                         label = "Database",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name),
                         selected = uploadedFiles$dataIP$prevalence_estimates$cdm_name[1],
                         multiple = TRUE,
                         options = list(
                           maxOptions = 1
                         ))
      ),
      column(4,
             pickerInput(inputId = "outcomePrevalenceAge",
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "sexPrevalenceAge",
                         label = "Sex",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex))
      ),
      column(4,
             pickerInput(inputId = "agePrevalenceAge",
                         label = "Age",
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)),
                         selected = "All",
                         multiple = TRUE)
      ),
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "intervalPrevalenceAge",
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             selectInput(inputId = "typePrevalenceAge",
                         label = "Analysis type",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "timeFromPrevalenceAge",
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             selectInput(inputId = "timeToPrevalenceAge",
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      )
    ),
    fluidRow(
      column(8,
             textAreaInput("captionPrevAge",
                           "Caption",
                           "Figure 6. Prevalence of drug/s use over calendar time (per month/year) stratified by sex and age.",
                           width = '100%',
                           height = "130px")
      ),
    ),
    fluidRow(
      column(4,
             actionButton("lockDataPrevalenceAge", "Add item to report")
             # checkboxInput(inputId = "lockDataPrevalenceAge",
             #               label = "Add data to report",
             #               value = FALSE)
      ),
      column(4,
             downloadButton("downloadFigure6Prev", "Download Plot")
      ),
    ),
    tags$br()
  )
}
