incidenceUI <- function(id, dataset) {
  ns <- NS(id)
  if (id == "Plot - Incidence rate per year") {
    tagList(
      fluidRow(
        column(4,
               selectInput(inputId = ns("facetIncidenceYear"),
                           label = "Select plot type",
                           choices = c("Facet by outcome",
                                       "Facet by database"),
                           selected = "Facet by outcome")
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("washoutIncidenceYear"),
                           label = "Washout",
                           choices = unique(dataset$analysis_outcome_washout),
                           selected = unique(dataset$analysis_outcome_washout)[1],
                           multiple = FALSE)
        ),
        column(4,
               pickerInput(inputId = ns("daysPriorIncidenceYear"),
                           label = "Days Prior History",
                           choices = unique(dataset$denominator_days_prior_observation),
                           selected = unique(dataset$denominator_days_prior_observation)[1],
                           multiple = FALSE)
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = ns("databaseIncidenceYear"),
                           label = "Database",
                           choices = c("All", unique(dataset$incidence_estimates$cdm_name)),
                           selected = "All",
                           multiple = TRUE)
        ),
        column(4,
               pickerInput(inputId = ns("outcomeIncidenceYear"),
                           label = "Outcome",
                           choices = c("All", unique(dataset$outcome_cohort_name)),
                           selected = "All",
                           multiple = TRUE)
        )
      ),
      fluidRow(
        column(4,
               selectInput(inputId = ns("sexIncidenceYear"),
                           label = "Sex",
                           choices = unique(dataset$denominator_sex))
        ),
        column(4,
               selectInput(inputId = ns("ageIncidenceYear"),
                           label = "Age",
                           choices = unique(dataset$denominator_age_group))
        ),
      ),
      fluidRow(
        column(4,
               selectInput(inputId = ns("intervalIncidenceYear"),
                           label = "Interval",
                           choices = unique(dataset$analysis_interval)),
        ),
        column(4,
               selectInput(inputId = ns("repeatedIncidenceYear"),
                           label = "Repeated Events",
                           choices = unique(dataset$analysis_repeated_events)),
        )
      ),
      fluidRow(
        column(4,
               selectInput(inputId = ns("timeFromIncidenceYear"),
                           label = "From",
                           choices = unique(dataset$incidence_start_date),
                           selected = min(unique(dataset$incidence_start_date)))
        ),
        column(4,
               selectInput(inputId = ns("timeToIncidenceYear"),
                           label = "To",
                           choices = unique(dataset$incidence_start_date),
                           selected = max(unique(dataset$incidence_start_date)))
        )
      ),
      fluidRow(
        column(8,
               textAreaInput(inputId = ns("captionIncYear"),
                             label = "Caption",
                             value = "Figure 1. Incidence rate/s of drug/s use over calendar time (per year) overall by database [Add months if relevant]",
                             width = '100%',
                             height = "130px")
        ),
      ),
      fluidRow(
        column(4,
               actionButton(ns("lockDataIncidenceYear"), "Add item to report")
        ),
        column(4,
               downloadButton(ns("downloadFigure1Inc"), "Download Plot")
        ),
      ),
      tags$br(),
      fluidRow(
        column(8,
          plotOutput(ns("previewFigure1"))
        )
      )
    )
  }
}

incidenceServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {

    # Figure 1
    incidenceFigure1 <- reactive({

      objectChoice <- "Plot - Incidence rate per year"
      incidence_estimates <- dataset()
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidenceYear))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidenceYear))
      # Database
      if (length(input$databaseIncidenceYear) != 1 || input$databaseIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(cdm_name %in% c(input$databaseIncidenceYear))
      }
      # Outcome
      if (length(input$outcomeIncidenceYear) != 1 || input$outcomeIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(outcome_cohort_name == input$outcomeIncidenceYear)
      }
      # Sex
      if (length(input$sexIncidenceYear) != 1 || input$sexIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_sex %in% input$sexIncidenceYear)
      }
      # Age group
      if (length(input$ageIncidenceYear) != 1 || input$ageIncidenceYear != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_age_group %in% input$ageIncidenceYear)
      }
      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidenceYear),
                       as.Date(input$timeToIncidenceYear)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidenceYear)
      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidenceYear)

      return(incidence_estimates)
    })

  previewFigure1 <- reactive({
    objectChoice <- "Plot - Incidence rate per year"
    expression <- getItemConfig(input = "title",
                                output = "function",
                                inputValue = objectChoice) %>%
      addPreviewItemType(input$facetIncidenceYear)
    incidence_estimates <- incidenceFigure1()
    eval(parse(text = expression))
  })

  output$previewFigure1 <- renderPlot({
    req(previewFigure1())
    previewFigure1()
  })

  addObject <- reactiveVal()

  observeEvent(input$lockDataIncidenceYear, {
    addObject(
      list(`Plot - Incidence rate per year` = list(incidence_estimates = incidenceFigure1(),
                                                   plotOption = input$facetIncidenceYear,
                                                   caption = input$captionIncYear))
    )
  })

  addObject

  })
}
