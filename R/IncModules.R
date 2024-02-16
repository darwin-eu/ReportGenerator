incidenceUI <- function(id, dataset) {
  if (id == "Plot - Incidence rate per year") {

    databaseChoices <- c("All", unique(dataset$cdm_name))
    databaseSelected <- ("All")
    databaseOptions <- list()

    sexChoices <- unique(dataset$denominator_sex)
    sexSelected <-  unique(dataset$denominator_sex)[1]
    sexMultiple <- FALSE

    ageChoices <- unique(dataset$denominator_age_group)
    ageSelected <- unique(dataset$denominator_age_group)[1]
    ageMultiple <- FALSE

    captionValue <-"Figure 1. Incidence rate/s of drug/s use over calendar time (per year) overall by database [Add months if relevant]"

    lockVariable <- "lockDataIncidenceYear"

  } else if (id == "Plot - Incidence rate per year by sex") {

    databaseChoices <- unique(dataset$cdm_name)
    databaseSelected <- dataset$cdm_name[1]
    databaseOptions <- list(maxOptions = 1)

    sexChoices <- c("Male", "Female")
    sexSelected <- c("Male", "Female")
    sexMultiple <- TRUE

    ageChoices <- unique(dataset$denominator_age_group)
    ageSelected <- unique(dataset$denominator_age_group)[1]
    ageMultiple <- FALSE

    captionValue <- "Figure 2. Incidence rate/s of drug/s use over calendar time (per year) stratified by sex and database [Add months if relevant]"

    lockVariable <- "lockDataIncidenceSex"

  } else if (id == "Plot - Incidence rate per year by age") {

    databaseChoices <- unique(dataset$cdm_name)
    databaseSelected <- dataset$cdm_name[1]
    databaseOptions <- list(maxOptions = 1)

    sexChoices <- unique(dataset$denominator_sex)
    sexSelected <-  unique(dataset$denominator_sex)[1]
    sexMultiple <- FALSE

    ageChoices <- c("All", unique(dataset$denominator_age_group))
    ageSelected <- "All"
    ageMultiple <- TRUE

    captionValue <- "Figure 3. Incidence rate/s of drug/s use over calendar time (per year) overall stratified by age and database [Add months if relevant]"

    lockVariable <- "lockDataIncidenceAge"

  }

    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "facetIncidence"),
                           label = "Select plot type",
                           choices = c("Facet by outcome", "Facet by database"),
                           selected = "Facet by outcome")
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "washoutIncidence"),
                           label = "Washout",
                           choices = unique(dataset$analysis_outcome_washout),
                           selected = unique(dataset$analysis_outcome_washout)[1],
                           multiple = FALSE)
        ),
        column(4,
               pickerInput(inputId = NS(id, "daysPriorIncidence"),
                           label = "Days Prior History",
                           choices = unique(dataset$denominator_days_prior_observation),
                           selected = unique(dataset$denominator_days_prior_observation)[1],
                           multiple = FALSE)
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "databaseIncidence"),
                           label = "Database",
                           choices = databaseChoices,
                           selected = databaseSelected,
                           multiple = TRUE,
                           options = databaseOptions)
        ),
        column(4,
               pickerInput(inputId = NS(id, "outcomeIncidence"),
                           label = "Outcome",
                           choices = c("All", unique(dataset$outcome_cohort_name)),
                           selected = "All",
                           multiple = TRUE)
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "sexIncidence"),
                           label = "Sex",
                           choices = sexChoices,
                           selected = sexSelected,
                           multiple = sexMultiple)
        ),
        column(4,
               pickerInput(inputId = NS(id, "ageIncidence"),
                           label = "Age",
                           choices = ageChoices,
                           selected = ageSelected,
                           multiple = ageMultiple)
        ),
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "intervalIncidence"),
                           label = "Interval",
                           choices = unique(dataset$analysis_interval)),
        ),
        column(4,
               pickerInput(inputId = NS(id, "repeatedIncidence"),
                           label = "Repeated Events",
                           choices = unique(dataset$analysis_repeated_events)),
        )
      ),
      fluidRow(
        column(4,
               pickerInput(inputId = NS(id, "timeFromIncidence"),
                           label = "From",
                           choices = unique(dataset$incidence_start_date),
                           selected = min(unique(dataset$incidence_start_date)))
        ),
        column(4,
               pickerInput(inputId = NS(id, "timeToIncidence"),
                           label = "To",
                           choices = unique(dataset$incidence_start_date),
                           selected = max(unique(dataset$incidence_start_date)))
        )
      ),
      fluidRow(
        column(8,
               textAreaInput(inputId = NS(id, "captionInc"),
                             label = "Caption",
                             value = captionValue,
                             width = '100%',
                             height = "130px")
        ),
      ),
      fluidRow(
        column(4,
               actionButton(NS(id, lockVariable), "Add item to report")
        ),
        column(4,
               downloadButton(NS(id, "downloadFigure"), "Download Plot")
        ),
      ),
      tags$br(),
      fluidRow(
        column(8,
          plotOutput(NS(id, "previewFigure"))
        )
      )
    )

}

incidenceServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Figure 1
    incidenceCommonData <- reactive({
      incidence_estimates <- dataset()
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidence))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidence))
      # Database
      if (length(input$databaseIncidence) != 1 || input$databaseIncidence != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(cdm_name %in% c(input$databaseIncidence))
      }
      # Outcome
      if (length(input$outcomeIncidence) != 1 || input$outcomeIncidence != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(outcome_cohort_name == input$outcomeIncidence)
      }
      # Sex
      if (length(input$sexIncidence) != 1 || input$sexIncidence != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_sex %in% input$sexIncidence)
      }
      # Age group
      if (length(input$ageIncidence) != 1 || input$ageIncidence != "All") {
        incidence_estimates <- incidence_estimates %>%
          filter(denominator_age_group %in% input$ageIncidence)
      }
      # Start Time
      incidence_estimates <- incidence_estimates %>%
        filter(between(incidence_start_date,
                       as.Date(input$timeFromIncidence),
                       as.Date(input$timeToIncidence)))
      # Interval
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_interval == input$intervalIncidence)
      # Repeated events
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_repeated_events == input$repeatedIncidence)

      return(incidence_estimates)
    })


    if (id == "Plot - Incidence rate per year") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemType(input$facetIncidence)
        incidence_estimates <- incidenceCommonData()
        eval(parse(text = expression))
      })
    } else if (id == "Plot - Incidence rate per year by sex") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeSex(input$facetIncidence)
        incidence_estimates <- incidenceCommonData()
        eval(parse(text = expression))
      })

    } else if (id == "Plot - Incidence rate per year by age") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeAge(input$facetIncidence)
        incidence_estimates <- incidenceCommonData()
        eval(parse(text = expression))
      })
    }

  output$previewFigure <- renderPlot({
    req(previewFigure())
    previewFigure()
  })

  output$downloadFigure <- downloadHandler(
    filename = function() {
      paste(id, ".png", sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = previewFigure(), device = "png", height = 500, width = 845, units = "mm")
    }
  )

  addObject <- reactiveVal()
  observeEvent(input$lockDataIncidenceYear, {
    addObject(
      list(`Plot - Incidence rate per year` = list(incidence_estimates = incidenceCommonData(),
                                                   plotOption = input$facetIncidence,
                                                   caption = input$captionInc))
    )
  })

  observeEvent(input$lockDataIncidenceSex, {
    addObject(
      list(`Plot - Incidence rate per year by sex` = list(incidence_estimates = incidenceCommonData(),
                                                          plotOption = input$facetIncidence,
                                                          caption = input$captionInc))
    )
  })

  observeEvent(input$lockDataIncidenceAge, {
    addObject(
      list(`Plot - Incidence rate per year by age` = list(incidence_estimates = incidenceCommonData(),
                                                          plotOption = input$facetIncidence,
                                                          caption = input$captionInc))
    )
  })

    return(addObject)

  })
}
