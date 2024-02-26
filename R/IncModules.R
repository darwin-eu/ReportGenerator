incidenceUI <- function(id, uploadedFiles) {
  if (id == "Plot - Incidence rate per year") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices

    sexChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex)
    sexSelected <-  databaseChoices[1]
    sexMultiple <- FALSE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    captionValue <-"Figure 1. Incidence rate/s of drug/s use over calendar time (per year) overall by database [Add months if relevant]"

    lockName <- "lockDataIncidenceYear"
  } else if (id == "Plot - Incidence rate per year by sex") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    sexChoices <- c("Male", "Female")
    sexSelected <- sexChoices
    sexMultiple <- TRUE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    captionValue <- "Figure 2. Incidence rate/s of drug/s use over calendar time (per year) stratified by sex and database [Add months if relevant]"

    lockName <- "lockDataIncidenceSex"
  } else if (id == "Plot - Incidence rate per year by age") {

    databaseChoices <- unique(uploadedFiles$dataIP$incidence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    sexChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_sex)
    sexSelected <-  sexChoices[1]
    sexMultiple <- FALSE

    ageChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_age_group)
    ageSelected <- ageChoices
    ageMultiple <- TRUE

    captionValue <- "Figure 3. Incidence rate/s of drug/s use over calendar time (per year) overall stratified by age and database [Add months if relevant]"

    lockName <- "lockDataIncidenceAge"
  }

  pickerOptions <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  washoutChoices <- unique(uploadedFiles$dataIP$incidence_estimates$analysis_outcome_washout)
  washoutSelected <- washoutChoices[1]
  daysPriorHistoryChoices <- unique(uploadedFiles$dataIP$incidence_estimates$denominator_days_prior_observation)
  daysPriorHistorySelected <- daysPriorHistoryChoices[1]
  outcomeChoices <- unique(uploadedFiles$dataIP$incidence_estimates$outcome_cohort_name)
  outcomeSelected <- outcomeChoices
  startDateChoices <- as.character(unique(uploadedFiles$dataIP$incidence_estimates$incidence_start_date))

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "facetIncidence"),
                         label = "Select plot type",
                         choices = c("Facet by outcome", "Facet by database"),
                         selected = "Facet by outcome",
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = NS(id, "ribbonIncidence"),
                         label = "Ribbon",
                         choices = c(TRUE, FALSE),
                         selected = TRUE,
                         multiple = FALSE)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "washoutIncidence"),
                         label = "Washout",
                         choices = washoutChoices,
                         selected = washoutSelected,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = NS(id, "daysPriorIncidence"),
                         label = "Days Prior History",
                         choices = daysPriorHistoryChoices,
                         selected = daysPriorHistorySelected,
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
                         options = pickerOptions)
      ),
      column(4,
             pickerInput(inputId = NS(id, "outcomeIncidence"),
                         label = "Outcome",
                         choices = outcomeChoices,
                         selected = outcomeSelected,
                         multiple = TRUE,
                         options = pickerOptions)
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
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_interval),
                         multiple = FALSE),
      ),
      column(4,
             pickerInput(inputId = NS(id, "repeatedIncidence"),
                         label = "Repeated Events",
                         choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_repeated_events),
                         multiple = FALSE),
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "timeFromIncidence"),
                         label = "From",
                         choices = startDateChoices,
                         selected = min(startDateChoices),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = NS(id, "timeToIncidence"),
                         label = "To",
                         choices = startDateChoices,
                         selected = max(startDateChoices),
                         multiple = FALSE)
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
             actionButton(NS(id, lockName), "Add item to report")
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

incidenceServer <- function(id, uploadedFiles) {
  moduleServer(id, function(input, output, session) {
    # Figure 1
    incidenceCommonData <- reactive({
      uploadedFiles <- uploadedFiles()
      incidence_estimates <- uploadedFiles$dataIP$incidence_estimates
      class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
      incidence_estimates[is.na(incidence_estimates)] = 0
      # Washout
      incidence_estimates <- incidence_estimates %>%
        filter(analysis_outcome_washout %in% c(input$washoutIncidence))
      # Days Prior
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_days_prior_observation %in% c(input$daysPriorIncidence))
      # Database
      incidence_estimates <- incidence_estimates %>%
        filter(cdm_name %in% c(input$databaseIncidence))
      # Outcome
      incidence_estimates <- incidence_estimates %>%
        filter(outcome_cohort_name == input$outcomeIncidence)
      # Sex
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_sex %in% input$sexIncidence)
      # Age group
      incidence_estimates <- incidence_estimates %>%
        filter(denominator_age_group %in% input$ageIncidence)

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
          addPreviewItemType(input$facetIncidence) %>%
          addPreviewItemRibbon(input$ribbonIncidence)
        incidence_estimates <- incidenceCommonData()
        eval(parse(text = expression))
      })
    } else if (id == "Plot - Incidence rate per year by sex") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeSex(input$facetIncidence) %>%
          addPreviewItemRibbon(input$ribbonIncidence)
        incidence_estimates <- incidenceCommonData()
        eval(parse(text = expression))
      })

    } else if (id == "Plot - Incidence rate per year by age") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeAge(input$facetIncidence) %>%
          addPreviewItemRibbon(input$ribbonIncidence)
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
                                                     caption = input$captionInc,
                                                     ribbon = input$ribbonIncidence))
      )
    })

    observeEvent(input$lockDataIncidenceSex, {
      addObject(
        list(`Plot - Incidence rate per year by sex` = list(incidence_estimates = incidenceCommonData(),
                                                            plotOption = input$facetIncidence,
                                                            caption = input$captionInc,
                                                            ribbon = input$ribbonIncidence))
      )
    })

    observeEvent(input$lockDataIncidenceAge, {
      addObject(
        list(`Plot - Incidence rate per year by age` = list(incidence_estimates = incidenceCommonData(),
                                                            plotOption = input$facetIncidence,
                                                            caption = input$captionInc,
                                                            ribbon = input$ribbonIncidence))
      )
    })
    return(addObject)
  })
}
