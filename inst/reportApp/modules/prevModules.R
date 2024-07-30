prevalenceUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "Prevalence per year - Plot") {

    # Database
    databaseChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)
    databaseSelected <- databaseChoices

    # Sex
    sexChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)
    sexSelected <-  sexChoices[1]
    sexMultiple <- FALSE

    # Age
    ageChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    # Caption
    captionValue <-"Figure 1. Prevalence of drug/s use over calendar time (per year) overall by database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceYear"
  } else if (id == "Prevalence per year by sex - Plot") {

    # Database
    databaseChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    # Sex
    sexChoices <- c("Male", "Female")
    sexSelected <- sexChoices
    sexMultiple <- TRUE

    # Age
    ageChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)
    ageSelected <- ageChoices[1]
    ageMultiple <- FALSE

    # Caption
    captionValue <- "Figure 2. Prevalence of drug/s use over calendar time (per year) stratified by sex and database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceSex"
  } else if (id == "Prevalence per year by age - Plot") {

    # Database
    databaseChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)
    databaseSelected <- databaseChoices[1]

    # Sex
    sexChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)
    sexSelected <-  sexChoices[1]
    sexMultiple <- FALSE

    # Age
    ageChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)
    ageSelected <- ageChoices
    ageMultiple <- TRUE

    # Caption
    captionValue <- "Figure 3. Prevalence of drug/s use over calendar time (per year) overall stratified by age and database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceAge"
  }

  pickerOptions <- list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
  outcomeChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)
  startDateChoices <- as.character(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date))
  dbPickerOptions <- list()
  if (length(databaseChoices) > 1) {
    dbPickerOptions <- pickerOptions
  }
  outcomePickerOptions <- list()
  if (length(outcomeChoices) > 1) {
    outcomePickerOptions <- pickerOptions
  }
  sexPickerOptions <- list()
  if (sexMultiple) {
    sexPickerOptions <- pickerOptions
  }
  agePickerOptions <- list()
  if (ageMultiple) {
    agePickerOptions <- pickerOptions
  }

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = ns("facetPrevalence"),
                         label = "Select plot type",
                         choices = c("Facet by outcome", "Facet by database"),
                         selected = "Facet by outcome",
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("databasePrevalence"),
                         label = "Database",
                         choices = databaseChoices,
                         selected = databaseSelected,
                         multiple = TRUE,
                         options = dbPickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("outcomePrevalence"),
                         label = "Outcome",
                         choices = outcomeChoices,
                         selected = outcomeChoices,
                         multiple = TRUE,
                         options = outcomePickerOptions)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("sexPrevalence"),
                         label = "Sex",
                         choices = sexChoices,
                         selected = sexSelected,
                         multiple = sexMultiple,
                         options = sexPickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("agePrevalence"),
                         label = "Age",
                         choices = ageChoices,
                         selected = ageSelected,
                         multiple = ageMultiple,
                         options = agePickerOptions)
      ),
      column(4,
             pickerInput(inputId = ns("typePrevalence"),
                         label = "Type",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_type),
                         multiple = FALSE),
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("intervalPrevalence"),
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_interval),
                         multiple = FALSE),
      ),
      column(4,
             pickerInput(inputId = ns("timeFromPrevalence"),
                         label = "From",
                         choices = startDateChoices,
                         selected = min(startDateChoices),
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("timeToPrevalence"),
                         label = "To",
                         choices = startDateChoices,
                         selected = max(startDateChoices),
                         multiple = FALSE)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = ns("ribbonPrevalence"),
                         label = "Ribbon",
                         choices = c(TRUE, FALSE),
                         selected = TRUE,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("showCIPrevalence"),
                         label = "Show confidence interval",
                         choices = c(TRUE, FALSE),
                         selected = TRUE,
                         multiple = FALSE)
      ),
      column(4,
             pickerInput(inputId = ns("stackPlotsPrevalence"),
                         label = "Stack plots",
                         choices = c(TRUE, FALSE),
                         selected = FALSE,
                         multiple = FALSE)
      )
    ),
    fluidRow(
      column(12,
             createCaptionInput(inputId = ns("captionInc"),
                                value = captionValue)
      ),
    ),
    fluidRow(
      createAddItemToReportUI(ns(lockVariable)),
      createDownloadPlotUI(ns)
    ),
    tags$br(),
    fluidRow(
      column(12,
             plotOutput(ns("previewFigure"))
      )
    )
  )
}

prevalenceServer <- function(id, uploadedFiles) {

  moduleServer(id, function(input, output, session) {
    # Figure 1
    prevalenceCommonData <- reactive({
      uploadedFiles <- uploadedFiles()
      prevalence_estimates <- uploadedFiles$dataIP$prevalence_estimates
      class(prevalence_estimates) <- c("IncidencePrevalenceResult",
                                       "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimate <- prevalence_estimates %>%
        mutate_if(is.numeric, list(~replace_na(., 0)))

      # Database
      prevalence_estimates <- prevalence_estimates %>%
        filter(cdm_name %in% c(input$databasePrevalence))
      # Outcome
      prevalence_estimates <- prevalence_estimates %>%
        filter(outcome_cohort_name %in% input$outcomePrevalence)
      # Sex
      prevalence_estimates <- prevalence_estimates %>%
        filter(denominator_sex %in% input$sexPrevalence)

      # Age group
      prevalence_estimates <- prevalence_estimates %>%
        filter(denominator_age_group %in% input$agePrevalence)

      # Start Time
      prevalence_estimates <- prevalence_estimates %>%
        filter(between(as.Date(prevalence_start_date),
                       as.Date(input$timeFromPrevalence),
                       as.Date(input$timeToPrevalence)))
      # Interval
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_interval == input$intervalPrevalence)
      # Repeated events
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_type == input$typePrevalence)

      prevalence_estimates <- prevalence_estimates %>%
        mutate_at(vars(prevalence,
                       prevalence_95CI_lower,
                       prevalence_95CI_upper), as.numeric)

      return(prevalence_estimates)
    })

    previewFigure <- reactive({
      expression <- getItemConfig(input = "title",
                                  output = "function",
                                  inputValue = id)
      if (id == "Prevalence per year - Plot") {
        expression <- expression %>%
          addPreviewItemType(input$facetPrevalence)
      } else if (id == "Prevalence per year by sex - Plot") {
        expression <- expression %>%
          addPreviewItemTypeSex(input$facetPrevalence)
      } else if (id == "Prevalence per year by age - Plot") {
        expression <- expression %>%
          addPreviewItemTypeAge(input$facetPrevalence)
      }
      expression <- expression %>%
        addPreviewItemRibbon(input$ribbonPrevalence) %>%
        addPlotOptions(input$showCIPrevalence, input$stackPlotsPrevalence)
      prevalence_estimates <- prevalenceCommonData()
      if (nrow(prevalence_estimates) > 0) {
        eval(parse(text = expression))
      }
    })

    output$previewFigure <- renderPlot({
      req(previewFigure())
      previewFigure()
    })

    output$downloadFigure <- downloadHandler(
      filename = function() {
        paste(id, ".png", sep = "")
      },
      content = function(file) {
        saveGGPlot(file = file,
                   plot = previewFigure(),
                   height = as.numeric(input$plotHeight),
                   width = as.numeric(input$plotWidth),
                   dpi = as.numeric(input$plotDpi))
      }
    )

    addObject <- reactiveVal()
    observeEvent(input$lockDataPrevalenceYear, {
      addObject(
        list(`Prevalence per year - Plot` = list(prevalence_estimates = prevalenceCommonData(),
                                                 plotOption = input$facetPrevalence,
                                                 caption = input$captionInc,
                                                 ribbon = input$ribbonPrevalence,
                                                 options = c(input$showCIPrevalence, input$stackPlotsPrevalence)))
      )
    })

    observeEvent(input$lockDataPrevalenceSex, {
      addObject(
        list(`Prevalence per year by sex - Plot` = list(prevalence_estimates = prevalenceCommonData(),
                                                        plotOption = input$facetPrevalence,
                                                        caption = input$captionInc,
                                                        ribbon = input$ribbonPrevalence,
                                                        options = c(input$showCIPrevalence, input$stackPlotsPrevalence)))
      )
    })

    observeEvent(input$lockDataPrevalenceAge, {
      addObject(
        list(`Prevalence per year by age - Plot` = list(prevalence_estimates = prevalenceCommonData(),
                                                        plotOption = input$facetPrevalence,
                                                        caption = input$captionInc,
                                                        ribbon = input$ribbonPrevalence,
                                                        options = c(input$showCIPrevalence, input$stackPlotsPrevalence)))
      )
    })
    return(addObject)
  })
}
