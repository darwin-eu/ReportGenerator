prevalenceUI <- function(id, uploadedFiles) {
  if (id == "Plot - Prevalence per year") {

    # Database
    databaseChoices <- c("All", unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name))
    databaseSelected <- ("All")
    databaseOptions <- list()

    # Sex
    sexChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)
    sexSelected <-  unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)[1]
    sexMultiple <- FALSE

    # Age
    ageChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)
    ageSelected <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)[1]
    ageMultiple <- FALSE

    # Caption
    captionValue <-"Figure 1. Prevalence of drug/s use over calendar time (per year) overall by database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceYear"

  } else if (id == "Plot - Prevalence per year by sex") {

    # Database
    databaseChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)
    databaseSelected <- uploadedFiles$dataIP$prevalence_estimates$cdm_name[1]
    databaseOptions <- list(maxOptions = 1)

    # Sex
    sexChoices <- c("Male", "Female")
    sexSelected <- c("Male", "Female")
    sexMultiple <- TRUE

    # Age
    ageChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)
    ageSelected <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group)[1]
    ageMultiple <- FALSE

    # Caption
    captionValue <- "Figure 2. Prevalence of drug/s use over calendar time (per year) stratified by sex and database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceSex"

  } else if (id == "Plot - Prevalence per year by age") {

    # Database
    databaseChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$cdm_name)
    databaseSelected <- uploadedFiles$dataIP$prevalence_estimates$cdm_name[1]
    databaseOptions <- list(maxOptions = 1)

    # Sex
    sexChoices <- unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)
    sexSelected <-  unique(uploadedFiles$dataIP$prevalence_estimates$denominator_sex)[1]
    sexMultiple <- FALSE

    # Age
    ageChoices <- c("All", unique(uploadedFiles$dataIP$prevalence_estimates$denominator_age_group))
    ageSelected <- "All"
    ageMultiple <- TRUE

    # Caption
    captionValue <- "Figure 3. Prevalence of drug/s use over calendar time (per year) overall stratified by age and database [Add months if relevant]"

    # Lock
    lockVariable <- "lockDataPrevalenceAge"

  }

  tagList(
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "facetPrevalence"),
                         label = "Select plot type",
                         choices = c("Facet by outcome", "Facet by database"),
                         selected = "Facet by outcome")
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "databasePrevalence"),
                         label = "Database",
                         choices = databaseChoices,
                         selected = databaseSelected,
                         multiple = TRUE,
                         options = databaseOptions)
      ),
      column(4,
             pickerInput(inputId = NS(id, "outcomePrevalence"),
                         label = "Outcome",
                         choices = c("All", unique(uploadedFiles$dataIP$prevalence_estimates$outcome_cohort_name)),
                         selected = "All",
                         multiple = TRUE)
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "sexPrevalence"),
                         label = "Sex",
                         choices = sexChoices,
                         selected = sexSelected,
                         multiple = sexMultiple)
      ),
      column(4,
             pickerInput(inputId = NS(id, "agePrevalence"),
                         label = "Age",
                         choices = ageChoices,
                         selected = ageSelected,
                         multiple = ageMultiple)
      ),
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "intervalPrevalence"),
                         label = "Interval",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_interval)),
      ),
      column(4,
             pickerInput(inputId = NS(id, "typePrevalence"),
                         label = "Type",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$analysis_type)),
      )
    ),
    fluidRow(
      column(4,
             pickerInput(inputId = NS(id, "timeFromPrevalence"),
                         label = "From",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = min(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
      ),
      column(4,
             pickerInput(inputId = NS(id, "timeToPrevalence"),
                         label = "To",
                         choices = unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date),
                         selected = max(unique(uploadedFiles$dataIP$prevalence_estimates$prevalence_start_date)))
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

prevalenceServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    # Figure 1
    prevalenceCommonData <- reactive({
      prevalence_estimates <- dataset()
      class(prevalence_estimates) <- c("IncidencePrevalenceResult",
                                       "PrevalenceResult", "tbl_df", "tbl", "data.frame")
      prevalence_estimates[is.na(prevalence_estimates)] = 0

      # Database
      if (length(input$databasePrevalence) != 1 || input$databasePrevalence != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(cdm_name %in% c(input$databasePrevalence))
      }
      # Outcome
      if (length(input$outcomePrevalence) != 1 || input$outcomePrevalence != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(outcome_cohort_name == input$outcomePrevalence)
      }
      # Sex
      if (length(input$sexPrevalence) != 1 || input$sexPrevalence != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_sex %in% input$sexPrevalence)
      }
      # Age group
      if (length(input$agePrevalence) != 1 || input$agePrevalence != "All") {
        prevalence_estimates <- prevalence_estimates %>%
          filter(denominator_age_group %in% input$agePrevalence)
      }
      # Start Time
      prevalence_estimates <- prevalence_estimates %>%
        filter(between(prevalence_start_date,
                       as.Date(input$timeFromPrevalence),
                       as.Date(input$timeToPrevalence)))
      # Interval
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_interval == input$intervalPrevalence)
      # Repeated events
      prevalence_estimates <- prevalence_estimates %>%
        filter(analysis_type == input$typePrevalence)

      return(prevalence_estimates)
    })


    if (id == "Plot - Prevalence per year") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemType(input$facetPrevalence)
        prevalence_estimates <- prevalenceCommonData()
        eval(parse(text = expression))
      })
    } else if (id == "Plot - Prevalence per year by sex") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeSex(input$facetPrevalence)
        prevalence_estimates <- prevalenceCommonData()
        eval(parse(text = expression))
      })

    } else if (id == "Plot - Prevalence per year by age") {
      previewFigure <- reactive({
        expression <- getItemConfig(input = "title",
                                    output = "function",
                                    inputValue = id) %>%
          addPreviewItemTypeAge(input$facetPrevalence)
        prevalence_estimates <- prevalenceCommonData()
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
    observeEvent(input$lockDataPrevalenceYear, {
      addObject(
        list(`Plot - Prevalence per year` = list(prevalence_estimates = prevalenceCommonData(),
                                                     plotOption = input$facetPrevalence,
                                                     caption = input$captionInc))
      )
    })

    observeEvent(input$lockDataPrevalenceSex, {
      addObject(
        list(`Plot - Prevalence per year by sex` = list(prevalence_estimates = prevalenceCommonData(),
                                                            plotOption = input$facetPrevalence,
                                                            caption = input$captionInc))
      )
    })

    observeEvent(input$lockDataPrevalenceAge, {
      addObject(
        list(`Plot - Prevalence per year by age` = list(prevalence_estimates = prevalenceCommonData(),
                                                            plotOption = input$facetPrevalence,
                                                            caption = input$captionInc))
      )
    })

    return(addObject)

  })
}
