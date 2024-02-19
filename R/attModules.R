attritionUI <- function(id, uploadedFiles) {
  ns <- NS(id)
  if (id == "Table - Number of participants") {
    lockName <- "lockTableNumPar"
    tagList(
      fluidRow(
        column(4,
               selectInput(inputId = ns("analysisIdTable1"),
                           label = "Analysis ID",
                           choices = unique(uploadedFiles$dataIP$incidence_attrition$analysis_id))
        ),
        column(8,
               textAreaInput(ns("captionTableAtt"),
                             "Caption",
                             table1aAutText(uploadedFiles$dataIP$incidence_attrition,
                                            uploadedFiles$dataIP$prevalence_attrition),
                             width = '100%',
                             height = "130px")
        )
      ),
      fluidRow(
        column(4,
               actionButton(ns(lockName), "Add item to report")
        )
      ),
      tags$br(),
      fluidRow(
        column(12,
               tableOutput(ns("previewTableAtt"))
               )
        )
      )
  } else if (id == "Table - Incidence Attrition") {
    lockName <- "lockTableIncAtt"
      tagList(
        fluidRow(
          column(4,
                 selectInput(inputId = ns("analysisIdTable1"),
                             label = "Analysis ID",
                             choices = unique(uploadedFiles$dataIP$incidence_attrition$analysis_id))
          ),
          column(8,
                 textAreaInput(ns("captionTableInc"),
                               "Caption",
                               tableAttrition(uploadedFiles$dataIP$incidence_attrition),
                               width = '100%',
                               height = "130px")
          )
        ),
        fluidRow(
          column(4,
                 actionButton(ns(lockName), "Add item to report")
          )
        ),
        tags$br(),
        fluidRow(
          column(12,
                 tableOutput(ns("previewTableAttInc"))
          )
        )
      )
    } else if (id == "Table - Prevalence Attrition") {
    lockName <- "lockTablePrevAtt"
      tagList(
        fluidRow(
          column(4,
                 selectInput(inputId = ns("analysisIdTable1"),
                             label = "Analysis ID",
                             choices = unique(uploadedFiles$dataIP$prevalence_attrition$analysis_id))
          ),
          column(8,
                 textAreaInput(ns("captionTablePrev"),
                               "Caption",
                               tableAttrition(uploadedFiles$dataIP$prevalence_attrition),
                               width = '100%',
                               height = "130px")
          )
        ),
        fluidRow(
          column(4,
                 actionButton(ns(lockName), "Add item to report")
          )
        ),
        tags$br(),
        fluidRow(
          column(12,
                 tableOutput(ns("previewTableAttPrev"))
          )
        )
      )
    }
}

  #
  # tableSexFilters <- function(uploadedFiles) {
  #   tagList(
  #     fluidRow(
  #       column(8,
  #              textAreaInput("captionTableSexAge",
  #                            "Caption",
  #                            "Table 1. Displays the total number of drug users, for each of the databases, during the study period. Total number of drug A users ranged from XXX to XXX across databases. Total number of drug B users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by sex, number of male drug A users ranged from XXX to XXX  across databases, whereas number of female drug A users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by age, number of drug A users in age group 1 ranged from XXX to XXX across databases, whereas number of drug A users in age group 2 ranged from XXX to XXX across databases. [continue for each age group and each drug ]. In summary, there were more users of [insert drug with highest count]; and least users of [insert drug with lowest count]. [Describe other observed patterns in the data].",
  #                            width = '100%',
  #                            height = "130px")
  #       )
  #     ),
  #     fluidRow(
  #       column(4,
  #              actionButton("lockTableSex", "Add item to report")
  #              # lockItemsUI("lockTableSex")
  #       ),
  #     )
  #   )
  # }


attritionServer <- function(id, uploadedFiles) {
  moduleServer(id, function(input, output, session) {

      prevalenceAttritionCommon <- reactive({
        uploadedFiles <- uploadedFiles()
        if (!is.null(uploadedFiles$dataIP$prevalence_attrition)) {
          commonData <- uploadedFiles$dataIP$prevalence_attrition
          if (class(commonData$excluded_records) == "character") {
            commonData$excluded_records <- as.numeric(commonData$excluded_records)
          }
          if (class(commonData$excluded_subjects) == "character") {
            commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
          }
          commonData[is.na(commonData)] = 0
          commonData <- commonData %>%
            filter(analysis_id %in% c(input$analysisIdTable1))
          commonData
        } else {
          NULL
        }
      })

      # incidence_attrition

      incidenceAttritionCommon <- reactive({
        uploadedFiles <- uploadedFiles()
        if (!is.null(uploadedFiles$dataIP$incidence_attrition)) {
          commonData <- uploadedFiles$dataIP$incidence_attrition
          if (class(commonData$excluded_records) == "character") {
            commonData$excluded_records <- as.numeric(commonData$excluded_records)
          }
          if (class(commonData$excluded_subjects) == "character") {
            commonData$excluded_subjects <- as.numeric(commonData$excluded_subjects)
          }
          commonData[is.na(commonData)] = 0
          commonData <- commonData %>%
            filter(analysis_id %in% c(input$analysisIdTable1))
          commonData
        } else {
          NULL
        }
      })

      addObject <- reactiveVal()


      if (id == "Table - Number of participants") {

        output$previewTableAtt <- renderTable({
          prevalence_attrition <- prevalenceAttritionCommon()
          incidence_attrition <- incidenceAttritionCommon()
          eval(parse(text = getItemConfig(input = "title",
                                          output = "function",
                                          inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTableNumPar, {
          addObject(
            list(`Table - Number of participants` = list(prevalence_attrition = prevalenceAttritionCommon(),
                                                         incidence_attrition = incidenceAttritionCommon(),
                                                         caption = input$captionTableAtt))
          )
        })

      } else if (id == "Table - Incidence Attrition") {

        attritionDataType <- "incidence"

        output$previewTableAttInc <- renderTable({
          incidence_attrition <- incidenceAttritionCommon()
          incidence_attrition <- incidenceAttritionCommon()
          eval(parse(text = getItemConfig(input = "title",
                                          output = "function",
                                          inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTableIncAtt, {
          addObject(
            list(`Table - Incidence Attrition` = list(incidence_attrition = incidenceAttritionCommon(),
                                                      attritionDataType = attritionDataType,
                                                      caption = input$captionTableInc))
          )
        })

      } else if (id == "Table - Prevalence Attrition") {

        attritionDataType <- "prevalence"

        output$previewTableAttPrev <- renderTable({
            prevalence_attrition <- prevalenceAttritionCommon()
            eval(parse(text = getItemConfig(input = "title",
                                            output = "function",
                                            inputValue = id)))
        }, colnames = FALSE)

        observeEvent(input$lockTablePrevAtt, {
          addObject(
            list(`Table - Prevalence Attrition` = list(prevalence_attrition = prevalenceAttritionCommon(),
                                                       attritionDataType = attritionDataType,
                                                       caption = input$captionTablePrev))
            )
          })
      }

      return(addObject)
  })
}


