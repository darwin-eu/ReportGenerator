test_that("tableIncidenceStyle", {

  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_incidence_table_plot.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  incidence_data <- dataReportList$nqhz$incidence_table$result

  # library(IncidencePrevalence)
  #
  # sampleSize <- 1000
  #
  # cdm <- IncidencePrevalence::mockIncidencePrevalence(sampleSize = sampleSize,
  #                                                     outPre = 0.3,
  #                                                     minOutcomeDays = 365,
  #                                                     maxOutcomeDays = 3650)
  #
  # # Denominator data
  # cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  #   cdm = cdm,
  #   name = "denominator",
  #   cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01")),
  #   ageGroup = list(
  #     c(0, 64),
  #     c(65, 100)
  #   ),
  #   sex = c("Male", "Female", "Both"),
  #   daysPriorObservation = 180
  # )
  #
  # # Incidence data
  # incidence_data <- IncidencePrevalence::estimateIncidence(
  #   cdm = cdm,
  #   denominatorTable = "denominator",
  #   outcomeTable = "outcome",
  #   interval = "years",
  #   repeatedEvents = TRUE,
  #   outcomeWashout = 180,
  #   completeDatabaseIntervals = TRUE
  # )


  tableIncidence(incidence_data,
                  .options = list(style = list("header" = list(gt::cell_text(weight = "bold"),
                                                               gt::cell_fill(color = "blue"))),
                                  caption = "Figure 1. XXXXX")
                  )

})

test_that("Incidence module works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$IncidencePrevalence$incidence_estimates
    uploaded_files_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
      visOmopResults::filterSettings(result_type == "incidence_attrition")
    selection <- "Incidence"
    expect_s3_class(incidencePrevalenceUI(selection, uploaded_files, uploaded_files_attrition), "shiny.tag.list")
  })
})
