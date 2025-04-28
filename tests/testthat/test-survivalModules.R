test_that("Survival Modules Single UI works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$CohortSurvival$single_event
    selection <- "Survival - Single Event"
    expect_s3_class(cohortSurvivalUI(selection, uploaded_files), "shiny.tag.list")
  })
})

test_that("Survival Modules Competing UI works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$CohortSurvival$competing_risk
    selection <- "Survival - Competing Risk"
    expect_s3_class(cohortSurvivalUI(selection, uploaded_files), "shiny.tag.list")
  })
})

test_that("tableSurvival works", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)[1]
  uploadedData <- joinDatabases(fileDataPath)
  uploaded_files <- list()
  uploaded_files$single_event <- uploadedData$summarised_result %>%
    visOmopResults::filterSettings(analysis_type == "single_event")

  expect_no_error(tableSurvival(uploaded_files$single_event))

  uploaded_files$competing_risk <- uploadedData$summarised_result %>%
    visOmopResults::filterSettings(analysis_type == "competing_risk")

  expect_no_error(tableSurvival(uploaded_files$competing_risk))

})
