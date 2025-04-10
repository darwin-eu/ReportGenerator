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

test_that("tableSurvival style works", {

  result_single_event <- testData$CohortSurvival$single_event


  # header_style_list <- list(gt::cell_fill(color = "#C8C8C8"),
  #                           gt::cell_text(align = "center",
  #                                         weight = "bold"))
  #
  # styleName <- list("header" = header_style_list, "body" = list())

  # visOmopResults:::gtStyleInternal(styleName = styleName)


  CohortSurvival::tableSurvival(x = result_single_event,
                                .options = list(style = getDarwinStyle(),
                                                caption = NULL))

  debugonce(visOmopResults:::validateStyle)
  debugonce(visOmopResults:::gtStyleInternal)

  tableSurvival(x = result_single_event)

  expect_no_error()

})
