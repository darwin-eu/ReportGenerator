test_that("Survival Modules Single UI works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$CohortSurvival$single_event
    selection <- "Survival - Single Event"
    expect_s3_class(cohortSurvivalUI(selection, uploadedFiles), "shiny.tag.list")
  })
})

test_that("Survival Modules Competing UI works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$CohortSurvival$competing_risk
    selection <- "Survival - Competing Risk"
    expect_s3_class(cohortSurvivalUI(selection, uploadedFiles), "shiny.tag.list")
  })
})
