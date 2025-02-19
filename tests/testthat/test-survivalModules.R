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
