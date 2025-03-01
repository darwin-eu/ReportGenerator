test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("single_event", testData$CohortSurvival$single_event), "shiny.tag.list")
    expect_s3_class(cohortSurvivalUI("competing_risk", testData$CohortSurvival$competing_risk), "shiny.tag.list")
  })
})
