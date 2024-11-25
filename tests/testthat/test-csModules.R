test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("survivalTable", testData$CohortSurvival$single_event), "shiny.tag.list")
    expect_s3_class(cohortSurvivalUI("failureTable", testData$CohortSurvival$competing_risk), "shiny.tag.list")
  })
})
