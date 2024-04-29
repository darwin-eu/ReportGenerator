test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("survivalTable", testData$`Survival estimate`), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalTable", testData$`Survival estimate`), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("survivalPlot", testData$`Survival estimate`), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalPlot", testData$`Survival estimate`), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failureTable", testData$`Survival estimate`), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failureTable", testData$`Survival estimate`), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failurePlot", testData$`Survival estimate`), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failurePlot", testData$`Survival estimate`), "reactiveVal")
  })
})
