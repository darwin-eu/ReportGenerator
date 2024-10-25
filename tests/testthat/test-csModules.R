test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("survivalTable", testData), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalTable", testData), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("survivalPlot", testData), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalPlot", testData), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failureTable", testData), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failureTable", testDat), "reactiveVal")
    expect_s3_class(cohortSurvivalUI("failurePlot", testData), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failurePlot", testData), "reactiveVal")
  })
})
