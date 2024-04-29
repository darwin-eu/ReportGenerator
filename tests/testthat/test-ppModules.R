test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics",
                                      testData$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          testData$summarised_characteristics), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      testData$summarised_large_scale_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          testData$summarised_large_scale_characteristics), "reactiveVal")
  })
})
