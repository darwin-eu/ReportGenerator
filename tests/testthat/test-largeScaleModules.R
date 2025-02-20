test_that("Large Scale Characteristics module works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$CohortCharacteristics$summarise_large_scale_characteristics
    selection <- "Summarised Large Scale Characteristics"
    expect_s3_class(largeScaleUI(selection, uploaded_files), "shiny.tag.list")
  })
})
