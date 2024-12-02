test_that("Prevalence module works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$IncidencePrevalence$prevalence_estimates
    selection <- "Prevalence"
    expect_s3_class(prevalenceUI(selection, uploadedFiles), "shiny.tag.list")
  })
})
