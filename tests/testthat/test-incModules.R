test_that("Incidence module works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$IncidencePrevalence$incidence_estimates
    selection <- "Incidence"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
  })
})
