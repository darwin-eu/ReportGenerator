test_that("Incidence module works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$IncidencePrevalence$incidence_estimates
    uploadedFilesAttrition <- testData$IncidencePrevalence$incidence_estimates %>%
      visOmopResults::filterSettings(result_type == "incidence_attrition")
    selection <- "Incidence"
    expect_s3_class(incidenceUI(selection, uploadedFiles, uploadedFilesAttrition), "shiny.tag.list")
  })
})
