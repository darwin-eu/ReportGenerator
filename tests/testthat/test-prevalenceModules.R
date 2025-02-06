test_that("Prevalence module works", {
  testServer(reportGenerator(), {
    uploadedFiles <- testData$IncidencePrevalence$prevalence_estimates
    uploadedFilesAttrition <- testData$IncidencePrevalence$prevalence_estimates %>%
      visOmopResults::filterSettings(result_type == "prevalence_attrition")
    selection <- "Prevalence"
    expect_s3_class(prevalenceUI(selection, uploadedFiles, uploadedFilesAttrition), "shiny.tag.list")
  })
})
