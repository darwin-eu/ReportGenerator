test_that("Prevalence module works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$IncidencePrevalence$prevalence_estimates
    uploaded_files_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
      visOmopResults::filterSettings(result_type == "prevalence_attrition")
    selection <- "Prevalence"
    expect_s3_class(prevalenceUI(selection, uploaded_files, uploaded_files_attrition), "shiny.tag.list")
  })
})
