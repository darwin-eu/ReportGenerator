test_that("Incidence module works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$IncidencePrevalence$incidence_estimates
    uploaded_files_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
      visOmopResults::filterSettings(result_type == "incidence_attrition")
    selection <- "Incidence"
    expect_s3_class(incidencePrevalenceUI(selection, uploaded_files, uploaded_files_attrition), "shiny.tag.list")
  })
})
